{-| module: Msg

Defines the internal message/event structure, and implements parsing/serialising.

Server events are either messages that the bot has received from the server, like
a PRIVMSG, or other events like the bot starting up. They're parsed from the
textual line received by a parser using @Parsec@, and for common commands with
specific syntax, they're parsed in a detailed way.

Client messages are messages that will be sent to the server by the bot, usually
within a callback. Client messages can be sent generically, as a stock 'CMsg'.
-}

module Msg (
    -- * Message structure
    -- ** Event
    SEvent(..), Sender(..), Recipient(..),
    -- ** Client message
    CMsg(..), ClientCmd(..),
    -- * Sending messages

    -- * Parsing and serialising
    joinMsg, readMsg,
    -- * Parsing internals
    parseMsg, parseSender, parseRecipient, parseArgs,
    parseUntil, parseWord,
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (parserFail)
import Text.Read (readMaybe)
import Data.List (intercalate)

-- | Commands that will be sent by the bot; deliberately named
-- identically to their textual representations, to make serialising easy.
data ClientCmd = NICK | USER | JOIN | PONG | PRIVMSG
    deriving Show

-- | Message that will be sent by the bot. The structure is fairly barebones
-- because it's not going to be parsed; whatever is sending the message can
-- implement the structure correctly itself, or a helper function in 'Bot'
-- can handle that.
data CMsg
    = CMsg { command :: ClientCmd, argsC :: [String] }
    deriving Show

-- | Events: a message that's been sent by the server and received by the
-- client, or an event initiated on the bot side like startup. The messages have
-- detailed structures; they're parsed into easily usable forms for callbacks.
-- The exception is 'SNumeric', which is generic for all numeric messages and
-- may have any number of arguments.
data SEvent
    = SPing { srv :: String }
    | SNotice { from :: Sender, to :: Recipient, text :: String }
    | SPrivmsg { from :: Sender, to :: Recipient, text :: String }
    | SNumeric { fromMaybe :: Maybe Sender, n :: Int, args :: [String] }
    | Startup
    | Connected
    deriving (Show, Read, Eq)

-- | Sender field - can be either user or server.
data Sender = SUser { nick :: String, user :: String, host :: String }
            | SServer { host :: String }
    deriving  (Show, Read, Eq)

-- | Recipient field - can either be user or channel.
data Recipient = RUser String | RChannel String
    deriving (Show, Read, Eq)

-- | Convert a message to a string in the IRC format.
joinMsg :: CMsg -> String
joinMsg (CMsg cmd args) = show cmd ++ end ++ "\r\n"
    where end = if length args > 1
                  then " " ++ (intercalate " " . init $ args) ++ " :" ++ last args
                  else if length args == 1 then " :" ++ last args
                  else ""

-- | Parse a message string in the IRC format.
readMsg :: String -> Either ParseError SEvent
readMsg = parse parseMsg ""

-- | The main parser for messages.
parseMsg :: Parser SEvent
parseMsg = do src <- optionMaybe $ parseSender
              cmd <- parseWord
              args <- parseArgs
              case cmd of
                   "PING" -> if length args > 0 then return . SPing . head $ args
                                                else parserFail $ "PING had no arguments"
                   "NOTICE" -> makeTextMsg SNotice src args
                   "PRIVMSG" -> makeTextMsg SPrivmsg src args
                   _ -> case (readMaybe cmd :: Maybe Int) of
                            Just n -> return . SNumeric src n $ args
                            Nothing -> parserFail $ "Unexpected command " ++ cmd
    where makeTextMsg con s a = case s of
            Nothing -> parserFail "No source for textual message"
            Just sender -> if length a /= 2 then parserFail "Too many arguments to textual message"
                            else case parse parseRecipient "" (a!!0) of
                                    Left err -> parserFail . show $ err
                                    Right recipient -> return $ con sender recipient (a!!1)

-- | Parse until one of the characters in @s@, or @eof@.
parseUntil :: String -> Parser String
parseUntil s = many1 $ do notFollowedBy eof
                          noneOf s

-- | Parse until one of @space@ @:@ @\r@ @\n@ @eof@, and eat a trailing space
-- if it's there.
parseWord :: Parser String
parseWord = do str <- parseUntil " :\r\n"
               optionMaybe . char $ ' '
               return str

-- | Parse the sender field of a message, which will be a @:@, followed by
-- either a user in the form @nick!user\@host@, or a server in the form @host@.
parseSender :: Parser Sender
parseSender = char ':' >> (user <|> host)
    where user = try $ do nick <- parseUntil "!"
                          char '!'
                          user <- parseUntil "@"
                          char '@'
                          host <- parseWord
                          return $ SUser nick user host
          host = parseWord >>= return . SServer

-- | Parse a recipient field, which might either be a channel in the form
-- @#channel-name@, or a user in the form @nick@.
parseRecipient :: Parser Recipient
parseRecipient = channel <|> user
    where channel = char '#' >> parseUntil "" >>= return . RChannel
          user = parseUntil "" >>= return . RUser

-- | Parse the arguments at the end of the message.
parseArgs :: Parser [String]
parseArgs = do args <- many parseWord
               lastArg <- optionMaybe $ do char ':'
                                           many . noneOf $ "\r\n"
               manyTill (oneOf "\r\n") eof
               return $ case lastArg of Just a  -> args ++ [a]
                                        Nothing -> args
