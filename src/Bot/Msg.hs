{-# LANGUAGE TemplateHaskell #-}
{-| module: Bot.Msg

Defines the internal message\/event structure, and implements parsing\/serialising.

Server events are either messages that the bot has received from the server, like
a PRIVMSG, or other events like the bot starting up. They're parsed from the
textual line received by a parser using @Parsec@, and for common commands with
specific syntax, they're parsed in a detailed way.

Client messages are messages that will be sent to the server by the bot, usually
within a callback. Client messages can be sent generically, as a stock 'CMsg'.
-}

module Bot.Msg (
    -- * Message structure
    -- ** Generic
    User(..), Channel(..), Server(..), 
    SenderC, RecipientC,
    fromS, fromR, toR,
    makeUser, makeUserH,
    makeChannel, channelToStr,
    -- ** Event
    SEvent(..), Sender(..), Recipient(..),
    -- ** Client message
    CMsg(..), ClientCmd(..),

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
import Bot.Msg.Splices


-- | A user with a nickname, and maybe user\/host\/other things. This
-- is multipurpose, used just to indicate that some nick is a user 
-- (when used with 'makeUser'), or, e.g., as a result from a NAMES or
-- WHOIS request containing more info about the user.
data User = User { nick :: String -- ^ Nick
                 , user :: Maybe String -- ^ User, if we know it.
                 , host :: Maybe String -- ^ Host, if we know it.
                 , statusCharL :: [(Channel, Char)] -- ^ A list of status prefixes that the user has in each channel that we know (e.g., \@ for op). 
                 } deriving (Eq, Read, Show)
-- | Make a user struct from just a nick.
makeUser :: String -> User
makeUser s = User s Nothing Nothing []

-- | Make a user struct from a nick, user string and host string.
makeUserH :: String -> String -> String -> User
makeUserH s u h = User s (Just u) (Just h) []

-- | A channel; the string doesn't include the '#' prefix.
newtype Channel = Channel String deriving (Eq, Read, Show)
-- | Take a channel name of the form "#chan" and create a 'Channel'.
makeChannel (x:xs) = Channel xs
-- | Turn a 'Channel' into a channel name of the form "#chan".
channelToStr (Channel c) = "#"++c

-- | A server and its hostname.
newtype Server = Server String deriving (Eq, Read, Show)

-- | Recipient class: 'User' and 'Channel' can both be recipients
-- in a message (see 'Recipient').
class RecipientC a where
    -- | 'Recipient' to 'User' or 'Channel'
    fromR :: Recipient -> a
    -- | 'User' or 'Channel' to 'Recipient'
    toR :: a -> Recipient
instance RecipientC User where
    fromR (RUser n) = makeUser n
    toR u = RUser (nick u)
instance RecipientC Channel where
    fromR (RChannel c) = Channel c
    toR (Channel c) = RChannel c
instance RecipientC Recipient where
    fromR = id
    toR = id

-- | Sender class: 'User' and 'Server' can both be senders
-- in a message (see 'Sender').
class SenderC a where
    -- | 'Sender' to 'User' or 'Server'
    fromS :: Sender -> a
instance SenderC User where
    fromS (SUser n u h) = makeUserH n u h
instance SenderC Server where
    fromS (SServer h) = Server h


-- | Commands that will be sent by the bot; deliberately named
-- identically to their textual representations, to make serialising easy.
data ClientCmd = NICK | USER | JOIN | PING | PONG | PRIVMSG | NAMES | WHOIS | WHOWAS | KICK | MODE
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
    | SPong { srv :: String, srv2 :: String }
    | SNotice { from :: Sender, to :: Recipient, text :: String }
    | SPrivmsg { from :: Sender, to :: Recipient, text :: String }
    | SJoin { from :: Sender, to :: Recipient }
    | SPart { from :: Sender, to :: Recipient }
    | SKick { from :: Sender, to :: Recipient, target :: String, reason :: String }
    | SMode { from :: Sender, modeTarget :: Recipient, modeChanges :: String }
    | SNumeric { fromMaybe :: Maybe Sender, n :: Int, args :: [String] }
    | Startup
    | Connected
    | Disconnected
    | Ready
    | UnReady
    deriving (Show, Read, Eq)

-- | Sender field - can be either user or server.
data Sender = SUser String String String -- ^ nick, user, host
            | SServer String -- ^ host
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
                   "PING"    -> $(makeNMsg 1) SPing args
                   "PONG"    -> $(makeNMsg 2) SPong args
                   "JOIN"    -> $(makeSTMsg 0) SJoin src args
                   "PART"    -> $(makeSTMsg 0) SPart src args
                   "MODE"    -> $(makeSTMsg 1) SMode src args
                   "NOTICE"  -> $(makeSTMsg 1) SNotice src args
                   "PRIVMSG" -> $(makeSTMsg 1) SPrivmsg src args
                   "KICK"    -> $(makeSTMsg 2) SKick src args
                   _ -> case (readMaybe cmd :: Maybe Int) of
                            Just n -> return . SNumeric src n $ args
                            Nothing -> parserFail $ "Unexpected command " ++ cmd


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
