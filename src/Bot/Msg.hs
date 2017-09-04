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
    parseUntil, parseWord, parseMode,
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (decimal)
import Text.Parsec.Prim (parserFail)
import Data.List.Split (chunksOf)
import Bot.Msg.Splices


-- | A user with a nickname, and maybe user\/host\/other things. This
-- is multipurpose, used just to indicate that some nick is a user
-- (when used with 'makeUser'), or, e.g., as a result from a NAMES or
-- WHOIS request containing more info about the user.
data User = User { nick :: String -- ^ Nick
                 , user :: Maybe String -- ^ User, if we know it.
                 , host :: Maybe String -- ^ Host, if we know it.
                 , statusCharL :: [(Channel, Char)] -- ^ A list of status prefixes that the user has in each channel that we know (e.g., \@ for op). A space here means no status.
                 } deriving (Eq, Read, Show, Ord)
-- | Make a user struct from just a nick.
makeUser :: String -> User
makeUser s = User s Nothing Nothing []

-- | Make a user struct from a nick, user string and host string.
makeUserH :: String -> String -> String -> User
makeUserH s u h = User s (Just u) (Just h) []

-- | A channel; the string doesn't include the '#' prefix.
newtype Channel = Channel String deriving (Eq, Read, Show, Ord)
-- | Take a channel name of the form "#chan" and create a 'Channel'.
makeChannel ('#':xs) = Channel xs
-- | Turn a 'Channel' into a channel name of the form "#chan".
channelToStr (Channel c) = "#"++c

-- | A server and its hostname.
newtype Server = Server String deriving (Eq, Read, Show, Ord)

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
    | SQuit { from :: Sender, quitReason :: String }
    | SNick { from :: Sender, newNick :: String }
    | SKick { from :: Sender, to :: Recipient, target :: String, reason :: String }
    | SMode { from :: Sender, modeTarget :: Recipient, modeChanges :: [(Bool, Char, [String])] }
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
    where end
            | length args  > 1 =
              " " ++ (unwords . init $ args) ++ " :" ++ last args
            | length args == 1 = " :" ++ last args
            | otherwise = ""

-- | Parse a message string in the IRC format.
readMsg :: String -> Either ParseError SEvent
readMsg = parse parseMsg ""

-- | The main parser for messages.
parseMsg :: Parser SEvent
parseMsg = do src <- optionMaybe parseSender
              cmd <- parseWord
              args <- parseArgs
              case cmd of
                   "PING"    -> $(makeNMsg 1) SPing args
                   "PONG"    -> $(makeNMsg 2) SPong args
                   "NICK"    -> $(makeSMsg 1) SNick src args
                   "QUIT"    -> $(makeSMsg 1) SQuit src args
                   "JOIN"    -> $(makeSTMsg 0) SJoin src args
                   "PART"    -> $(makeSTMsg 0) SPart src args
                   "NOTICE"  -> $(makeSTMsg 1) SNotice src args
                   "PRIVMSG" -> $(makeSTMsg 1) SPrivmsg src args
                   "KICK"    -> $(makeSTMsg 2) SKick src args
                   "MODE"    -> parseMode src args
                   _ -> case parse decimal "" cmd of
                          Right n -> return . SNumeric src n $ args
                          Left _ -> parserFail $ "Unexpected command " ++ cmd


-- | Parse until one of the characters in @s@, or @eof@.
parseUntil :: String -> Parser String
parseUntil s = many1 $ do notFollowedBy eof
                          noneOf s

-- | Parse until one of @space@ @:@ @\r@ @\n@ @eof@, and eat a trailing space
-- if it's there.
parseWord' :: Parser String
parseWord' = do str <- parseUntil " :\r\n"
                optionMaybe . char $ ' '
                return str

-- | Parse until one of @space@ @\r@ @\n@ @eof@, and eat a trailing space
-- if it's there.
parseWord :: Parser String
parseWord = do str <- parseUntil " \r\n"
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
          host = SServer <$> parseWord

-- | Parse a recipient field, which might either be a channel in the form
-- @#channel-name@, or a user in the form @nick@.
parseRecipient :: Parser Recipient
parseRecipient = channel <|> user
    where channel = RChannel <$> (char '#' >> parseUntil "")
          user = RUser <$> parseUntil ""

-- | Parse the arguments at the end of the message.
parseArgs :: Parser [String]
parseArgs = do args <- many parseWord'
               lastArg <- optionMaybe $ do char ':'
                                           many . noneOf $ "\r\n"
               manyTill (oneOf "\r\n") eof
               return $ case lastArg of Just a  -> args ++ [a]
                                        Nothing -> args

-- | Unwrap a parser parsing a mode change argument of the form @[+|-]{char}@.
readModeChanges :: String -> Either ParseError (Bool, [Char])
readModeChanges = parse ( do { ch <- oneOf "+-";
                               rest <- many anyChar;
                               return (ch == '+', rest) } ) ""

readRecipient :: String -> Either ParseError Recipient
readRecipient = parse parseRecipient ""

-- | Parse a MODE message given the sender and arguments. The MODE message
-- has fairly complex syntax unsuited for one of the common splices in
-- 'Bot.Msg.Splices'.
parseMode :: Maybe Sender -> [String] -> Parser SEvent
parseMode s a = case do
    sender <- maybeToEither "No source for source/target message" s
    recipient <- readRecipient (a!!0)
    (ch, modes) <- readModeChanges (a!!1)
    if (length a - 2) `mod` (length modes) /= 0
      then fail $ "Wrong number of arguments to MODE message; "++show (length modes)
                  ++" mode changes but "++show (length a - 2)++" mode arguments."
      else let nargs = (length a - 2) `div` (length modes)
               changes = zip3 (repeat ch) modes $ chunksOf nargs (drop 2 a ++ repeat [])
           in return (sender, recipient, changes)
  of
  Left err -> parserFail . show $ err
  Right (sender, recipient, changes) -> return $ SMode sender recipient changes


maybeToEither e (Just x) = Right x
maybeToEither e Nothing = fail e
