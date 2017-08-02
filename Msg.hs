module Msg ( ClientCmd(..)
           , CMsg(..), SMsg(..)
           , Sender(..), Recipient(..)
           , joinMsg, readMsg )
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim ( parserFail )
import Text.Read (readMaybe)
import Data.List (intercalate)


data ClientCmd = NICK | USER | JOIN | PONG | PRIVMSG
    deriving Show

data CMsg = CMsg { command :: ClientCmd, argsC :: [String] }
    deriving Show

data SMsg = SPing { srv :: String }
          | SNotice { from :: Sender, to :: Recipient, text :: String }
          | SPrivmsg { from :: Sender, to :: Recipient, text :: String }
          | SNumeric { fromMaybe :: Maybe Sender, n :: Int, args :: [String] }
    deriving Show

data Sender = SUser { nick :: String, user :: String, host :: String }
            | SServer { host :: String }
    deriving Show

data Recipient = RUser String | RChannel String
instance Show Recipient where
    show (RUser r) = r
    show (RChannel c) = "#" ++ c




-- | Convert a message to a string in the IRC format.
joinMsg :: CMsg -> String
joinMsg (CMsg cmd args) = show cmd ++ end ++ "\r\n"
    where end = if length args > 1
                  then " " ++ (intercalate " " . init $ args) ++ " :" ++ last args
                  else if length args == 1 then " :" ++ last args
                  else ""

-- | Parse a message string in the IRC format.
readMsg :: String -> Either ParseError SMsg
readMsg = parse parseMsg ""


parseMsg :: Parser SMsg
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

parseUntil :: String -> Parser String
parseUntil s = many1 $ do notFollowedBy eof
                          noneOf s

parseWord :: Parser String
parseWord = do str <- parseUntil " :\r\n"
               optionMaybe . char $ ' '
               return str

parseSender :: Parser Sender
parseSender = char ':' >> (user <|> host)
    where user = try $ do nick <- parseUntil "!"
                          char '!'
                          user <- parseUntil "@"
                          char '@'
                          host <- parseWord
                          return $ SUser nick user host
          host = parseWord >>= return . SServer

parseRecipient :: Parser Recipient
parseRecipient = channel <|> user
    where channel = char '#' >> parseUntil "" >>= return . RChannel
          user = parseUntil "" >>= return . RUser

parseArgs :: Parser [String]
parseArgs = do args <- many parseWord
               lastArg <- optionMaybe $ do char ':'
                                           many . noneOf $ "\r\n"
               manyTill (oneOf "\r\n") eof
               return $ case lastArg of Just a  -> args ++ [a]
                                        Nothing -> args


