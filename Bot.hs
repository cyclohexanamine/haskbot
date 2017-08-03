import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine)
import Text.Printf (hPrintf)
import Control.Concurrent (ThreadId, forkIO, threadDelay)

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..)
           , joinMsg, readMsg )


-- Config

c_server = "irc.rizon.net"
c_port   = 6667
c_chan   = "#votebot-testing"
c_nick   = "voteplus"


-- Logic

respond :: SMsg -> Maybe CMsg
respond msg = case msg of
    SPing srv -> Just (CMsg PONG [srv])
    SNumeric _ 376 _ -> Just (CMsg JOIN [c_chan])
    SPrivmsg (SUser nick _ _) (RChannel _) _ -> if nick /= c_nick
                                                  then respondToChanMsg msg
                                                  else Nothing
    _ -> Nothing

respondToChanMsg :: SMsg -> Maybe CMsg
respondToChanMsg (SPrivmsg (SUser nick _ _) ch text)
    | nick /= "nyaffles" = Nothing
    | otherwise = Just (CMsg PRIVMSG [show ch, "Echoing: " ++ text])


-- Message handling

handle :: Handle -> String -> IO ()
handle h s = case readMsg s of
    Left err -> putStrLn ("Message parse error - " ++ show err ++ " - " ++ s)
    Right msg -> case respond msg of
                    Just response -> writeMsg h response
                    Nothing       -> return ()

writeMsg :: Handle -> CMsg -> IO ()
writeMsg h m = let msgString = joinMsg m
                 in do hPrintf h msgString
                       putStrLn $ "> " ++ msgString


-- Main

main = do
    h <- connectTo c_server (PortNumber (fromIntegral c_port))
    hSetBuffering h NoBuffering
    writeMsg h . CMsg NICK $ [c_nick]
    writeMsg h . CMsg USER $ [c_nick, "0", "*" , c_nick]
    listen h

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s
    handle h s
  where
    forever a = do a; forever a
