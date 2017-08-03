import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine)
import Text.Printf (hPrintf)
import Control.Monad (liftM, mapM_)
import Data.Maybe (catMaybes)

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..)
           , joinMsg, readMsg )
import GlobalState ( GlobalState, GlobalKey(..), GlobalStore
                   , getGlobal, setGlobal, empty, runState )


-- Config

c_server = "irc.rizon.net"
c_port   = 6667
c_chan   = "#votebot-testing"
c_nick   = "voteplus"


-- Logic
findCallbacks :: SMsg -> [GlobalState (Maybe CMsg)]
findCallbacks msg = case msg of
    SPing srv -> [return $ Just (CMsg PONG [srv])]
    SNumeric _ 376 _ -> [return $ Just (CMsg JOIN [c_chan])]
    SPrivmsg (SUser nick _ _) (RChannel _) _ -> if nick /= c_nick
                                                  then [return $ respondToChanMsg msg]
                                                  else [return Nothing]
    _ -> [return Nothing]
    
respondToChanMsg :: SMsg -> Maybe CMsg
respondToChanMsg (SPrivmsg (SUser nick _ _) ch text)
    | nick /= "nyaffles" = Nothing
    | otherwise = Just (CMsg PRIVMSG [show ch, "Echoing: " ++ text])

respond :: SMsg -> GlobalState [CMsg]
respond msg = liftM catMaybes . sequence $ cb
    where cb = findCallbacks msg 


-- Message handling

handle :: Handle -> String -> GlobalState (IO ())
handle h s = case readMsg s of
    Left err -> return $! putStrLn ("Message parse error - " ++ show err ++ " - " ++ s)
    Right msg -> (liftM $ mapM_ (writeMsg h)) (respond msg)

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
    listen h empty
    
listen :: Handle -> GlobalStore -> IO ()
listen h st = do s <- hGetLine h
                 putStrLn s
                 let (ioact, newSt) = runState (handle h s) st
                  in do ioact; listen h newSt
