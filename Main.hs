import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine)
import Control.Monad (liftM, mapM_)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import Control.Exception (PatternMatchFail, evaluate, try)
import System.IO.Unsafe (unsafePerformIO)

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..)
           , joinMsg, readMsg )
import Bot ( Bot, GlobalKey(..), GlobalStore
           , getGlobal, setGlobal, empty, runStateT
           , writeMsg, putLogInfo, putLogWarning, putLogError
           , socketH )
import Scripting ( callbacks )


-- Config

c_server = "irc.rizon.net"
c_port   = 6667
c_chan   = "#votebot-testing"
c_nick   = "voteplus"


-- Message handling

tryApply :: (a -> b) -> a -> Maybe b
tryApply f v = case unsafePerformIO $ tryMatch ( evaluate (f v) ) of
                Left err -> Nothing
                Right r -> Just r
               where tryMatch = try :: IO a -> IO (Either PatternMatchFail a)

findCallbacks :: SMsg -> Bot ()
findCallbacks msg = mapM_ id . catMaybes . map (flip tryApply $ msg) $ callbacks
                       

handle :: String -> Bot ()
handle s = case readMsg s of
    Left err -> putLogWarning ("Couldn't parse message - " ++ show err ++ " - " ++ s)
    Right msg -> findCallbacks msg


-- Main

main = do
    h <- connectTo c_server (PortNumber (fromIntegral c_port))
    runStateT (runBot h) empty
    
runBot :: Handle -> Bot ()
runBot h = do
    setGlobal socketH h
    lift $ hSetBuffering h NoBuffering
    writeMsg $ CMsg NICK [c_nick]
    writeMsg $ CMsg USER [c_nick, "0", "*" , c_nick]
    listen
    

listen :: Bot ()
listen = do h <- getGlobal socketH
            s <- lift $ hGetLine h
            putLogInfo $ "< " ++ s
            handle s
            listen
