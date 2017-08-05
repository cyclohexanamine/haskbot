module Run (startBot) where

import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine)
import Data.Maybe (catMaybes)
import Control.Exception (PatternMatchFail, evaluate, try)
import System.IO.Unsafe (unsafePerformIO)

import Msg
import Bot
import Scripting (callbacks)


-- Message handling

-- | Handle a line - message string - from the server, invoking the appropriate callbacks.
-- There's a list of callbacks in 'Scripting.callbacks', and we will invoke all functions in
-- there that successfully pattern match.
handleLine :: String -> Bot ()
handleLine s = case readMsg s of
    Left err -> putLogWarning ("Couldn't parse message - " ++ show err ++ " - " ++ s)
    Right msg -> applyCallbacks msg

-- | Apply all message callbacks in 'callbacks' which pattern-match the message.
applyCallbacks :: SMsg -> Bot ()
applyCallbacks msg = mapM_ id . catMaybes . map (flip tryApply $ msg) $ callbacks

-- | Try to apply 'f' to 'v' - if pattern matching on the argument fails,
-- return 'Nothing'. Otherwise return 'Just (f v)'. Used to match callbacks.
tryApply :: (a -> b) -> a -> Maybe b
tryApply f v = case unsafePerformIO $ tryMatch ( evaluate (f v) ) of
                Left err -> Nothing
                Right r -> Just r
               where tryMatch = try :: IO a -> IO (Either PatternMatchFail a)


-- Networking
      
-- | The main listening loop: listen on the socket in 'socketH' until we get a
-- line, then respond to that line and loop again.
listen :: Bot ()
listen = do h <- getGlobal socketH
            s <- liftIO $ hGetLine h
            putLogInfo $ "< " ++ s
            handleLine s
            listen

-- | Startup. We should already have initialised global variables about the server and
-- nick from the config, and we use those to connect to the server, sending registration
-- info, then starting the listening loop.
startBot :: Bot ()
startBot = do
    host <- getGlobal serverHostname
    port <- getGlobal serverPort
    h <- liftIO $ connectTo host . PortNumber . fromIntegral $ (read port :: Int)
    liftIO $ hSetBuffering h NoBuffering
    setGlobal socketH h

    nick <- getGlobal botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]

    listen
