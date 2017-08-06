{-|
Module: Run

The main logic for the bot. Implements the core networking functionality,
as well as finding and applying callbacks.

There's a list of callbacks in 'Scripting.callbacks', and for each message,
we will invoke all functions in there that successfully pattern match the
message.

Support for non-blocking calls on Windows is poor, so we implement our own
non-blocking, using a listener thread that blocks, and an MVar between that
and the main thread. We can do non-blocking reads on the MVar reliably.
-}


module Run (
    -- * Networking
    startBot, listenMain, listenH,
    -- * Message handling
    handleLine, applyCallbacks, tryApply,
    ) where

import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(LineBuffering), hSetBuffering, hGetLine, hReady)
import Data.Maybe (catMaybes)
import Control.Exception (PatternMatchFail, evaluate, try)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Control.Concurrent.MVar

import Msg
import Bot
import Scripting (callbacks)


-- Message handling

-- | Handle a line - message string - from the server, invoking the appropriate callbacks.
handleLine :: String -> Bot ()
handleLine s = case readMsg s of
    Left err -> putLogWarning ("Couldn't parse message - " ++ show err ++ " - " ++ s)
    Right msg -> applyCallbacks msg

-- | Apply all message callbacks in 'callbacks' which pattern-match the message.
applyCallbacks :: SMsg -> Bot ()
applyCallbacks msg = mapM_ id . catMaybes . map (flip tryApply $ msg) $ callbacks

-- | Try to apply @f@ to @v@ - if pattern matching on the argument fails,
-- return @Nothing@. Otherwise return @Just (f v)@. Used to match callbacks.
tryApply :: (a -> b) -> a -> Maybe b
tryApply f v = case unsafePerformIO $ tryMatch ( evaluate (f v) ) of
                Left err -> Nothing
                Right r -> Just r
               where tryMatch = try :: IO a -> IO (Either PatternMatchFail a)


-- Networking

-- | Listen on the socket provided, writing lines we get into the @MVar@.
listenH :: Handle -> MVar String -> IO ()
listenH h mv = do
    s <- hGetLine h
    putMVar mv s
    listenH h mv
    
-- | The main listening loop: check whether 'listenH' has read a line,
-- and process that if there is one.
listenMain :: MVar String -> Bot ()
listenMain mv = do 
    mbLine <-  liftIO . tryTakeMVar $ mv 
    case mbLine of 
      Just s -> do putLogInfo $ "< " ++ s
                   handleLine s
      Nothing -> liftIO $ threadDelay 100000
    listenMain mv

-- | Startup. We should already have initialised global variables about the server and
-- nick from the config, and we use those to connect to the server, sending registration
-- info, then starting the listening loop.
startBot :: Bot ()
startBot = do
    host <- getGlobal serverHostname
    port <- getGlobal serverPort
    h <- liftIO $ connectTo host . PortNumber . fromIntegral $ (read port :: Int)
    liftIO $ hSetBuffering h LineBuffering
    setGlobal socketH h

    nick <- getGlobal botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]

    mv <- liftIO newEmptyMVar
    liftIO . forkIO $ listenH h mv
    listenMain mv
