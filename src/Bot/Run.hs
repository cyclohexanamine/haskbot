{-|
Module: Bot.Run

The main event logic for the bot. Implements the core networking
functionality, as well as finding and applying callbacks.

There's a list of callbacks in 'Scripting.callbacks', and for each message,
we will invoke all functions in there that successfully pattern match the
message.

Support for non-blocking calls on Windows is poor, so we implement our own
non-blocking socket reads using a listener thread that blocks ('listenH')
and an MVar between that and the main thread ('listenMain').
We can do non-blocking reads on the MVar reliably.
-}


module Bot.Run (
    -- * Networking
    startBot, connectAndListen, listenMain, listenH,
    -- * Callback handling
    handleLine, applyCallbacks, tryApply, runTimers
    ) where

import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(LineBuffering), hSetBuffering, hGetLine, hReady)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (PatternMatchFail, evaluate, try)
import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)

import Bot
import Bot.Scripting as S (callbacks)


-- Callbacks

-- | Check all the timers that are outstanding, and run all that have expired.
runTimers :: Bot ()
runTimers = do timers <- getGlobal timerList
               currTime <- liftIO getCurrentTime
               let expired = filter (\(h, t, c) -> t < currTime) timers
               let newTimers = mapMaybe (\a@(h, t, c) -> if t < currTime then Nothing else Just a) timers
               mapM_ id . map (\(h, t, c)->c) $ expired
               setGlobal timerList newTimers

-- | Handle a line - message string - from the server, invoking the appropriate callbacks.
handleLine :: String -> Bot ()
handleLine s = case readMsg s of
    Left err -> putLogWarning ("Couldn't parse message - " ++ show err ++ " - " ++ s)
    Right msg -> applyCallbacks msg

-- | Apply all message callbacks in 'callbackList' which pattern-match the message.
applyCallbacks :: SEvent -> Bot ()
applyCallbacks msg = do callbacks <- getGlobal callbackList
                        mapM_ id . catMaybes . map (flip tryApply $ msg) . map snd $callbacks

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

-- | The main listening loop:
-- Check whether the socket listener thread has died, and restart things
-- with a backoff if so.
-- Otherwise, check whether 'listenH' has read a line, and process that
-- if there is one. Then process any timers.
listenMain :: MVar String -> MVar () ->  Bot ()
listenMain mv dead = do
    mbDead <- liftIO . tryTakeMVar $ dead
    case mbDead of
      Just _ -> do putLogWarning "Thread listener died; restarting..."
                   liftIO $ threadDelay 1000000
                   connectAndListen
      Nothing -> do mbLine <-  liftIO . tryTakeMVar $ mv
                    case mbLine of
                      Just s -> do putLogInfo $ "< " ++ s
                                   handleLine s
                      Nothing -> liftIO $ threadDelay 100000
                    runTimers
                    listenMain mv dead

-- | We should already have initialised global variables about the server and nick
-- from the config, and we use those to connect to the server. Then start the
-- listening loop and signal a 'Connected' event.
connectAndListen :: Bot ()
connectAndListen = do
    host <- getGlobal' serverHostname
    port <- getGlobal' serverPort
    h <- liftIO $ connectTo host . PortNumber . fromInteger $ port
    liftIO $ hSetBuffering h LineBuffering
    setGlobal socketH h

    mv <- liftIO newEmptyMVar
    deadListener <- liftIO newEmptyMVar
    liftIO $ forkFinally (listenH h mv) (\_ -> putMVar deadListener ())

    applyCallbacks Connected
    listenMain mv deadListener


-- | Initialise callbacks, signal a 'Startup' event, and then invoke
-- 'connectAndListen'.
startBot :: Bot ()
startBot = do
    mapM_ addCallback S.callbacks
    applyCallbacks Startup
    connectAndListen
