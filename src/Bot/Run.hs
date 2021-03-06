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
    handleLine, applyCallbacks, tryApply, runCallbackSafe, runTimers, runTimersList
    ) where

import Control.Concurrent (forkIO, threadDelay)
import GHC.Conc (threadStatus, ThreadStatus(ThreadDied, ThreadFinished))
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (PatternMatchFail, evaluate, try)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(LineBuffering), hSetBuffering, hGetLine, hClose)
import System.IO.Error (isDoesNotExistError, ioError, tryIOError)
import System.IO.Unsafe (unsafePerformIO)

import Bot.Bot
import Bot.Msg
import Bot.Store
import Bot.Scripting as S (callbacks)


-- Callbacks

-- | Run the priority timers, and run the non-priority timers if the bot
-- is ready.
runTimers :: Bot ()
runTimers = do runTimersList priorityTimerList
               status <- getGlobal readyStatus
               when status $ runTimersList timerList

-- | Check all the timers that are outstanding, and run all that have expired,
-- from the timer list at the given key, updating it appropriately.
runTimersList :: GlobalKey [(CallbackHandle, UTCTime, Bot ())] -> Bot ()
runTimersList tl = do
    currTime <- liftIO getCurrentTime
    expired <- consumeGlobal tl (\(h, t, c) -> t < currTime)
    mapM_ (runCallbackSafe . \(h, t, c)->c) expired

-- | Handle a line - message string - from the server, invoking the appropriate callbacks.
handleLine :: String -> Bot ()
handleLine s = case readMsg s of
    Left err -> putLogWarning ("Couldn't parse message - " ++ show err ++ " - " ++ s)
    Right msg -> putLogAll (show msg) >> applyCallbacks msg

-- | Apply all message callbacks in 'callbackList' which pattern-match the message.
applyCallbacks :: SEvent -> Bot ()
applyCallbacks msg = do callbacks <- getGlobal callbackList
                        let applicable = mapMaybe (flip tryApply msg . snd) callbacks
                        mapM_ runCallbackSafe applicable

-- | Try to apply @f@ to @v@ - if pattern matching on the argument fails,
-- return @Nothing@. Otherwise return @Just (f v)@. Used to match callbacks.
tryApply :: (a -> b) -> a -> Maybe b
tryApply f v = case unsafePerformIO . tryMatch . evaluate $ f v of
                Left err -> Nothing
                Right r -> Just r
  where tryMatch = try :: IO a -> IO (Either PatternMatchFail a)

-- | Run the given callback, handling any exceptions that occur.
runCallbackSafe :: Bot () -> Bot ()
runCallbackSafe = runSafe (\err -> putLogError $ "Error in callback: " ++ show err)

-- | Invoke callbacks for all the stored signals, clearing the list.
applySignals :: Bot ()
applySignals = do
    sigs <- consumeGlobal signalList (const True)
    mapM_ applyCallbacks sigs


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
-- if there is one. Then process any timers, if the bot is in the 'ready' state.
listenMain :: MVar String -> Bot ()
listenMain mv = do
    listenerStatus <- getGlobal listenThread >>= liftIO . threadStatus
    if listenerStatus `elem` [ThreadFinished, ThreadDied]
      then do applyCallbacks Disconnected
              liftIO $ threadDelay 1000000
              getGlobal socketH >>= liftIO . hClose
              connectAndListen
      else do mbLine <- liftIO . tryTakeMVar $ mv
              case mbLine of
                Just s -> do putLogDebug $ "< " ++ s
                             handleLine s
                Nothing -> liftIO $ threadDelay 100000
              applySignals
              runTimers
              listenMain mv

-- | We should already have initialised global variables about the server and nick
-- from the config, and we use those to connect to the server. Then start the
-- listening loop and signal a 'Connected' event.
connectAndListen :: Bot ()
connectAndListen = do
    host <- getGlobal' serverHostname
    port <- getGlobal' serverPort
    putLogInfo $ "Connecting to " ++ host ++ " " ++ show port
    hOrErr <-  liftIO . tryIOError $ (connectTo host . PortNumber . fromInteger $ port)
    case hOrErr of
      Right h -> do
        liftIO $ hSetBuffering h LineBuffering
        setGlobal socketH h

        mv <- liftIO newEmptyMVar
        listenThread' <- liftIO $ forkIO (listenH h mv)
        setGlobal listenThread listenThread'
        applyCallbacks Connected
        listenMain mv

      Left e ->
        if isDoesNotExistError e
          then do putLogInfo "Couldn't connect to host; retrying"
                  liftIO $ threadDelay 10000000
                  connectAndListen
          else liftIO . ioError $ e


-- | Initialise callbacks, signal a 'Startup' event, and then invoke
-- 'connectAndListen'.
startBot :: Bot ()
startBot = do
    putLogInfo "Bot starting up."
    mapM_ addCallback S.callbacks
    applyCallbacks Startup
    connectAndListen
