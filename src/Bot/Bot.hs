{-# language GeneralizedNewtypeDeriving #-}

{-| Module: Bot.Bot

IRC bot monad, carrying around a state and IO. Most IRC-specific logic
is implemented in other modules.

The state, 'GlobalStore', can hold values of arbitrary type; the keys used,
'GlobalKey', contain information about the type, so that the values can be
retrieved from the @Dynamic@ instances that are actually stored.

The keys use strings for uniqueness, but are also unique by type. So
two different keys will only point to the same object if they have the same
key string /and/ default values of the same type.

The keys also contain a default value, which is returned whenever the
lookup fails. This makes it seem as though the store was initialised with
the default values for all the keys.

'GlobalStore' is intended to provide a scripting interface for addon modules,
by providing a way for users to specify arbitrary stateful variables, and
share them across modules by exporting the keys.
-}


module Bot.Bot (
    -- * The Bot monad
    Bot(..), runBot, changeBotState,

    -- * Global storage
    PersistentKey(..),
    getGlobal, setGlobal, modGlobal, getGlobal', setGlobal', modGlobal',

    -- ** Bot-specific keys for global store
    -- $configkey
    serverHostname, serverPort, botNick, logDest, logLevel,
    -- $mainkey
    socketH, listenThread, callbackList, configFile,
    -- $other
    timerList, priorityTimerList, signalList, readyStatus,

    -- * Callback control
    CallbackHandle(..), nilCH,
    addCallback, removeCallback,
    addTimer, removeTimer, runInS, getEndTime, runInSPriority,
    signalEvent,

    -- * IO
    -- ** Messaging
    writeMsg,
    -- ** Logging
    putLog, putLogAll, putLogDebug, putLogInfo, putLogWarning, putLogError, putLogCritical,

    -- * Re-exported for convenience
    get, put, liftIO, rstrip,
    ) where

import Control.Concurrent (ThreadId)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, get, put)
import Control.Monad.Trans.State.Strict (StateT, runStateT)

import Data.Char (isSpace)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Ini as I (Ini(..), readIniFile, writeIniFile, lookupValue, unIni)
import Data.Text (pack, unpack)
import Data.HashMap.Lazy as M (lookupDefault, insert, empty)
import Data.List (elemIndex)
import Data.UUID (UUID, nil)
import Data.UUID.V4 (nextRandom)

import Text.Printf (hPrintf)
import Text.Read (readMaybe)
import Network (PortNumber)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine, hPrint)

import Bot.Store
import Bot.Msg


-- | Bot monad, having State GlobalStore and IO.
newtype Bot a = Bot {
      getBot :: StateT GlobalStore IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadState GlobalStore)

-- | Extract the IO computation by running the bot on the given initial state.
runBot :: Bot a -> GlobalStore -> IO a
runBot b = liftM fst . runStateT (getBot b)

-- | Modify the global state by running the computation on the initial state.
changeBotState :: Bot a -> GlobalStore -> IO GlobalStore
changeBotState b = liftM snd . runStateT (getBot b)


-- | Get the value at k in the state.
getGlobal :: Typeable a => GlobalKey a -> Bot a
getGlobal k = do store <- get
                 return $ getGlobalFromStore store k

-- | Set the value at k to v in the state.
setGlobal :: Typeable a => GlobalKey a -> a -> Bot ()
setGlobal k v = do store <- get
                   put $ setGlobalToStore store k v

-- | Modify the value at k by f.
modGlobal :: Typeable a => GlobalKey a -> (a -> a) -> Bot ()
modGlobal k f = do v <- getGlobal k
                   setGlobal k (f v)


-- | Write a message out to the server.
writeMsg :: CMsg -> Bot ()
writeMsg msg = do let msgString = joinMsg msg
                  h <- getGlobal socketH
                  liftIO . hPrintf h $ msgString
                  putLogDebug $ "> " ++ rstrip msgString


-- | Write a line out to the log, prepended with a timestamp.
putLog :: String -- ^ Log level (e.g., @"INFO"@, @"ERROR"@, etc.)
          -> String -- ^ Line
          -> Bot ()
putLog lvl s = do logFile <- getGlobal' logDest
                  logLvl <- getGlobal' logLevel
                  case do { i <- elemIndex lvl logLevels; i' <- elemIndex logLvl logLevels;
                            if i >= i' then return True else fail "" } of
                    Nothing -> return ()
                    Just _ -> do
                      timestamp <- liftIO getCurrentTime
                      let logLine = lvl ++ " " ++ show timestamp ++ " -- " ++ s
                      liftIO . putStrLn $ s
                      liftIO . appendFile logFile $ logLine
  where logLevels = ["ALL", "DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]
-- | Log with level ALL.
putLogAll = putLog "ALL"
-- | Log with level DEBUG.
putLogDebug = putLog "DEBUG"
-- | Log with level INFO.
putLogInfo = putLog "INFO"
-- | Log with level WARNING.
putLogWarning = putLog "WARNING"
-- | Log with level ERROR.
putLogError = putLog "ERROR"
-- | Log with level CRITICAL.
putLogCritical = putLog "CRITICAL"


-- | A callback handle referring to callbacks; internally a randomly-generated
-- UUID.
newtype CallbackHandle = CallbackHandle UUID
    deriving (Eq, Show, Read)
nilCH = CallbackHandle nil

-- | Add the given function as a callback for events; return the handle.
addCallback :: (SEvent -> Bot ()) -> Bot CallbackHandle
addCallback cb = do callbacks <- getGlobal callbackList
                    handle <- liftM CallbackHandle $ liftIO nextRandom
                    setGlobal callbackList $ callbacks ++ [(handle, cb)]
                    return handle

-- | Remove the callback referred to by the handle. Silently fails if there
-- is no such callback.
removeCallback :: CallbackHandle -> Bot ()
removeCallback h = do callbacks <- getGlobal callbackList
                      setGlobal callbackList . filter ((/=h).fst) $ callbacks


-- | Add a timer to run the given computation at the given time, returning
-- a handle. The computation won't be run until the bot is ready.
addTimer :: UTCTime -> Bot () -> Bot CallbackHandle
addTimer time cb = do timers <- getGlobal timerList
                      handle <- liftM CallbackHandle $ liftIO nextRandom
                      setGlobal timerList $ timers ++ [(handle, time, cb)]
                      return handle

-- | Remove the timer referred to by the handle. Silently fails if there
-- is no such timer.
removeTimer :: CallbackHandle -> Bot ()
removeTimer h = do timers <- getGlobal timerList
                   setGlobal timerList . filter (\(h',_,_)->h'/=h) $ timers

-- | Run the given computation in the given number of seconds, returning
-- a handle. The computation won't be run until the bot is ready.
runInS :: Integral a => a -> Bot () -> Bot CallbackHandle
runInS s cb = do currTime <- liftIO getCurrentTime
                 let diffTime = fromIntegral s
                 let newTime = addUTCTime diffTime currTime
                 addTimer newTime cb

-- | Get the time that the given timer will end on, or Nothing if it can't
-- be found.
getEndTime :: CallbackHandle -> Bot (Maybe UTCTime)
getEndTime h = do timers <- getGlobal timerList
                  case filter (\(h',_,_)->h'==h) timers of
                    (_,t,_):xs -> return $ Just t
                    __         -> return Nothing

-- | Run the given computation in the given number of seconds, returning
-- a handle. The computation will be run regardless of ready status.
runInSPriority :: Integral a => a -> Bot () -> Bot ()
runInSPriority s cb = do currTime <- liftIO getCurrentTime
                         let diffTime = fromIntegral s
                         let newTime = addUTCTime diffTime currTime
                         modGlobal priorityTimerList (++[(nilCH, newTime, cb)])

-- | Signal an event.
signalEvent :: SEvent -> Bot ()
signalEvent ev = modGlobal signalList (++[ev])


-- Keys for bot things.
{- $other __Already initialised__ -}
-- | List of timed callbacks, to be called at the time specified. These will
-- not be called while the bot is not ready.
timerList = GlobalKey [] "timerList" :: GlobalKey [(CallbackHandle, UTCTime, Bot ())]
-- | List of timed callbacks, to be called at the time specified regardless of
-- whether the bot is ready.
priorityTimerList = GlobalKey [] "priorityTimerList" :: GlobalKey [(CallbackHandle, UTCTime, Bot ())]
-- | List of events to signal immediately. Events can't be signalled directly within callbacks,
-- so they're put here and signalled by the event loop.
signalList = GlobalKey [] "signalList" :: GlobalKey [SEvent]
-- | Whether we're ready or not - 'ready' means connected to the server, and in all
-- the autojoin channels. This has to be here so that 'Bot.Run' can see it.
readyStatus = GlobalKey False "readyStatus" :: GlobalKey Bool

{- $configkey __To be initialised in config:__
    The key strings here map to the relevant keys in .ini, as "SECTION.key": -}
-- | IRC server host
serverHostname = CacheKey undefined "SERVER" "hostname" :: PersistentKey String
-- | IRC server port.
serverPort = CacheKey undefined "SERVER" "port" :: PersistentKey Integer
-- | Nick of bot.
botNick = CacheKey undefined "BOT" "nick" :: PersistentKey String
-- | Logging output filename.
logDest = CacheKey undefined "LOG" "logFile" :: PersistentKey String
-- | Minimum logging level to record. Log levels are
--
-- * 'CRITICAL' - some unrecoverable error for the whole bot, probably crashing it.
-- * 'ERROR' - something has gone wrong, possibly an exception in a callback.
-- * 'WARNING' - something bad but expectable has happened.
-- * 'INFO' - something of interest has happened.
-- * 'DEBUG' - verbose output, but still human-readable.
-- * 'ALL' - very verbose.
logLevel = CacheKey "INFO" "LOG" "logLevel" :: PersistentKey String

{- $mainkey __To be initialised elsewhere before running ('Bot.Run.startBot'):__ -}
-- | Config file filename.
configFile = GlobalKey undefined "configFile" :: GlobalKey String
-- | Socket handle
socketH = GlobalKey undefined "socketH" :: GlobalKey Handle
-- | Listener thread ID
listenThread = GlobalKey undefined "listenH" :: GlobalKey ThreadId
-- | List of callbacks to apply to messages
callbackList = GlobalKey [] "callbacks" :: GlobalKey [(CallbackHandle, SEvent -> Bot ())]

-- Store-specific

-- | @PersistentKey a sec name@ creates a key that transparently reads/writes
-- a value to the config file, as a value called @name@ in section @sec@.
--
-- @CacheKey a sec name@ does the same, but also acts as an instance of
-- @GlobalKey a (sec ++ "." ++ name)@; only reading the value from file once,
-- storing the value in the store in memory and reading from that subequently.
-- It does write to the file for every new value, though.
--
-- The objects referred to must have 'Eq', 'Read' and 'Show' instances for this.
data PersistentKey a
    = PersistentKey a String String
    | CacheKey a String String
    deriving (Read, Show, Eq)

toGlobalKey (CacheKey d s n) = GlobalKey d $ s ++ "." ++ n
toPersKey (CacheKey d s n) = PersistentKey d s n

-- | 'getGlobal', but for 'PersistentKey'. This is a separate function because
-- the typeclass restrictions are stronger than 'getGlobal'.
getGlobal' :: (Eq a, Read a, Show a, Typeable a) => PersistentKey a -> Bot a
getGlobal' (PersistentKey def sec nm) = do
    cfg <- getGlobal configFile
    iniE <- liftIO $ I.readIniFile cfg
    case iniE >>= lookupValue (pack sec) (pack nm) of
      Left err -> return def
      Right t -> case readMaybe . unpack $ t of
                   Just v -> return v
                   Nothing -> error $ "Bad string in config file for "++sec
                                ++"."++nm++" - couldn't read a value of type "
                                ++showTypeSig def++" from string " ++show (unpack t)
getGlobal' k@(CacheKey _ _ _) = do
    st <- get
    let gk = toGlobalKey k
    if isInStore st gk
        then getGlobal gk
        else do
          v <- getGlobal' . toPersKey $ k
          setGlobal gk v
          return v

-- | 'setGlobal', but for 'PersistentKey'. This is a separate function because
-- the typeclass restrictions are stronger than 'setGlobal'.
setGlobal' :: (Eq a, Read a, Show a, Typeable a) => PersistentKey a -> a -> Bot ()
setGlobal' (PersistentKey def sec nm) v = do
    cfg <- getGlobal configFile
    iniE <- liftIO $ I.readIniFile cfg
    case iniE of
      Right ini -> do
        let newSec = M.insert (pack nm) (pack . show $ v) . M.lookupDefault M.empty (pack sec) $ I.unIni ini
        let newIni = I.Ini . M.insert (pack sec) newSec . I.unIni $ ini
        liftIO $ I.writeIniFile cfg newIni
      Left err -> error err
setGlobal' k@(CacheKey _ _ _) v = do
    oldV <- getGlobal' k
    if oldV == v
      then return ()
      else  do setGlobal (toGlobalKey k) v
               setGlobal' (toPersKey k) v

-- | 'modGlobal', but for 'PersistentKey'. This is a separate function because
-- the typeclass restrictions are stronger than 'setGlobal'.
modGlobal' :: (Eq a, Read a, Show a, Typeable a) => PersistentKey a -> (a -> a) -> Bot ()
modGlobal' k f = do v <- getGlobal' k
                    setGlobal' k (f v)


rstrip = reverse . dropWhile isSpace . reverse
