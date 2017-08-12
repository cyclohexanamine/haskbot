{-# language GeneralizedNewtypeDeriving #-}

{-|
Module: Bot

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


module Bot (
    -- * The Bot monad
    Bot(..), runBot,

    -- * Global storage
    PersistentKey(..),
    getGlobal, setGlobal, modGlobal, getGlobal', setGlobal', modGlobal',
    -- ** Bot-specific keys for global store
    -- $configkey
    serverHostname, serverPort, botNick, logDest,
    -- $mainkey
    socketH, callbackList, configFile,
    -- $other
    timerList, currChanList, autoJoinList,

    -- * Callback control
    CallbackHandle(..),
    addCallback, removeCallback,
    addTimer, removeTimer, runInS,

    -- * IO
    -- ** Messaging
    writeMsg, sendMessage, joinChannels,
    -- ** Logging
    putLog, putLogInfo, putLogWarning, putLogError,

    -- * Re-exported for convenience
    liftIO,
    module Bot.Msg, module Bot.Store
    ) where

import Control.Monad (liftM, mapM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, get, put)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Char (isSpace)
import Data.Ini as I (Ini(..), readIniFile, writeIniFile, lookupValue, unIni)
import Data.Text (pack, unpack)
import Data.HashMap.Lazy as M (lookupDefault, insert, empty)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Text.Printf (hPrintf)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine, hPrint)
import Network (PortNumber)

import Bot.Store
import Bot.Msg


-- | Bot monad, having State GlobalStore and IO.
newtype Bot a = Bot {
      getBot :: StateT GlobalStore IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadState GlobalStore)

-- | Extract the IO computation by running the bot on the given initial state.
runBot :: Bot a -> GlobalStore -> IO a
runBot b = liftM fst . runStateT (getBot b)


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
                  putLogInfo $ "> " ++ rstrip msgString

-- | Send a textual message to the recipient
sendMessage :: Recipient -> String -> Bot ()
sendMessage r t = writeMsg $ CMsg PRIVMSG [rs, t]
    where rs = case r of RUser s -> s
                         RChannel c -> "#" ++ c

-- | Join the list of channels, where each element is a channel name string (e.g.,
-- @"#channame"@)
joinChannels :: [String] -> Bot ()
joinChannels = mapM_ (\c -> writeMsg $ CMsg JOIN [c])


-- | Write a line out to the log, prepended with a timestamp.
putLog :: String -- ^ Log level (e.g., @"INFO"@, @"ERROR"@, etc.)
          -> String -- ^ Line
          -> Bot ()
putLog lvl s = do logFile <- getGlobal' logDest
                  timestamp <- liftIO getCurrentTime
                  let logLine = lvl ++ " " ++ show timestamp ++ " -- " ++ s ++ "\n"
                  liftIO . putStrLn $ s
                  liftIO . appendFile logFile $ logLine
-- | Log with level INFO.
putLogInfo = putLog "INFO"
-- | Log with level WARNING.
putLogWarning = putLog "WARNING"
-- | Log with level ERROR.
putLogError = putLog "ERROR"


-- | A callback handle referring to callbacks; internally a randomly-generated
-- UUID.
newtype CallbackHandle = CallbackHandle UUID
    deriving (Eq, Show, Read)

-- | Add the given function as a callback for events; return the handle.
addCallback :: (SEvent -> Bot()) -> Bot (CallbackHandle)
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
-- a handle.
addTimer :: UTCTime -> Bot () -> Bot (CallbackHandle)
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
-- a handle.
runInS :: Integral a => a -> Bot () -> Bot (CallbackHandle)
runInS s cb = do currTime <- liftIO getCurrentTime
                 let diffTime = fromIntegral s
                 let newTime = addUTCTime diffTime currTime
                 addTimer newTime cb




-- Keys for bot things.
{- $other __Already initialised__ -}
-- | List of timed callbacks, to be called at the time specified.
timerList = GlobalKey [] "timerList" :: GlobalKey [(CallbackHandle, UTCTime, Bot ())]
-- | Channels the bot is currently in.
currChanList = GlobalKey [] "chanList" :: GlobalKey [Recipient]
-- | Channels the bot should autojoin.
autoJoinList = CacheKey [] "BOT" "autoJoinList" :: PersistentKey [Recipient]

{- $configkey __To be initialised in config:__
    The key strings here map to the relevant keys in .ini, as "SECTION.key": -}
-- | IRC server host
serverHostname = CacheKey undefined "SERVER" "hostname" :: PersistentKey String
-- | IRC server port.
serverPort = CacheKey undefined "SERVER" "port" :: PersistentKey Integer
-- | Nick of bot.
botNick = CacheKey undefined "BOT" "nick" :: PersistentKey String
-- | Logging output filename.
logDest = CacheKey undefined "LOG" "logfile" :: PersistentKey String

{- $mainkey __To be initialised elsewhere before running ('Run.startBot'):__ -}
-- | Config file filename.
configFile = GlobalKey undefined "configFile" :: GlobalKey String
-- | Socket handle
socketH = GlobalKey undefined "socketH" :: GlobalKey Handle
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
      Right t -> do
        return (read . unpack $ t)
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


-- Misc utility

rstrip = reverse . dropWhile isSpace . reverse
