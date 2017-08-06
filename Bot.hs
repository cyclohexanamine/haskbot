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

{-# language GeneralizedNewtypeDeriving #-}

module Bot (
    -- * The Bot monad
    Bot(..), runBot, Bot.empty,

    -- * Global storage
    getGlobal, setGlobal,
    -- ** Bot-specific keys for global store
    -- $configkey
    serverHostname, serverPort, botNick, botChan, logDest, configKeys,
    -- $mainkey
    socketH, callbackList,
    -- $other
    timerList,

    -- * Callback control
    CallbackHandle(..),
    addCallback, removeCallback,
    addTimer, removeTimer, runInS,

    -- * IO
    -- ** Messaging
    writeMsg, sendMessage, joinChannels,
    -- ** Logging
    putLog, putLogInfo, putLogWarning, putLogError,

    -- * Global storage implementation
    GlobalStore(..), GlobalKey(..),
    getGlobalFromStore, setGlobalToStore,

    -- * Re-exported for convenience
    liftIO,
    module Msg,
    ) where

import Control.Monad (liftM, mapM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, get, put)
import Control.Monad.Trans.State.Strict (StateT, runStateT)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable, TypeRep, typeOf)
import Data.HashMap.Strict as M (HashMap, lookup, insert, empty)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Char (isSpace)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Text.Printf (hPrintf)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine, hPrint)

import Msg


-- | Bot monad, having State GlobalStore and IO.
newtype Bot a = Bot {
      getBot :: StateT GlobalStore IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadState GlobalStore)

-- | Extract the IO computation by running the bot on the given initial state.
runBot :: Bot a -> GlobalStore -> IO a
runBot b = liftM fst . runStateT (getBot b)

-- | An empty GlobalStore.
empty = M.empty :: GlobalStore


-- | Get the value at k in the state.
getGlobal :: Typeable a => GlobalKey a -> Bot a
getGlobal k = do store <- get
                 return $ getGlobalFromStore store k

-- | Set the value at k to v in the state.
setGlobal :: Typeable a => GlobalKey a -> a -> Bot ()
setGlobal k v = do store <- get
                   put $ setGlobalToStore store k v


-- | Write a message out to the server.
writeMsg :: CMsg -> Bot ()
writeMsg msg = do let msgString = joinMsg msg
                  h <- getGlobal socketH
                  liftIO . hPrintf h $ msgString
                  putLogInfo $ "> " ++ rstrip msgString

-- | Send a textual message to the recipient
sendMessage :: Recipient -> String -> Bot ()
sendMessage r t = writeMsg $ CMsg PRIVMSG [show r, t]

-- | Join the list of channels, where each element is a channel name string (e.g.,
-- @"#channame"@)
joinChannels :: [String] -> Bot ()
joinChannels = mapM_ (\c -> writeMsg $ CMsg JOIN [c])


-- | Write a line out to the log, prepended with a timestamp.
putLog :: String -- ^ Log level (e.g., @"INFO"@, @"ERROR"@, etc.)
          -> String -- ^ Line
          -> Bot ()
putLog lvl s = do logFile <- getGlobal logDest
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

{- $configkey __To be initialised in config:__
    The key strings here map to the relevant keys in .ini, as "SECTION.key": -}
-- | IRC server host
serverHostname = GlobalKey undefined "SERVER.hostname" :: GlobalKey String
-- | IRC server port.
serverPort = GlobalKey undefined "SERVER.port" :: GlobalKey String
-- | Nick of bot.
botNick = GlobalKey undefined "BOT.nick" :: GlobalKey String
-- | Channel that bot should join.
botChan = GlobalKey undefined "BOT.chan" :: GlobalKey String
-- | Logging output filename.
logDest = GlobalKey undefined "LOG.logfile" :: GlobalKey String
-- | A list of the above keys.
configKeys = [serverHostname, serverPort, botNick, botChan, logDest]

{- $mainkey __To be initialised elsewhere before running ('Run.startBot'):__ -}
-- | Socket handle
socketH = GlobalKey undefined "socketH" :: GlobalKey Handle
-- | List of callbacks to apply to messages
callbackList = GlobalKey [] "callbacks" :: GlobalKey [(CallbackHandle, SEvent -> Bot ())]

-- Store-specific

-- | Store for keeping global state of arbitrary type. It has keys of type
-- @(TypeRep, String)@, allowing for uniqueness for keys of the same type
-- and guaranteeing that keys of different types will never collide.
-- It stores values as @Dynamic@, extracting them using the type information
-- provided by the key.
type GlobalStore = M.HashMap (TypeRep, String) Dynamic

-- | Key for GlobalStore - a is the type that's stored. @GlobalKey a s@
-- creates a global key with:
--
-- * default value @a@ (which also specifies the type of the value being stored)
-- * id string @s@ (which should be unique for keys of the same type)
data GlobalKey a = GlobalKey a String

-- | Look up the key in the store, returning the value found if there is one,
-- or the default value contained in the key otherwise.
getGlobalFromStore :: Typeable a => GlobalStore -> GlobalKey a -> a
getGlobalFromStore st (GlobalKey def s) =
    case M.lookup (typeOf def, s) st >>= fromDynamic of
      Just x -> x
      Nothing -> def

-- | Set the given value to the key in the store.
setGlobalToStore :: Typeable a => GlobalStore -> GlobalKey a -> a -> GlobalStore
setGlobalToStore st (GlobalKey def s) val = M.insert k v st
    where k = (typeOf def, s)
          v = toDyn val


-- Misc utility

rstrip = reverse . dropWhile isSpace . reverse
