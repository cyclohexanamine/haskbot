module Bot ( Bot, GlobalKey(..), GlobalStore
           , getGlobal, setGlobal, empty, runStateT
           , socketH, logDest
           , putLog, putLogInfo, putLogWarning, putLogError
           , writeMsg
           ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable, TypeRep, typeOf)
import Data.HashMap.Strict as M (HashMap, lookup, insert, empty)
import Data.Time.Clock (getCurrentTime)
import Text.Printf (hPrintf)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine, hPrint)

import Msg ( CMsg, joinMsg )


-- | Bot monad, having State GlobalStore and IO.
type Bot = StateT GlobalStore IO

-- | Get the value at k in the state.
getGlobal :: Typeable a => GlobalKey a -> Bot a
getGlobal k = do store <- get
                 return $ getGlobalFromStore store k

-- | Set the value at k to v in the state.
setGlobal :: Typeable a => GlobalKey a -> a -> Bot ()
setGlobal k v = do store <- get
                   put $ setGlobalToStore store k v

-- | Key for logging output filename
logDest :: GlobalKey String
logDest = GlobalKey "bot.log" "logDest"

-- | Key for socket handle
socketH :: GlobalKey Handle
socketH = GlobalKey undefined "socketH"

-- | Write a message out to the server
writeMsg :: CMsg -> Bot ()
writeMsg msg = do let msgString = joinMsg msg
                  h <- getGlobal socketH
                  lift $ hPrintf h msgString
                  putLogInfo $ "> " ++ msgString

-- | Write a line out to the log, prepended with a timestamp
putLog :: String -- ^ Log level (e.g., "INFO", "ERROR", etc.)
          -> String -- ^ Line
          -> Bot ()
putLog lvl s = do logFile <- getGlobal logDest
                  timestamp <- lift getCurrentTime
                  let logLine = lvl ++ " " ++ show timestamp ++ " -- " ++ s ++ "\n"
                  lift $ appendFile logFile logLine
                  lift . putStrLn $ s
                  
putLogInfo = putLog "INFO"
putLogWarning = putLog "WARNING"
putLogError = putLog "ERROR"


-- Store-specific
                   
-- | Key for GlobalStore - a is the type that's stored.
data GlobalKey a = GlobalKey a  -- ^ default value: 
                             String -- ^ id string (should be unique for keys of the same type)

-- | Store for keeping global state of arbitrary type.
type GlobalStore = M.HashMap (TypeRep, String) Dynamic

getGlobalFromStore :: Typeable a => GlobalStore -> GlobalKey a -> a
getGlobalFromStore st (GlobalKey def s) =
    case M.lookup (typeOf def, s) st >>= fromDynamic of
      Just x -> x
      Nothing -> def

setGlobalToStore :: Typeable a => GlobalStore -> GlobalKey a -> a -> GlobalStore
setGlobalToStore st (GlobalKey def s) val = M.insert k v st
    where k = (typeOf def, s)
          v = toDyn val

          
