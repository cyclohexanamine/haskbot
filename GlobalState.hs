module GlobalState ( GlobalState, GlobalKey(..), GlobalStore
                   , getGlobal, setGlobal, empty, runState
                   ) where

import Data.Dynamic
import Data.Typeable
import Data.HashMap.Strict as M
import Control.Monad.State.Strict

-- | Key for GlobalStore - a is the type that's stored.
data GlobalKey a = GlobalKey a String -- ^ Parameters: default value; id string (should be unique for keys of the same type)

-- | Store for keeping global state of arbitrary type.
type GlobalStore = M.HashMap (TypeRep, String) Dynamic

getGlobalFromStore :: Typeable a => GlobalStore -> GlobalKey a -> a
getGlobalFromStore st (GlobalKey def s) =
    case M.lookup (typeOf def, s) st >>= fromDynamic of
      Just x -> x
      Nothing -> def

setGlobalToStore :: Typeable a => GlobalStore -> GlobalKey a -> a -> GlobalStore
setGlobalToStore st (GlobalKey def s) val = insert k v st
    where k = (typeOf def, s)
          v = toDyn val

-- | State monad wrapping GlobalStore
type GlobalState = State GlobalStore

-- | Get the value at k in the state.
getGlobal :: Typeable a => GlobalKey a -> GlobalState a
getGlobal k = do store <- get
                 return $ getGlobalFromStore store k

-- | Set the value at k to v in the state.
setGlobal :: Typeable a => GlobalKey a -> a -> GlobalState ()
setGlobal k v = do store <- get
                   put $ setGlobalToStore store k v
