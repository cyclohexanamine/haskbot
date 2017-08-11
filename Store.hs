{-|
Module: Store

A store mapping keys to values of arbitrary type. 'GlobalStore' is a
hashmap from @(String, TypeRep)@ pairs to @Dynamic@ objects; the keys used,
'GlobalKey', contain information about the type, so that the values can be
retrieved from the @Dynamic@ instances.

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

module Store (
    module Store,
    Typeable,
    ) where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Typeable (Typeable, TypeRep, typeOf)
import Data.HashMap.Strict as M (HashMap, lookup, member, insert, empty)

-- | Store for keeping global state of arbitrary type. It has keys of type
-- @(TypeRep, String)@, allowing for uniqueness for keys of the same type
-- and guaranteeing that keys of different types will never collide.
-- It stores values as @Dynamic@, extracting them using the type information
-- provided by the key.
type GlobalStore = M.HashMap (TypeRep, String) Dynamic

-- | An empty GlobalStore.
empty = M.empty :: GlobalStore

-- | Key for GlobalStore - a is the type that's stored. @GlobalKey a s@
-- creates a global key with:
--
-- * default value @a@ (which also specifies the type of the value being stored)
-- * id string @s@ (which should be unique for keys of the same type)
data GlobalKey a = GlobalKey a String

-- | Convenience function for converting 'GlobalKey' to '(TypeRep, String)'.
makeK :: Typeable a => GlobalKey a -> (TypeRep, String)
makeK (GlobalKey def s) = (typeOf def, s)

-- | Look up the key in the store, returning the value found if there is one,
-- or the default value contained in the key otherwise.
getGlobalFromStore :: Typeable a => GlobalStore -> GlobalKey a -> a
getGlobalFromStore st k@(GlobalKey def _) =
    case M.lookup (makeK k) st >>= fromDynamic of
      Just x -> x
      Nothing -> def

-- | Set the given value to the key in the store.
setGlobalToStore :: Typeable a => GlobalStore -> GlobalKey a -> a -> GlobalStore
setGlobalToStore st k val = M.insert (makeK k) (toDyn val) st

-- | Check whether the given key is in the given store.
isInStore :: Typeable a => GlobalStore -> GlobalKey a -> Bool
isInStore st k = member (makeK k) st
