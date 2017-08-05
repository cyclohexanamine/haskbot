module Main where

import Data.Ini as I (Ini, readIniFile, lookupValue)
import Data.Text (pack, unpack)
import Data.Either (lefts, rights)
import Data.List.Split (splitOn)

import Bot (GlobalStore, GlobalKey(..), empty, runBot, configKeys, setGlobalToStore)
import Run (startBot)


-- | Load a config file in the ini format from @loc@, returning @Right st@, a store
-- initialised with the values found, if the config sets all the relevant keys in 
-- 'configKeys'. If there are some missing keys or a problem with the config, return
-- @Left err@ instead, with an error message.
loadConfig :: String -> IO (Either String GlobalStore)
loadConfig loc = do
    iniE <- I.readIniFile loc
    case iniE of
      Left err -> do putStrLn err; return . Left $ "Couldn't find config file " ++ loc
      Right ini -> let vals = map (lookupK ini) $ configKeys
                   in if length (rights vals) /= length configKeys 
                        then return . Left $ "Missing keys " ++ show (lefts vals)
                        else return . Right . foldl foldIntoStore empty . zip configKeys . rights $ vals
  where foldIntoStore st (k, v) = setGlobalToStore st k v
        lookupK i (GlobalKey _ s) =
          case splitOn "." s of
            [sec, key] -> lookupValue (pack sec) (pack key) i >>= return . unpack
            _ -> Left $ "Invalid config key " ++ s


main :: IO ()
main = do
    cfg <- loadConfig "bot.ini"
    case cfg of Right st -> runBot startBot st
                Left err -> putStrLn err
