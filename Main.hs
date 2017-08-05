{-| module: Main

Top-level module for the bot. Deals with command-line arguments,
loading the config, and initialising the bot state.
-}

module Main  (
    -- * Command line options
    Flag(..), parseCmdArgs,
    -- * Config
    loadConfig,
    -- * Main
    main,
    ) where

import Data.Ini as I (Ini, readIniFile, lookupValue)
import Data.Text (pack, unpack)
import Data.Either (lefts, rights)
import Data.List.Split (splitOn)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo)
import System.Environment (getArgs)

import Bot (GlobalStore, GlobalKey(..), empty, runBot, configKeys, setGlobalToStore)
import Run (startBot)

-- | Command line flags
data Flag
    = Config String -- ^ @-c configfile@ or @--config configfile@
    | Help          -- ^ @-h@ or @--help@
    deriving (Eq, Show)

flags =
   [ Option ['c'] ["config"]       (ReqArg Config "configfile")
        "Specify location of config file"
   , Option ['h'] ["help"]         (NoArg Help)
        "Print this help message"
   ]

-- | Parse the command line arguments, returning @Right config@ if there's a
-- config file location specified, or @Left err@ with an error message otherwise.
parseCmdArgs :: IO (Either String String)
parseCmdArgs =  do
    args <- getArgs
    let (argL, _, errs) = getOpt Permute flags args
    let configs = [s | Config s <- argL]
    if length errs /= 0 || Help `elem` argL || length configs == 0
      then return . Left $ (concat errs ++ usageInfo header flags)
      else return . Right . head $ configs
  where header = "Usage: Main -c configfile [-h]"


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
    parsed <- parseCmdArgs
    case parsed of
      Left err -> putStrLn err
      Right st ->  do
        cfg <- loadConfig st
        case cfg of Right st -> runBot startBot st
                    Left err -> putStrLn err
