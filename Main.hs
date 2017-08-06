{-| module: Main

Top-level module for the bot. Deals with command-line arguments,
loading the config, and initialising the bot state.
-}

module Main  (
    -- * Command line options
    Flag(..), parseCmdArgs,
    -- * Main
    main,
    ) where

import Data.Ini as I (Ini, readIniFile, lookupValue)
import Data.Text (pack, unpack)
import Data.Either (lefts, rights)
import Data.List.Split (splitOn)
import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo)
import System.Environment (getArgs)

import Bot (GlobalStore, GlobalKey(..), empty, runBot, configFile, setGlobalToStore)
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
  where header = "Usage: haskbot -c configfile [-h]"

main :: IO ()
main = do
    parsed <- parseCmdArgs
    case parsed of
      Left err -> putStrLn err
      Right cfg ->  do
        let st = setGlobalToStore empty configFile cfg
        runBot startBot st
