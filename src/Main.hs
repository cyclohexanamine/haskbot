{-| module: Main

Top-level module for the bot. Deals with command-line arguments,
and starting the bot.
-}

module Main  (
    -- * Main
    main,
    -- * Command line options
    Flag(..), parseCmdArgs,
    -- * Other
    makeSafe,
    ) where

import System.Console.GetOpt (ArgOrder(..), OptDescr(..), ArgDescr(..), getOpt, usageInfo)
import System.Environment (getArgs)
import System.IO (hGetEncoding, hSetEncoding, mkTextEncoding, stdin, stdout, stderr)

import Bot (empty, runBot, configFile, setGlobalToStore)
import Bot.Run (startBot)

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
    if not (null errs) || Help `elem` argL || null configs
      then return . Left $ (concat errs ++ usageInfo header flags)
      else return . Right . head $ configs
  where header = "Usage: haskbot -c configfile [-h]"

-- | Set handles to transliterate Unicode characters instead of writing them out
-- to console and potentially causing an error.
makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding ((takeWhile (/= '/') $ show ce) ++ "//TRANSLIT") >>=
      hSetEncoding h

-- | Get the command line arguments, initialise the bot state with the location
-- of the config file, and start the bot.
main :: IO ()
main = do
    mapM_ makeSafe [stdout, stdin, stderr]
    parsed <- parseCmdArgs
    case parsed of
      Left err -> putStrLn err
      Right cfg ->  do
        let st = setGlobalToStore empty configFile cfg
        runBot startBot st
