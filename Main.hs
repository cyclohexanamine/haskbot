import Network (PortID(PortNumber), connectTo)
import System.IO (Handle, BufferMode(NoBuffering), hSetBuffering, hGetLine)
import Control.Monad (liftM, mapM_)
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes)
import Data.Either (lefts, rights)
import Data.Ini as I (Ini, readIniFile, lookupValue)
import Data.Text (pack, unpack)
import Data.List.Split (splitOn)
import Control.Exception (PatternMatchFail, evaluate, try)
import System.IO.Unsafe (unsafePerformIO)

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..)
           , joinMsg, readMsg )
import Bot ( Bot, GlobalKey(..), GlobalStore, setGlobalToStore
           , getGlobal, setGlobal, empty, runStateT
           , writeMsg, putLogInfo, putLogWarning, putLogError
           , socketH, serverHostname, serverPort, botNick, botChan, configKeys )
import Scripting ( callbacks )


-- Message handling

tryApply :: (a -> b) -> a -> Maybe b
tryApply f v = case unsafePerformIO $ tryMatch ( evaluate (f v) ) of
                Left err -> Nothing
                Right r -> Just r
               where tryMatch = try :: IO a -> IO (Either PatternMatchFail a)

findCallbacks :: SMsg -> Bot ()
findCallbacks msg = mapM_ id . catMaybes . map (flip tryApply $ msg) $ callbacks


handle :: String -> Bot ()
handle s = case readMsg s of
    Left err -> putLogWarning ("Couldn't parse message - " ++ show err ++ " - " ++ s)
    Right msg -> findCallbacks msg


-- Config

loadConfig :: String -> IO (Either String GlobalStore)
loadConfig loc = do
    iniE <- I.readIniFile loc
    case iniE of
      Left err -> do putStrLn err; return . Left $ "Couldn't find config file " ++ loc
      Right ini -> let vals = map (lookupK ini) $ configKeys
                   in if length (rights vals) /= length configKeys then return . Left $ "Missing keys " ++ show (lefts vals)
                      else return . Right . foldl (\st (k, v) -> setGlobalToStore st k v) empty . zip configKeys . rights $ vals

  where lookupK :: I.Ini -> GlobalKey String -> Either String String
        lookupK i (GlobalKey _ s) =
          case splitOn "." s of
            [sec, key] -> lookupValue (pack sec) (pack key) i >>= return . unpack
            _ -> Left $ "Invalid config key " ++ s

main = do
    cfg <- loadConfig "bot.ini"
    case cfg of Right st -> runStateT runBot st >> return ()
                Left err -> putStrLn err


-- Main

runBot :: Bot ()
runBot = do
    host <- getGlobal serverHostname
    port <- getGlobal serverPort
    h <- lift $ connectTo host . PortNumber . fromIntegral $ (read port :: Int)
    lift $ hSetBuffering h NoBuffering
    setGlobal socketH h
    
    nick <- getGlobal botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]
    
    listen


listen :: Bot ()
listen = do h <- getGlobal socketH
            s <- lift $ hGetLine h
            putLogInfo $ "< " ++ s
            handle s
            listen
