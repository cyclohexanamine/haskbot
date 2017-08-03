module Scripting where

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..)
           , joinMsg, readMsg )
import GlobalState ( GlobalState, GlobalKey(..), GlobalStore
                   , getGlobal, setGlobal, empty, runState )

data CallbackClass = SrvMsg | UserMsg | Ping | Numeric Int deriving Eq

c_chan   = "#votebot-testing"
                   
callbacks :: [( CallbackClass, SMsg -> GlobalState (Maybe CMsg) )]
callbacks = [ ( Ping, \(SPing srv) -> return $ Just (CMsg PONG [srv]) )
            , ( UserMsg, \msg@(SPrivmsg (SUser nick _ _) (RChannel _) _) -> return $ respondToChanMsg msg )
            , ( Numeric 376, \(SNumeric _ 376 _) -> return $ Just (CMsg JOIN [c_chan]) )
            ]

respondToChanMsg :: SMsg -> Maybe CMsg
respondToChanMsg (SPrivmsg (SUser nick _ _) ch text)
    | nick /= "nyaffles" = Nothing
    | otherwise = Just (CMsg PRIVMSG [show ch, "Echoing: " ++ text])
