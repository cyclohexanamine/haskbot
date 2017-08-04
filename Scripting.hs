module Scripting where

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..)
           , joinMsg, readMsg )
import GlobalState ( GlobalState, GlobalKey(..), GlobalStore
                   , getGlobal, setGlobal, empty, runState )

data CallbackClass = SrvMsg | UserMsg | Ping | Numeric Int deriving Eq

testCounter = GlobalKey (0::Int) "testCounter"


c_chan   = "#votebot-testing"

callbacks :: [SMsg -> GlobalState (Maybe CMsg)]
callbacks = [ respondToPing
            , respondToChanMsg
            , respondTo376
            ]

respondToChanMsg :: SMsg -> GlobalState (Maybe CMsg)
respondToChanMsg (SPrivmsg (SUser nick _ _) ch text)
    do count <- getGlobal testCounter
       setGlobal testCounter (count+1)
       return $ Just (CMsg PRIVMSG [show ch, "Echoing: " ++ text ++ " - " ++ show (count+1)])

respondToPing :: SMsg -> GlobalState (Maybe CMsg)
respondToPing (SPing srv) = return $ Just (CMsg PONG [srv])

respondTo376 :: SMsg -> GlobalState (Maybe CMsg)
respondTo376 (SNumeric _ 376 _) = return $ Just (CMsg JOIN [c_chan])

