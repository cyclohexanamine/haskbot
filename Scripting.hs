module Scripting where

import Msg ( CMsg(..), ClientCmd(..), SMsg(..), Sender(..), Recipient(..) )
import Bot ( Bot, GlobalKey(..),
           , getGlobal, setGlobal
           , writeMsg, putLogInfo )

testCounter = GlobalKey (0::Int) "testCounter"


c_chan   = "#votebot-testing"

callbacks :: [SMsg -> Bot ()]
callbacks = [ respondToPing
            , respondToChanMsg
            , respondTo376
            ]

respondToChanMsg :: SMsg -> Bot ()
respondToChanMsg (SPrivmsg (SUser nick _ _) ch@(RChannel _) text) =
    do count <- getGlobal testCounter
       setGlobal testCounter (count+1)
       writeMsg $ CMsg PRIVMSG [show ch, "Echoing: " ++ text ++ " - " ++ show (count+1)]

respondToPing :: SMsg -> Bot ()
respondToPing (SPing srv) = writeMsg $ CMsg PONG [srv]

respondTo376 :: SMsg -> Bot ()
respondTo376 (SNumeric _ 376 _) = writeMsg $ CMsg JOIN [c_chan]

