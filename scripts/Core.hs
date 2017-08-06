module Scripts.Core where
import Bot

respondToPing :: SMsg -> Bot ()
respondToPing (SPing srv) = writeMsg $ CMsg PONG [srv]

respondTo376 :: SMsg -> Bot ()
respondTo376 (SNumeric _ 376 _) = do
    chan <- getGlobal botChan
    writeMsg $ CMsg JOIN [chan]
