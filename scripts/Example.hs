module Scripts.Example where
import Bot

testCounter = GlobalKey (0::Int) "testCounter"

respondToChanMsg :: SMsg -> Bot ()
respondToChanMsg (SPrivmsg (SUser nick _ _) ch@(RChannel _) text) =
    do count <- getGlobal testCounter
       setGlobal testCounter (count+1)
       writeMsg $ CMsg PRIVMSG [show ch, "Echoing: " ++ text ++ " - " ++ show (count+1)]
       runInS 4 . writeMsg $ CMsg PRIVMSG [show ch, "delayed action"]
