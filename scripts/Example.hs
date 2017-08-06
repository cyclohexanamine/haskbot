module Scripts.Example where
import Bot

testCounter = GlobalKey (0::Int) "testCounter"

respondToChanMsg :: SEvent -> Bot ()
respondToChanMsg (SPrivmsg (SUser nick _ _) ch@(RChannel _) text) =
    do count <- getGlobal testCounter
       setGlobal testCounter (count+1)
       sendMessage ch $ "Echoing: " ++ text ++ " - " ++ show (count+1)
       runInS 4 . sendMessage ch $ "delayed action"
       return ()
