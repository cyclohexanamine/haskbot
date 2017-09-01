{-| module: Bot.Scripting.Example

Some example functionality for the bot.

-}

module Bot.Scripting.Example where
import Bot

-- | The callbacks that should be hooked immediately on the bot starting.
-- 'Bot.Scripting' needs to add this to its master callback list.
callbacks = [ respondToChanMsg ]

-- | Echo all channel messages, keeping count of the number of messages
-- we've echoed. And enact a delayed action a couple of seconds later.
respondToChanMsg :: SEvent -> Bot ()
respondToChanMsg (SPrivmsg (SUser nick _ _) ch@(RChannel _) text) =
    do count <- getGlobal testCounter
       setGlobal testCounter (count+1)
       sendMessage ch $ "Echoing: " ++ text ++ " - " ++ show (count+1)
       runInS 10 . sendMessage ch $ "delayed action"
       return ()

-- | A variable in the bot's store, keeping track of how many messages
-- we've echoed since startup.
testCounter = GlobalKey (0::Int) "testCounter"

