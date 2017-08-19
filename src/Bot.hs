{-| Module: Bot

IRC bot interface. Implements some IRC-specific logic for the bot, and
re-exports everything needed to program the bot.
-}


module Bot (
    -- * Bot-specific keys for global store
    currChanList, autoJoinList, nameCallbacks, whoisCallbacks,

    -- * IO
    -- ** Messaging
    sendMessage,
    -- * Queries
    getWhois, getWhowas, getNames,
    -- * Channel management
    joinChannels, kickUser, kickUserFor,

    -- * Re-exported
    module Bot.Bot, module Bot.Msg, module Bot.Store,
    ) where

import Bot.Bot
import Bot.Store
import Bot.Msg


-- | Send a textual message to the recipient
sendMessage :: RecipientC a => a -> String -> Bot ()
sendMessage r t = writeMsg $ CMsg PRIVMSG [rs, t]
    where rs = case (toR r) of RUser s -> s
                               RChannel c -> "#" ++ c

-- | Join the list of channels, where each element is a channel name string (e.g.,
-- @"#channame"@)
joinChannels :: [Channel] -> Bot ()
joinChannels = mapM_ (\(Channel c) -> writeMsg . CMsg JOIN $ ["#"++c])

-- | Kick the given user from the given channel, with no reason.
kickUser :: Channel -> User -> Bot ()
kickUser (Channel c) u = writeMsg $ CMsg KICK ["#"++c, nick u]

-- | Kick the given user from the given channel, with the reason given.
kickUserFor :: Channel -> User -> String -> Bot ()
kickUserFor (Channel c) u reason = writeMsg $ CMsg KICK ["#"++c, nick u, reason]


-- | List of callbacks for active NAME queries, to be invoked when
-- the server replies.
nameCallbacks = GlobalKey [] "nameCallbacks" :: GlobalKey [(String, [User] -> Bot ())]
-- | Make a NAME query (list of nicks in the channel) for the given channel,
-- invoking the callback when the server replies.
getNames :: Channel -> ([User] -> Bot ()) -> Bot ()
getNames (Channel ch) cb = do
    modGlobal nameCallbacks (++[(ch, cb)])
    writeMsg $ CMsg NAMES ["#"++ch]

-- | List of callbacks for active WHOIS\/WHOWAS queries, to be invoked when
-- the server replies.
whoisCallbacks = GlobalKey [] "whoisCallbacks" :: GlobalKey [(String, Maybe User -> Bot ())]
-- | Make a WHOIS query for the given user, invoking the callback when
-- the server replies.
getWhois :: User -> (Maybe User -> Bot ()) -> Bot ()
getWhois (User nick _ _) cb = do
    modGlobal whoisCallbacks (++[(nick, cb)])
    writeMsg $ CMsg WHOIS [nick]

-- | Make a WHOWAS query for the given user, invoking the callback when
-- the server replies.
getWhowas :: User -> (Maybe User -> Bot ()) -> Bot ()
getWhowas (User nick _ _) cb = do
    modGlobal whoisCallbacks (++[(nick, cb)])
    writeMsg $ CMsg WHOWAS [nick]



-- Keys for bot things.
-- | Channels the bot is currently in.
currChanList = GlobalKey [] "chanList" :: GlobalKey [Channel]
-- | Channels the bot should autojoin.
autoJoinList = CacheKey [] "BOT" "autoJoinList" :: PersistentKey [Channel]

