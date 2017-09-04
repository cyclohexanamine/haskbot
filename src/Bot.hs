{-| Module: Bot

IRC bot interface. Implements some IRC-specific logic for the bot, and
re-exports everything needed to program the bot.
-}


module Bot (
    -- * Bot-specific keys for global store
    currChanList, autoJoinList, nameCallbacks, whoisCallbacks,
    statusChars, statusOpChars,

    -- * IO
    -- ** Messaging
    sendMessage,
    -- * Queries
    getWhois, getWhowas, getNames, getOwnStatus,
    -- * Channel management
    joinChannels, kickUser, kickUserFor, banMask, unbanMask,

    -- * Re-exported
    module Bot.Bot, module Bot.Msg, module Bot.Store,
    ) where

import Data.List (find)

import Bot.Bot
import Bot.Store
import Bot.Msg


-- | Send a textual message to the recipient
sendMessage :: RecipientC a => a -> String -> Bot ()
sendMessage r t = writeMsg $ CMsg PRIVMSG [rs, t]
    where rs = case toR r of RUser s -> s
                             RChannel c -> "#" ++ c

-- | Join the list of channels, where each element is a channel name string (e.g.,
-- @"#channame"@)
joinChannels :: [Channel] -> Bot ()
joinChannels = mapM_ (\c -> writeMsg . CMsg JOIN $ [channelToStr c])

-- | Kick the given user from the given channel, with no reason.
kickUser :: Channel -> User -> Bot ()
kickUser c u = writeMsg $ CMsg KICK [channelToStr c, nick u]

-- | Kick the given user from the given channel, with the reason given.
kickUserFor :: Channel -> User -> String -> Bot ()
kickUserFor c u reason = writeMsg $ CMsg KICK [channelToStr c, nick u, reason]

-- | Ban the given mask from the given channel.
banMask :: Channel -> String -> Bot ()
banMask c mask = writeMsg $ CMsg MODE [channelToStr c, "+b", mask]

-- | Unban the given mask from the given channel.
unbanMask :: Channel -> String -> Bot ()
unbanMask c mask = writeMsg $ CMsg MODE [channelToStr c, "-b", mask]


-- | List of callbacks for active NAME queries, to be invoked when
-- the server replies.
nameCallbacks = GlobalKey [] "nameCallbacks" :: GlobalKey [(String, [User] -> Bot ())]
-- | Make a NAME query (list of nicks in the channel) for the given channel,
-- invoking the callback when the server replies.
getNames :: Channel -> ([User] -> Bot ()) -> Bot ()
getNames c@(Channel ch) cb = do
    modGlobal nameCallbacks (++[(ch, cb)])
    writeMsg $ CMsg NAMES [channelToStr c]

-- | List of callbacks for active WHOIS\/WHOWAS queries, to be invoked when
-- the server replies.
whoisCallbacks = GlobalKey [] "whoisCallbacks" :: GlobalKey [(String, Maybe User -> Bot ())]
-- | Make a WHOIS query for the given user, invoking the callback when
-- the server replies.
getWhois :: User -> (Maybe User -> Bot ()) -> Bot ()
getWhois User{nick=n} cb = do
    modGlobal whoisCallbacks (++[(n, cb)])
    writeMsg $ CMsg WHOIS [n]

-- | Make a WHOWAS query for the given user, invoking the callback when
-- the server replies.
getWhowas :: User -> (Maybe User -> Bot ()) -> Bot ()
getWhowas User{nick=n} cb = do
    modGlobal whoisCallbacks (++[(n, cb)])
    writeMsg $ CMsg WHOWAS [n]

-- | Find the bot's own status character (or Nothing if it doesn't have any)
-- in the given channel, invoking the callback when it finds it.
getOwnStatus :: Channel -> (Maybe Char -> Bot ()) -> Bot ()
getOwnStatus ch cb = do
    ownNick <- getGlobal' botNick
    getNames ch $ \userL ->
        cb $ do User{statusCharL=scl} <- find ((==ownNick).nick) userL
                lookup ch scl


-- Keys for bot things.
-- | Channels the bot is currently in.
currChanList = GlobalKey [] "chanList" :: GlobalKey [Channel]
-- | Channels the bot should autojoin.
autoJoinList = CacheKey [] "BOT" "autoJoinList" :: PersistentKey [Channel]
-- | Character prefixes for users that indicate status in channels; e.g., '@' for op
-- or '+' for voice.
statusChars = CacheKey [] "SERVER" "statusChars" :: PersistentKey [Char]
-- | Status prefixes that indicate op privileges.
statusOpChars = CacheKey [] "SERVER" "statusOpChars" :: PersistentKey [Char]
