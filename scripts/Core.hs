{-| module: Scripts.Core

Implements the core IRC-related functionality, like user registration,
responding to pings, joining and leaving channels, and so on.

-}

module Scripts.Core (
    -- * Connection
    connected, respondTo376,
    -- * Other
    respondToPing,
    ) where
import Bot


handle376 = GlobalKey undefined "handle376" :: GlobalKey CallbackHandle

-- | User registration, and set a callback to join channels.
connected :: SEvent -> Bot ()
connected (Connected) = do
    nick <- getGlobal' botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]
    addCallback respondTo376 >>= setGlobal handle376

-- | Join channels on 376 (end of MOTD), and then unhook this callback.
respondTo376 :: SEvent -> Bot ()
respondTo376 (SNumeric _ 376 _) = do
    getGlobal handle376 >>= removeCallback
    chans <- getGlobal' autoJoinList
    if length chans == 0 then return ()
    else joinChannels ["#"++c | RChannel c <- chans]

-- | Respond to a server PING with PONG.
respondToPing :: SEvent -> Bot ()
respondToPing (SPing srv) = writeMsg $ CMsg PONG [srv]


-- Channel management
currChanList = GlobalKey [] "chanList" :: GlobalKey [Recipient]
autoJoinList = CacheKey [] "BOT" "autoJoinList" :: PersistentKey [Recipient]

-- respondToJoin (SJoin ch) = 

