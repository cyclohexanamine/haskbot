{-| module: Scripts.Core

Implements the core IRC-related functionality, like user registration,
responding to pings, joining and leaving channels, and so on.

-}

module Scripts.Core (
    -- * Startup
    startup, respondTo376,
    -- * Other
    respondToPing,
    ) where
import Bot


handle376 = GlobalKey undefined "handle376" :: GlobalKey CallbackHandle

-- | User registration, and set a callback to join channels.
startup :: SEvent -> Bot ()
startup (Startup) = do
    nick <- getGlobal' botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]
    addCallback respondTo376 >>= setGlobal handle376

-- | Join channels on 376 (end of MOTD), and then unhook this callback.
respondTo376 :: SEvent -> Bot ()
respondTo376 (SNumeric _ 376 _) = do
    chan <- getGlobal' botChan
    joinChannels [chan]
    getGlobal handle376 >>= removeCallback

-- | Respond to a server PING with PONG.
respondToPing :: SEvent -> Bot ()
respondToPing (SPing srv) = writeMsg $ CMsg PONG [srv]
