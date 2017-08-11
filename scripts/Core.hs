{-| module: Scripts.Core

Implements the core IRC-related functionality, like user registration,
responding to pings, joining and leaving channels, and so on.

-}

module Scripts.Core (
    callbacks,
    -- * Connection
    connected, respondTo376,
    -- * Channel management
    respondToJoin, respondToPart, respondToKick,
    -- * Other
    respondToPing,
    ) where
import Bot

import Data.List (delete)

-- | Event hooks
callbacks = [ connected
            , respondTo376
            
            , respondToPing
            
            , respondToJoin
            , respondToPart
            , respondToKick
            ]



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

-- | If it's the bot that's joined, update the current channels list.
respondToJoin (SJoin (SUser nick _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else modGlobal currChanList (++[ch]) 
         >> putLogInfo ("Joined " ++ show ch)
    
-- | If it's the bot that's parted, update the current channels list.
respondToPart (SPart (SUser nick _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else  modGlobal currChanList (delete ch)
         >> putLogInfo ("Parted " ++ show ch)

-- | If it's the bot that's been kicked, update the current channels list.
respondToKick e@(SKick (SUser nick _ _) ch@(RChannel _) target reason) = do
    ownNick <- getGlobal' botNick
    if target /= ownNick then return ()
    else modGlobal currChanList (delete ch)
         >> putLogInfo ("Kicked from "++(show ch)++" by " ++ nick ++ ": " ++ reason)
