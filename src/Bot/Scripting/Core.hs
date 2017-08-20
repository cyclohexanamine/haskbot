{-| module: Bot.Scripting.Core

Implements the core IRC-related functionality, like user registration,
responding to pings, joining and leaving channels, and so on.

-}

module Bot.Scripting.Core (
    callbacks,
    -- * Connection
    connected, respondToEndOfMOTD,
    -- * Queries
    respondToNAMES, respondToWHOISUSER,
    namesResponse, whoisResponse,
    -- * Channel management
    respondToJoin, respondToPart, respondToKick,
    -- * Other
    respondToPing,
    ) where
import Bot

import Data.List (delete)
import Data.List.Split (splitOn)

-- | Event hooks
callbacks = [ connected

            , respondToPing

            , respondToJoin
            , respondToPart
            , respondToKick

            , respondToNAMES
            , respondToWHOISUSER
            ]



handle376 = GlobalKey nilCH "handle376" :: GlobalKey CallbackHandle

-- | User registration, and set a callback to join channels.
connected :: SEvent -> Bot ()
connected (Connected) = do
    nick <- getGlobal' botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]
    addCallback respondToEndOfMOTD >>= setGlobal handle376

-- | Join channels on 376 (end of MOTD), and then unhook this callback.
respondToEndOfMOTD :: SEvent -> Bot ()
respondToEndOfMOTD (SNumeric _ 376 _) = do
    getGlobal handle376 >>= removeCallback
    chans <- getGlobal' autoJoinList
    if length chans == 0 then return ()
    else joinChannels chans

-- | Respond to a server PING with PONG.
respondToPing :: SEvent -> Bot ()
respondToPing (SPing srv) = writeMsg $ CMsg PONG [srv]


-- Channel management

-- | If it's the bot that's joined, update the current channels list.
respondToJoin (SJoin (SUser nick _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else modGlobal currChanList (++[fromR ch])
         >> putLogInfo ("Joined " ++ show ch)

-- | If it's the bot that's parted, update the current channels list.
respondToPart (SPart (SUser nick _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else modGlobal currChanList (delete (fromR ch))
         >> putLogInfo ("Parted " ++ show ch)

-- | If it's the bot that's been kicked, update the current channels list.
respondToKick e@(SKick (SUser nick _ _) ch@(RChannel _) target reason) = do
    ownNick <- getGlobal' botNick
    if target /= ownNick then return ()
    else modGlobal currChanList (delete (fromR ch))
         >> putLogInfo ("Kicked from "++(show ch)++" by " ++ nick ++ ": " ++ reason)


-- | NAMES reply - handles both RPL_NAMREPLY (353) and RPL_ENDOFNAMES (366);
-- the requester will be called on 353 and unhooked; or if, e.g., the channel
-- doesn't exist, the server will only send a 366 and the requester will be
-- called on that.
respondToNAMES (SNumeric _ 353 ([_,_,ch,names])) = do
    chrs <- getGlobal' statusChars
    let toUser (x:xs) = if x `elem` chrs 
                          then (makeUser xs){statusCharL=[(makeChannel ch, x)]} 
                          else makeUser (x:xs)
    let namesL = map toUser . splitOn " " $ names
    namesResponse ch namesL

respondToNAMES (SNumeric _ 366 [_,_,ch,_]) = namesResponse ch []

-- | Find the requester for the NAMES query, unhook and invoke it.
namesResponse :: String -> [User] -> Bot ()
namesResponse ch cL = do
    cbs <- getGlobal nameCallbacks
    let shouldRun = [t | t@(c, cb) <- cbs, "#"++c == ch]
    let shouldKeep = [t | t@(c, cb) <- cbs, "#"++c /= ch]
    setGlobal nameCallbacks shouldKeep
    mapM_ (($cL) . snd) shouldRun

-- | WHOIS reply - like 'respondToNAMES', this handles both RPL_WHOISUSER (311)
-- and RPL_ENDOFWHOIS (318). It also handles the equivalent WHOWAS responses,
-- since they're virtually identical - RPL_WHOWASUSER (314) and RPL_ENDOFWHOWAS
-- (369).
respondToWHOISUSER (SNumeric _ 311 [_,n,u,h,_,realName]) = whoisResponse n . Just $ makeUserH n u h
respondToWHOISUSER (SNumeric _ 318 [_,n,_]) = whoisResponse n Nothing
respondToWHOISUSER (SNumeric _ 314 [_,n,u,h,_,realName]) = whoisResponse n . Just $ makeUserH n u h
respondToWHOISUSER (SNumeric _ 369 [_,n,_]) = whoisResponse n Nothing

-- | Find the requester for the WHOIS\/WHOWAS query, unhook and invoke it.
whoisResponse :: String -> Maybe User -> Bot ()
whoisResponse n userMb = do
    cbs <- getGlobal whoisCallbacks
    let shouldRun = [t | t@(u, cb) <- cbs, u == n]
    let shouldKeep = [t | t@(u, cb) <- cbs, u /= n]
    setGlobal whoisCallbacks shouldKeep
    mapM_ (($userMb) . snd) shouldRun
