{-| module: Bot.Scripting.Core

Implements the core IRC-related functionality, like user registration,
responding to pings, joining and leaving channels, and so on.

-}

module Bot.Scripting.Core (
    callbacks,
    -- * Connection
    connected, respondToEndOfMOTD,
    managePingTimer, pingTimeoutLen,
    pingTimer, respondToPong, pingTimeout,
    -- * Queries
    respondToNAMES, respondToWHOISUSER,
    namesResponse, whoisResponse,
    -- * Nick management
    acquiredNick, nickservIdent,
    -- * Channel management
    respondToJoin, respondToPart, respondToKick,
    -- * Other
    respondToPing,
    ) where
import Bot

import Control.Concurrent (forkIO, threadDelay, killThread)
import Data.List (delete)
import Data.List.Split (splitOn)


-- | Event hooks
callbacks = [ connected

            , respondToPing

            , managePingTimer
            , respondToPong

            , acquiredNick

            , respondToJoin
            , respondToPart
            , respondToKick

            , respondToNAMES
            , respondToWHOISUSER
            ]


-- Connection management

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


pingTimerHandle = GlobalKey nilCH "pingTimerHandle"
pingTimeoutHandle = GlobalKey nilCH "pingTimeoutHandle"
pingN = GlobalKey "" "pingN" :: GlobalKey String
-- | How long we should wait without receiving a ping response before
-- reconnecting to the server. Pings will be sent at intervals of a third
-- of this.
pingTimeoutLen = CacheKey 60 "BOT" "pingTimeoutLen" :: PersistentKey Integer

-- | Register or unregister the ping timers when we connect/disconnect.
-- Every once in a while we'll send a ping to the server, to see if the
-- connection is still active. We have a timeout timer, 'pingTimeout', that
-- will force a reconnect if it expires. When we receive ping response, we
-- reset the timer, so that it's only triggered if we don't receive a timely
-- response.
managePingTimer :: SEvent -> Bot ()
managePingTimer (Connected) = do
    tm <- getGlobal' pingTimeoutLen
    runInS tm pingTimeout >>= setGlobal pingTimeoutHandle
    runInS (div tm 3) pingTimer >>= setGlobal pingTimerHandle
managePingTimer (Disconnected) = do
    getGlobal pingTimerHandle >>= removeTimer
    getGlobal pingTimeoutHandle >>= removeTimer

-- | Send a ping to the server, every pingTimeoutLen/3 seconds.
pingTimer :: Bot ()
pingTimer = do
    nick <- getGlobal' botNick
    writeMsg $ CMsg PING [nick]
    tm <- getGlobal' pingTimeoutLen
    runInS (div tm 3) pingTimer >>= setGlobal pingTimerHandle

-- | Receive a ping reply from the server and reset the timeout timer.
respondToPong :: SEvent -> Bot ()
respondToPong (SPong _ _) = do
    getGlobal pingTimeoutHandle >>= removeTimer
    tm <- getGlobal' pingTimeoutLen
    runInS tm pingTimeout >>= setGlobal pingTimeoutHandle

-- | Disconnect from the server by any means by killing the listener thread.
-- This will force a disconnect/reconnect in the main event thread.
pingTimeout :: Bot ()
pingTimeout = do
    tm <- getGlobal' pingTimeoutLen
    putLogInfo $ "Ping timeout: " ++ show tm ++ " seconds."
    getGlobal listenThread >>= liftIO . killThread


-- Nick management
-- | The password to use to identify the nickname with NickServ.
identPassword = CacheKey "" "BOT" "identPassword"
-- | The address of the NickServ service on the server.
nickservAddr = CacheKey (SServer "") "SERVER" "nickserv"

handleNickservNotice = GlobalKey nilCH "handleNickservNotice" :: GlobalKey CallbackHandle

-- | On '001', i.e., getting assigned a nick, start listening for a message from NickServ
-- in case we have to identify for it.
acquiredNick :: SEvent -> Bot ()
acquiredNick (SNumeric _ 001 _) = addCallback nickservIdent >>= setGlobal handleNickservNotice

-- | On the first message from NickServ, whatever it is, identify for our nick.
nickservIdent :: SEvent -> Bot ()
nickservIdent (SNotice sender@(SUser _ _ _) _ _) = do
    nsAddr <- getGlobal' nickservAddr
    nsPass <- getGlobal' identPassword
    if sender /= nsAddr || nsAddr == SServer "" || nsPass == "" then return ()
    else do
        getGlobal handleNickservNotice >>= removeCallback
        sendMessage (fromS sender :: User) ("IDENTIFY "++nsPass)



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
