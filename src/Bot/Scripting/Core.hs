{-| module: Bot.Scripting.Core

Implements the core IRC-related functionality, like user registration,
responding to pings, joining and leaving channels, and so on.

-}

module Bot.Scripting.Core (
    callbacks,
    -- * Connection
    connected, disconnected, manageReadyStatus,
    respondToEndOfMOTD,
    managePingTimer, pingTimeoutLen,
    pingTimer, respondToPong, pingTimeout,
    -- * Queries
    respondToNAMES, respondToWHOISUSER, respondToWHOWASUSER,
    namesResponse, whoisResponse,
    -- * Nick management
    nickservAddr, acquiredNick, nickservIdent, identPassword, identSuccess,
    requestOp,
    -- * Channel management
    chanservAddr, respondToJoin, respondToPart, respondToKick, unableToJoin,
    unableToJoinChan, opOnJoin,
    -- * Other
    respondToPing,
    ) where
import Bot

import Control.Concurrent (forkIO, threadDelay, killThread)
import Data.List (delete, (\\))
import Data.List.Split (splitOn)


-- | Event hooks
callbacks = [ connected
            , disconnected
            , manageReadyStatus

            , respondToPing

            , managePingTimer
            , respondToPong

            , acquiredNick
            , identSuccess

            , respondToJoin
            , respondToPart
            , respondToKick
            , unableToJoin
            , opOnJoin

            , respondToNAMES
            , respondToWHOISUSER
            , respondToWHOWASUSER
            ]


-- Connection management

handle376 = GlobalKey nilCH "handle376" :: GlobalKey CallbackHandle

-- | User registration, and set a callback to join channels.
connected :: SEvent -> Bot ()
connected (Connected) = do
    putLogInfo "Connected to server."
    nick <- getGlobal' botNick
    writeMsg $ CMsg NICK [nick]
    writeMsg $ CMsg USER [nick, "0", "*" , nick]
    addCallback respondToEndOfMOTD >>= setGlobal handle376

-- | Remove all the channels from the current channel list.
disconnected :: SEvent -> Bot ()
disconnected (Disconnected) = do
    putLogInfo "Disconnected from server."
    setGlobal currChanList []
    signalEvent UnReady

-- | Keep track of ready\/unready status.
manageReadyStatus (Ready) = do
    putLogInfo "Now ready."
    setGlobal readyStatus True
manageReadyStatus (UnReady) = do
    putLogInfo "Now unready."
    setGlobal readyStatus False


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

-- | When we successfully identify for our nick, we want to request
-- op from ChanServ, in case any ops depended on being identified.
identSuccess :: SEvent -> Bot ()
identSuccess (SMode _ (RUser nick) [(True, 'r', [])]) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else requestOp

-- | The address of the ChanServ service on the server.
chanservAddr = CacheKey (SServer "") "SERVER" "chanserv"
-- | Request op in all channels from ChanServ.
requestOp = do
    cs <- getGlobal' chanservAddr
    case cs of ss@SUser{} -> sendMessage (fromS ss :: User) "OP"
               _ -> return ()


-- Channel management

-- | If it's the bot that's joined, update the current channels list. If
-- we just joined the last remaining channel on the autojoin list, signal 'Ready'.
respondToJoin (SJoin (SUser nick _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else do modGlobal currChanList (++[fromR ch])
            putLogInfo ("Joined " ++ show ch)
            currChans <- getGlobal currChanList
            autojoin <- getGlobal' autoJoinList
            if (fromR ch) `elem` autojoin && autojoin \\ currChans == []
              then signalEvent Ready
              else return ()

-- | If it's the bot that's parted, update the current channels list.
respondToPart (SPart (SUser nick _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if nick /= ownNick then return ()
    else modGlobal currChanList (delete (fromR ch))
         >> putLogInfo ("Parted " ++ show ch)

-- | If it's the bot that's been kicked, update the current channels list,
-- and rejoin the channel after a delay. If this was an autojoin channel,
-- signal UnReady.
respondToKick e@(SKick (SUser nick _ _) ch@(RChannel _) target reason) = do
    ownNick <- getGlobal' botNick
    if target /= ownNick then return ()
    else do modGlobal currChanList (delete (fromR ch))
            putLogInfo ("Kicked from "++(show ch)++" by " ++ nick ++ ": " ++ reason)
            autojoin <- getGlobal' autoJoinList
            if (fromR ch) `elem` autojoin
              then signalEvent UnReady
              else return ()
            runInSPriority 5 $ joinChannels [fromR ch]

-- | If we're unable to join a channel, we want to record why, and then
-- retry joining in 10 seconds.
unableToJoinChan chstr n = do
    putLogWarning $ "Unable to join channel " ++ chstr ++ " - ERR "
                    ++ show n ++ ". Retrying."
    runInSPriority 10 $ joinChannels [makeChannel chstr]

-- | Catch all the cases when we're unable to join a channel.
unableToJoin (SNumeric _ n@471 (_:st:_)) = unableToJoinChan st n
unableToJoin (SNumeric _ n@473 (_:st:_)) = unableToJoinChan st n
unableToJoin (SNumeric _ n@474 (_:st:_)) = unableToJoinChan st n
unableToJoin (SNumeric _ n@475 (_:st:_)) = unableToJoinChan st n

-- | Try to get op from ChanServ when we join a channel.
opOnJoin (SJoin (SUser n _ _) ch@(RChannel _)) = do
    ownNick <- getGlobal' botNick
    if n /= ownNick then return ()
    else requestOp



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
    cbs <- consumeGlobal nameCallbacks (\(c, cb) -> "#"++c == ch)
    mapM_ (($cL) . snd) cbs


whoisTempReplies = GlobalKey [] "whoisTempReplies" :: GlobalKey [(String, User)]

-- | WHOIS reply - like 'respondToNAMES', this handles RPL_WHOISUSER (311), RPL_WHOISCHANNELS
-- (319) and RPL_ENDOFWHOIS (318). We keep a list of intermediate responses which we update
-- when we get a 311 and 319, and invoke the callbacks on whatever we've stored when
-- a 318 arrives.
respondToWHOISUSER (SNumeric _ 311 [_,n,u,h,_,realName]) =
    modGlobal whoisTempReplies (++[(n, makeUserH n u h)])
respondToWHOISUSER (SNumeric _ 319 [_,n,chanL]) = do
    chrs <- getGlobal' statusChars
    let toSC (x:xs) = if x `elem` chrs then (makeChannel xs,x) else (makeChannel $ x:xs,' ')
        sCL = map toSC . splitOn " " $ chanL
    modGlobal whoisTempReplies $ map (\(n',u) -> if n'==n then (n', u {statusCharL=sCL}) else (n',u))
respondToWHOISUSER (SNumeric _ 318 [_,n,_]) = do
    tr <- consumeGlobal whoisTempReplies ((n==).fst)
    if length tr == 0 then whoisResponse n Nothing
    else mapM_ (\(n,u) -> whoisResponse n (Just u)) tr

-- | WHOWAS reply - like 'respondToNAMES', this handles both RPL_WHOWASUSER (314)
-- and RPL_ENDOFWHOWAS (369). Handle and . Unlike WHOIS, we're never going to get a
-- list of channels, so we don't need to store any intermediate callbacks.
respondToWHOWASUSER (SNumeric _ 314 [_,n,u,h,_,realName]) = whoisResponse n . Just $ makeUserH n u h
respondToWHOWASUSER (SNumeric _ 369 [_,n,_]) = whoisResponse n Nothing

-- | Find the requester for the WHOIS\/WHOWAS query, unhook and invoke it.
whoisResponse :: String -> Maybe User -> Bot ()
whoisResponse n userMb = do
    putLogDebug $ "Whois response: " ++ show n ++ " - " ++ show userMb
    cbs <- getGlobal whoisCallbacks
    let shouldRun = [t | t@(u, cb) <- cbs, u == n]
    let shouldKeep = [t | t@(u, cb) <- cbs, u /= n]
    setGlobal whoisCallbacks shouldKeep
    mapM_ (($userMb) . snd) shouldRun
