{-| module: Bot.Scripting.Laws

Auto-enforce rules like not allowing users to have certain characters
in their nick.

-}

module Bot.Scripting.Laws (
    callbacks,
    -- * Forbid characters
    -- ** Settings
    channels, forbiddenChars, graceOffences, graceTime,
    offendersL, exceptionsL, warningMessage,
    -- ** Logic
    listenNick, listenJoin, handleNickEv,
    grace, punish,
    ) where

import Data.List (lookup, intersect)
import Bot

-- | Event hooks
callbacks = [ listenNick
            , listenJoin
            ]

-- | Channels to operate in.
channels = CacheKey [] "LAWS" "channels" :: PersistentKey [Channel]
-- | Characters to forbid users having in their nicks.
forbiddenChars = CacheKey "" "LAWS" "forbiddenChars" :: PersistentKey [Char]
-- | Number of prior offences to allow before instakicking the offender.
graceOffences = CacheKey 0 "LAWS" "graceOffences" :: PersistentKey Int
-- | Time to wait before kicking the offender, if they're in grace.
graceTime = CacheKey 0 "LAWS" "graceTime" :: PersistentKey Int
-- | List of hosts, and numbers of offences.
offendersL = CacheKey [] "LAWS" "offendersL" :: PersistentKey [(String, Int)]
-- | Exception nicks to allow.
exceptionsL = CacheKey [] "LAWS" "exceptionsL" :: PersistentKey [String]
-- | Message to send an offender when in grace.
warningMessage = CacheKey "" "LAWS" "warningMessage" :: PersistentKey String

-- | Listen for a nick change event. These don't come associated with a channel,
-- so we WHOIS lookup the user in question to find all the channels they're in. We then
-- see whether there are any that are in our 'channels' list, calling 'handleNickEv'
-- if so.
listenNick :: SEvent -> Bot ()
listenNick (SNick (SUser nick _ host) newnick) = do
    putLogDebug $ "User " ++ nick ++ "@" ++ host ++ " nicked to " ++ newnick
    getWhois (makeUser newnick) resp
  where resp Nothing = return ()
        resp (Just u@User{statusCharL=ch}) = do
            putLogDebug $ "Whois returned: " ++ show u
            chans <- getGlobal' channels
            let relevantChans = chans `intersect` map fst ch
            mapM_ (\ch -> handleNickEv ch newnick host) relevantChans

-- | Listen for a join event, checking whether the channel is in our 'channels' list and
-- calling 'handleNickEv' if so.
listenJoin (SJoin (SUser nick _ host) ch@(RChannel _)) = do
    putLogDebug $ "User " ++ nick ++ "@" ++ host ++ " joined " ++ show ch
    chans <- getGlobal' channels
    when (fromR ch `elem` chans) $ handleNickEv (fromR ch) nick host

-- | @handleNickEv ch newnick host@: Handle a nick event in the given channel 'ch'.
-- The user with nick 'newnick' and host 'host' has changed nicks or joined the channel.
-- Check if their nick contains a forbidden character, and if so, deal with it - look up
-- the number of offences they've already made (by their host), warning them if they're
-- under the threshold for instakick ('graceOffences') and instakicking them if they're over.
handleNickEv :: Channel -> String -> String -> Bot ()
handleNickEv ch newnick host  = do
    forbid <- getGlobal' forbiddenChars
    exceptions <- getGlobal' exceptionsL
    when ((not . null $ newnick `intersect` forbid) && newnick `notElem` exceptions) $
      do graceN <- getGlobal' graceOffences
         offences <- lookupDefault host 0 <$> getGlobal' offendersL
         modGlobal' offendersL . setAssoc host $ offences + 1
         if offences < graceN then grace ch newnick
         else punish ch newnick
         putLogDebug $ "Nick event in " ++ show ch ++ ": " ++ newnick ++ " " ++ host
         putLogDebug $ "Took action: " ++ show offences ++ " " ++ show graceN

-- | Warn a user, and set a timer to kick them soon.
grace :: Channel -> String -> Bot ()
grace ch nick = do
    msg <- getGlobal' warningMessage
    time <- getGlobal' graceTime
    sendMessage ch $ nick ++ ": " ++ msg ++ " I will kick you in "++ show time ++ " seconds"
                     ++ " if you don't change your nick or leave the channel."
    runInS time $ punish ch nick
    putLogDebug $ "Sending grace to " ++ nick

-- | Kick a user.
punish :: Channel -> String -> Bot ()
punish ch nick = do
    kickUserFor ch (makeUser nick) "Forbidden characters in nick."
    putLogDebug $ "Punishing " ++ nick


lookupDefault k def l = case lookup k l of Just x -> x
                                           Nothing -> def

setAssoc k v l = if (0==) . length . filter ((k==).fst) $ l then l++[(k,v)]
                 else map (\(k',v') -> if k'==k then (k',v) else (k',v')) l
