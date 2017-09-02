{-| module: Bot.Scripting.Flood

Flood control. There are three parameters: 'timeWindow', 'linesWeight',
'charsWeight'. The bot will kick anyone who, in a single channel, posts enough
messages within 'timeWindow' seconds such that @number-of-lines * linesWeight
 + number-of-characters * charsWeight > 1@.

e.g., if you want the bot to kick anyone who sends more then 6 lines in a
5 second window, set @timeWindow = 5@, @linesWeight = 0.1667 (1\/6)@,
@charsWeight = 0.0@.

The bot will only do this in 'channels'. This also handles join flooding,
by treating a join message as equivalent to 

-}

module Bot.Scripting.Flood (
    -- * Settings
    channels, timeWindow, linesWeight, charsWeight,
    -- * Flood control
    MessageMap, trackMessage, cleanLines,
    addLine, isFlooding, stopFlooding,
    callbacks,
    ) where

import qualified Data.Map.Lazy as M (Map, findWithDefault, adjust, alter, empty)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)

import Bot


-- | Event hooks
callbacks = [ trackMessage ]

channels = CacheKey [] "FLOOD" "channels" :: PersistentKey [Channel]
linesWeight = CacheKey 0.0 "FLOOD" "linesWeight" :: PersistentKey Float
charsWeight = CacheKey 0.0 "FLOOD" "charsWeight" :: PersistentKey Float
joinWeight = CacheKey 0.0 "FLOOD" "joinWeight" :: PersistentKey Float
timeWindow = CacheKey 0 "FLOOD" "timeWindow" :: PersistentKey Int

-- | Message history is stored per user per channel, in a 'Map' with
-- keys @(Channel, User)@. The values are lists of @(time message was sent,
-- (how many lines the message counts as, number of characters in message))@
-- for each message sent by that user to that channel and received by the 
-- bot within 'timeWindow'; although these values are in general floats.
type MessageMap = M.Map (Channel, User) [(UTCTime, (Float, Float))]
messageMap = GlobalKey M.empty "messageMap" :: GlobalKey MessageMap

-- | Main event hook. Captures various channel events and weights them
-- accordingly, passing them on to 'updateUserMessage'. 
trackMessage :: SEvent -> Bot ()
trackMessage (SPrivmsg u@(SUser _ _ _) ch@(RChannel _) txt) =
    updateUserMessage (fromS u) (fromR ch) (1.0, fromIntegral . length $ txt)
trackMessage (SJoin u@(SUser _ _ _) ch@(RChannel _)) = do
    w <- getGlobal' joinWeight
    updateUserMessage (fromS u) (fromR ch) (w, 0.0)

-- Update a user's message history, check if they're flooding, and kick
-- them if they are.
updateUserMessage :: User -> Channel -> (Float, Float) -> Bot ()
updateUserMessage u ch n = do
    let k = (ch, u)
    chans <- getGlobal' channels
    if ch `elem` chans
      then do cleanLines k
              addLine k n
              flooding <- isFlooding k
              if flooding then stopFlooding k else return ()
      else return ()

-- | Remove any logs outside the time window from the user's message history.
cleanLines :: (Channel, User) -> Bot ()
cleanLines k = do
    t <- liftIO getCurrentTime
    delta <- getGlobal' timeWindow
    let f l = [(t',n) | (t',n) <- l, addUTCTime (fromIntegral delta) t' >= t ]
    modGlobal messageMap (M.adjust f k)

-- | Add the given line to the user's message history.
addLine :: (Channel, User) -> (Float, Float) -> Bot ()
addLine k n = do
    t <- liftIO getCurrentTime
    let f Nothing = Just [(t, n)]
        f (Just l) = Just $ l ++ [(t, n)]
    modGlobal messageMap (M.alter f k)

-- | Check whether the user is flooding given their message history.
isFlooding :: (Channel, User) -> Bot Bool
isFlooding k = do
    m <- getGlobal messageMap
    lW <- getGlobal' linesWeight
    cW <- getGlobal' charsWeight
    let ll = M.findWithDefault [] k m
        tupleAdd (a,b) (c,d) = (a+c, b+d)
        (lines, chars) = foldl (tupleAdd) (0.0,0.0) . map snd $ ll 
        score = lW * lines + cW * chars
    putLogAll $ "Flood score for " ++ show k ++ " - " ++ show score
    if score > 1.0 then putLogDebug $"Flood detected: " ++ show [lW, cW] 
                                     ++ show (lines, chars)
                                     ++ " Message history: " ++ show m
                   else return ()
    return $ score > 1.0

-- | Kick a user for flooding.
stopFlooding :: (Channel, User) -> Bot ()
stopFlooding (ch, u) = kickUserFor ch u "Flooding"

