{-| Module: Bot.Scripting.Vote

The voting logic.
-}
module Bot.Scripting.Vote (
    callbacks, voteChan, actionSettings, onStartup,
    -- * Vote data
    VoteOpt(..), ActionSetting(..),
    -- * Command handling
    processCommand, parseCommand, validateVote, parseVoteCmd,
    -- * Voting logic
    vote, findTarget, startVote, voteEnd,
    VoteChoice(..), castVote, voteInfo,
    -- * Helpers
    showCurrVote, formatVote,
    ) where

import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Prim (parserFail)

import Bot


-- | Vote options.
data VoteOpt = VoteOpt { actionO :: ActionSetting, targetO :: String
                       , lenO :: Integer, reasonO :: Maybe String
                       , hostO :: Maybe String
                       } deriving (Eq, Read, Show)

-- | Action setting struct - an action is something that can be voted on for
-- the bot to carry out.
data ActionSetting = ActionSetting { actAS :: String -- ^ The name of the action in the !vote command, e.g., "kick".
                                   , actNameAS :: String -- ^ The name of the action as a verb, e.g., "kicked".
                                   , voteLenAS :: Integer -- ^ The length of the vote, in seconds.
                                   , voteThresholdAS :: Integer -- ^ The minimum number of votes that must be cast for the vote to pass.
                                   , useHostAS :: Bool -- ^ Whether the action requires a hostname on the part of the target.
                                   , defaultLenAS :: Integer -- ^ The default duration of the action in seconds, if not specified by a '-l' option.
                                   } deriving (Eq, Read, Show)

-- | Action settings for each action.
actionSettings = PersistentKey [] "VOTE" "actionSettings" :: PersistentKey [ActionSetting]

findAct act = getGlobal' actionSettings >>= return . find ((==act) . actAS)


-- | Event hooks
callbacks = [ processCommand, onStartup ]

-- Persistent values holding information about voting, the current vote, etc.
-- | The channel the bot should take votes in.
voteChan = CacheKey (Channel "") "VOTE" "voteChan"
voteTimer = GlobalKey nilCH "voteTimer" :: GlobalKey CallbackHandle
voteOpts = CacheKey Nothing "VOTE" "voteOpts" :: PersistentKey (Maybe VoteOpt)
yesHosts = CacheKey [] "VOTE" "yesHosts" :: PersistentKey [String]
noHosts = CacheKey [] "VOTE" "noHosts" :: PersistentKey [String]
banTimers = CacheKey [] "VOTE" "banTimers" :: PersistentKey [(UTCTime, Channel, [String])]

commandChar = '!'
-- | The commands a user can give in the channel.
commands = [ ("vote", vote)
           , ("yes", castVote VYes)
           , ("no", castVote VNo)
           , ("voteinfo", voteInfo)
           , ("votehelp", voteHelp)
           ]


-- | Reinstate any outstanding ban callbacks.
-- Also, clear the current vote for dev purposes.
onStartup :: SEvent -> Bot ()
onStartup Startup = do
    getGlobal' banTimers >>= mapM_ (\(t, ch, targs) -> addTimer t (enactUnban ch targs))
    setGlobal' voteOpts Nothing


-- | Listen to all channel messages, and extract those which are
-- commands for this bot.
processCommand :: SEvent -> Bot()
processCommand ev@(SPrivmsg (SUser _ _ _) ch@(RChannel _) text)
    | length text /= 0
    , head text == commandChar
    = do vChan <- getGlobal' voteChan
         if (fromR ch) /= vChan then return ()
         else case parse parseCommand "" . tail $ text of
            Left err -> putLogError $ "Vote command parse error: " ++ show err
            Right act -> act ev
    | otherwise
    = return ()


-- | !vote - command to start a vote.
vote :: SEvent -> Bot ()
vote (SPrivmsg usr ch text) = do
    currOptsMb <- getGlobal' voteOpts
    newOptsMb <- validateVote (fromR ch) text
    if isJust currOptsMb then sendMessage ch "Vote already active"
    else case newOptsMb of
            Just opts -> findTarget opts
            Nothing  -> return ()


-- | Look up the target - a whois\/whowas for kick\/banning, or checking if they're in the channel for a kick.
findTarget :: VoteOpt -> Bot ()
findTarget o@VoteOpt{targetO=targ} =
    if useHostAS . actionO $ o
    then getWhois (makeUser targ) $ \mbUser ->
        case mbUser of
          Just User{host=(Just h)} -> startVote $ o { hostO = Just h }
          _ -> getWhowas (makeUser targ) $ \mbUser ->
            case mbUser of
              Just User{host=(Just h)} -> startVote $ o { hostO = Just h }
              _ -> getGlobal' voteChan >>= \c -> do sendMessage c $ "Can't find target '" ++ targ ++ "'."
    else getGlobal' voteChan >>= \c -> getNames c $ \namesL ->
        if targ `elem` [n | User{nick=n} <- namesL] then startVote o
                              else sendMessage c $ "Target '" ++ targ ++ "' not in channel."


-- | Start the given vote.
startVote :: VoteOpt -> Bot ()
startVote o = do
    ch <- getGlobal' voteChan
    let t = voteLenAS . actionO $ o
    runInS t voteEnd >>= setGlobal voteTimer
    setGlobal' yesHosts []
    setGlobal' noHosts []
    setGlobal' voteOpts $ Just o
    voteStr <- showCurrVote
    sendMessage ch $ "Starting vote. " ++ voteStr
    putLogInfo $ "Starting vote with options " ++ show o


-- | A yes\/no vote.
data VoteChoice = VYes | VNo

-- | !yes or !no - vote yes or no, checking the user's host to see if they've already
-- voted.
castVote :: VoteChoice -> SEvent -> Bot ()
castVote choice (SPrivmsg (SUser nick _ host) ch _) = do
    vChan <- getGlobal' voteChan
    let choices = case choice of VYes -> (yesHosts, noHosts)
                                 VNo -> (noHosts, yesHosts)
    let choiceNames = case choice of VYes -> ("yes", "no")
                                     VNo -> ("no", "yes")
    thisL <- getGlobal' $ fst choices
    otherL <- getGlobal' $ snd choices
    isVote <- getGlobal' voteOpts >>= return . isJust
    if (fromR ch) /= vChan || not isVote then sendMessage ch $ "No vote currently taking place."
    else if host `elem` thisL then sendMessage ch $ "You've already voted "++(fst choiceNames)++
                                                    ", "++nick++". Your vote is unaffected."
    else if host `elem` otherL then do
        setGlobal' (fst choices) $ thisL ++ [host]
        setGlobal' (snd choices) $ delete host otherL
        sendMessage ch $ "You've already voted "++(snd choiceNames)++", "++nick++
                         ". Your vote has been changed to "++(fst choiceNames)++"."
    else do
        setGlobal' (fst choices) $ thisL ++ [host]
        sendMessage ch $ "Your vote has been counted as "++(fst choiceNames)++", "++nick++"."


-- | Send info about the current vote.
voteInfo (SPrivmsg (SUser nick _ host) c@(RChannel ch) _) = do
    (Channel vc) <- getGlobal' voteChan
    optsMb <- getGlobal' voteOpts
    yesL <- getGlobal' yesHosts
    noL <- getGlobal' noHosts
    let countsS = "Votes are " ++ show (length yesL) ++ " for, " ++ show (length noL) ++ " against. "
    voteStr <- showCurrVote
    let infoStr = "Vote currently active. " ++ voteStr ++ countsS
    if ch /= vc then return ()
    else if isNothing optsMb then sendMessage c "No vote currently taking place."
    else sendMessage c infoStr


-- | Gets info about the current vote, including end time.
showCurrVote :: Bot (String)
showCurrVote = do
    optsMb <- getGlobal' voteOpts
    case optsMb of
      Just opts -> do
        endTime <- getGlobal voteTimer >>= getEndTime >>= \x->
            case x of Just t -> return $ formatTime defaultTimeLocale "%c" t
                      Nothing -> return ""
        return $ formatVote opts ++ "Ending " ++ endTime ++ ". "
      Nothing -> return ("")

-- | Pretty print a vote.
formatVote :: VoteOpt -> String
formatVote opts = startS ++ actS ++ reasonS
    where startS = targetO opts ++ " to be "
          actS = actNameAS (actionO opts)
          reasonS = case reasonO opts of Just reason -> " for "++reason++". "
                                         _ -> ". "


-- | End the current vote, taking the action if successful.
voteEnd :: Bot ()
voteEnd = getGlobal' voteOpts >>= \optsMb -> case optsMb of
  Nothing -> return ()
  Just opts -> do
    putLogInfo $ "Ending vote: " ++ show opts
    yesC <- getGlobal' yesHosts >>= return . fromIntegral . length
    noC <- getGlobal' noHosts >>= return . fromIntegral . length
    ch <- getGlobal' voteChan
    let act = actionO opts
    setGlobal' voteOpts Nothing
    if yesC + noC < (voteThresholdAS act)
    then sendMessage ch $ "Not enough votes were cast; needed "++show (voteThresholdAS act)++" for a "++
                          actAS act++" vote but got "++show (yesC + noC)++"."
    else if noC > yesC
    then sendMessage ch $ "Vote failed - the votes were "++show yesC++" for and "++show noC++" against."++
                          "No action will be taken."
    else if noC == yesC
    then sendMessage ch $ "Vote tied - the votes were "++show yesC++" each. No action will be taken."
    else do sendMessage ch $ "Vote successful. "++formatVote opts
            enactVote opts

-- | Carry out the vote action.
enactVote :: VoteOpt -> Bot ()
enactVote opts = do
    ch <- getGlobal' voteChan
    opchs <- getGlobal' statusOpChars
    let reasonStr = "Voted" ++ case reasonO opts of Just s -> ": " ++ s
                                                    Nothing -> "."
    getOwnStatus ch $ \resp ->
        if isNothing resp || not ((fromJust resp) `elem` opchs)
        then sendMessage ch "Need op privileges to carry out the action."
        else case actAS . actionO $ opts of
            "kick" -> kickUserFor ch (makeUser . targetO $ opts) reasonStr
            "ban" -> do mapM_ (banMask ch) targetMasks
                        timeMb <- runInS (lenO opts) (enactUnban ch targetMasks) >>= getEndTime
                        case timeMb of Just t -> modGlobal' banTimers (++[(t, ch, targetMasks)])
                                       Nothing -> return ()
                        kickUserFor ch (makeUser . targetO $ opts) reasonStr
            "permaban" -> do mapM_ (banMask ch) targetMasks
                             kickUserFor ch (makeUser . targetO $ opts) reasonStr
            "unban" -> mapM_ (unbanMask ch) targetMasks
  where targetMasks = [targetO opts++"!*@*"] ++ case hostO opts of Just h  -> ["*!*@"++h]
                                                                   Nothing -> []

enactUnban :: Channel -> [String] -> Bot ()
enactUnban ch targetMasks = do
    opchs <- getGlobal' statusOpChars
    getOwnStatus ch $ \resp ->
        if isNothing resp || not ((fromJust resp) `elem` opchs)
        then sendMessage ch $ "The following masks should be unbanned now, but I don't have op privileges: "++
                               intercalate ", " targetMasks
        else do mapM_ (unbanMask ch) targetMasks
                modGlobal' banTimers $ filter (\(_, ch', targetMasks') -> ch/=ch' || targetMasks/=targetMasks')


-- | Show a help message giving some details about voting.
voteHelp :: SEvent -> Bot ()
voteHelp (SPrivmsg (SUser nick _ host) ch _) = do
    kickA <- findAct "kick" >>= return . fromJust
    banA <- findAct "ban" >>= return . fromJust
    permabanA <- findAct "permaban" >>= return . fromJust
    unbanA <- findAct "unban" >>= return . fromJust
    sendMessage (makeUser nick) $ "I am a democratic moderation bot. To start a vote, say !vote action target reason. The reason is optional, and can be more than one word as long as it does not contain hyphens. The options for action are 'kick', 'ban', 'permaban' and 'unban'. To manually specify the length of a ban, append -l length, where length is the length of the ban in hours. The default length is "++show((defaultLenAS banA)`div`3600)++" hours."
    sendMessage (makeUser nick) $ "To vote yes, say !yes and to vote no, say !no. I will automatically get the host of the target given their exact nick, but I cannot do this for people who have not been on the server recently - a few hours."
    sendMessage (makeUser nick) $ "The minimum number of votes is "++show(voteThresholdAS kickA)++" for a kick, "++show(voteThresholdAS banA)++" for bans, "++show(voteThresholdAS permabanA)++" for permabans, "++show(voteThresholdAS unbanA)++" for unbans; the duration of the vote in minutes is "++show((voteLenAS kickA)`div`60)++" for a kick, "++show((voteLenAS banA)`div`60)++" for bans, "++show((voteLenAS permabanA)`div`60)++" for permabans, "++show((voteLenAS unbanA)`div`60)++" for unbans. If there is a tie I will remove the last vote. To see information about the current vote, say '!voteinfo'."


-- Command parsing

parseUntil' s = do x <- parseUntil s
                   many $ oneOf s
                   return x

-- | Parse the top-level command, returning an action to take.
parseCommand :: Parser (SEvent -> Bot ())
parseCommand = do cmd <- parseUntil' " "
                  if cmd == ""
                  then return $ \ev -> return ()
                  else case lookup cmd commands of
                         Just act -> return act
                         Nothing -> return $ \ev -> do
                            vc <- getGlobal' voteChan
                            sendMessage vc $ "Invalid vote command '" ++ cmd ++ "'"

-- | Parser for a !vote command, returning parsed parameters if successful, or
-- an error message to send to the channel if not.
parseVoteCmd :: Parser (Either String (String, String, Maybe Integer, Maybe String))
parseVoteCmd = do start <- parseUntil' " "
                  action <- optionMaybe $ parseUntil' " "
                  target <- optionMaybe $ parseUntil' " "
                  reason <- optionMaybe $ parseUntil' "-" >>= return . rstrip
                  len <- optionMaybe $ do optionMaybe $ char '-'
                                          char 'l'
                                          optionMaybe $ char ' '
                                          decimal >>= return . (*3600)
                  if start /= "!vote"
                  then parserFail $ "Parse fail in parseVoteCmd - " ++ start
                  else if isNothing action
                  then return $ Left "Need an action"
                  else if isNothing target
                  then return $ Left "Need a target."
                  else return $ Right (fromJust action, fromJust target, len,  reason)

-- | Parse a !vote command, sending the appropriate error messages, and returning
-- the vote options if the command was valid, or Nothing if not.
validateVote :: Channel -> String -> Bot (Maybe VoteOpt)
validateVote ch text = do
    case parse parseVoteCmd "" text of
        Left err -> putLogError (show err) >> return Nothing
        Right (Left msg) -> sendMessage ch msg >> return Nothing
        Right (Right (act, targ, lenMb, reasonMb)) -> do
            actSettingMb <- findAct act
            case actSettingMb of
                Nothing -> sendMessage ch ("'"++act++"' is not a valid action.") >> return Nothing
                Just actSetting ->
                    let len = case lenMb of Nothing -> defaultLenAS actSetting
                                            Just l -> l
                    in return . Just $ VoteOpt actSetting targ len reasonMb Nothing
