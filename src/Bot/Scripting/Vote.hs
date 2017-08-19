{-| Module: Bot.Scripting.Vote

The voting logic.
-}
module Bot.Scripting.Vote (
    callbacks, voteChan, actionSettings,
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

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Prim (parserFail)
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Data.Time.Format

import Bot


-- | Vote options.
data VoteOpt = VoteOpt { actionO :: ActionSetting, targetO :: String
                       , lenO :: Either Char Integer, reasonO :: Maybe String
                       , hostO :: Maybe String }
    deriving (Eq, Read, Show)

-- | Action setting struct - an action is something that can be voted on for
-- the bot to carry out.
data ActionSetting = ActionSetting { actAS :: String -- ^ The name of the action in the !vote command, e.g., "kick".
                                   , actNameAS :: String -- ^ The name of the action as a verb, e.g., "kicked".
                                   , voteLenAS :: Integer -- ^ The length of the vote, in seconds.
                                   , voteThresholdAS :: Integer -- ^ The minimum number of votes that must be cast for the vote to pass.
                                   , useHostAS :: Bool -- ^ Whether the action requires a hostname on the part of the target.
                                   }
    deriving (Eq, Read, Show)

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

commandChar = '!'
-- | The commands a user can give in the channel.
commands = [ ("vote", vote)
           , ("yes", castVote VYes)
           , ("no", castVote VNo)
           , ("voteinfo", voteInfo)
           ]

-- | Clear the current vote (for dev purposes).
onStartup :: SEvent -> Bot ()
onStartup Startup = do
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
findTarget o =
    if useHostAS . actionO $ o
    then getWhois (User targ Nothing Nothing) $ \mbUser ->
        case mbUser of
          Just (User n _ (Just h)) -> startVote $ o { hostO = Just h }
          _ -> getWhowas (User targ Nothing Nothing) $ \mbUser ->
            case mbUser of
              Just (User n _ (Just h)) -> startVote $ o { hostO = Just h }
              _ -> getGlobal' voteChan >>= \c -> do sendMessage c $ "Can't find target '" ++ targ ++ "'."
    else getGlobal' voteChan >>= \c -> getNames c $ \namesL ->
        if targ `elem` [n | (User n _ _) <- namesL] then startVote o
                              else sendMessage c $ "Target '" ++ targ ++ "' not in channel."
  where targ = targetO o

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
    if (fromR ch) /= vChan || not isVote then return ()
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
voteEnd = getGlobal' voteOpts >>= \oMb -> case oMb of
  Nothing -> return ()
  Just opts -> do
    putLogInfo $ "Ending vote: " ++ show opts
    setGlobal' voteOpts Nothing
    ch <- getGlobal' voteChan
    kickUser ch . makeUser . targetO $ opts


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
parseVoteCmd :: Parser (Either String (String, String, Maybe (Either Char Integer), Maybe String))
parseVoteCmd = do start <- parseUntil' " "
                  action <- optionMaybe $ parseUntil' " "
                  target <- optionMaybe $ parseUntil' " "
                  reason <- optionMaybe $ parseUntil' "-" >>= return . rstrip
                  len <- optionMaybe $ do optionMaybe $ char '-'
                                          char 'l'
                                          optionMaybe $ char ' '
                                          (char 'p' >>= return . Left) <|> (decimal >>= return . Right)
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
                    let len = case lenMb of Nothing -> Right (voteLenAS actSetting)
                                            Just l -> l
                    in return . Just $ VoteOpt actSetting targ len reasonMb Nothing
