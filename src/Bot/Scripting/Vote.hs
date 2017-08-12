{-| Module: Bot.Scripting.Vote

The voting logic.
-}
module Bot.Scripting.Vote (
    callbacks, voteChan,
    -- * Vote data
    VoteOpt(..),
    -- * Command handling
    processCommand, parseCommand, parseVoteCmd,
    -- * Voting logic
    vote, findTarget, startVote, voteEnd,
    VoteChoice(..), castVote, voteInfo,
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Prim (parserFail)
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)

import Bot

-- | Event hooks
callbacks = [ processCommand, onStartup ]

-- Persistent values holding information about voting, the current vote, etc.
-- | The channel the bot should conduct voting in.
voteChan = CacheKey (Channel "") "VOTE" "voteChan"
voteActive = CacheKey False "VOTE" "voteActive"
voteOpts = CacheKey (VoteOpt Nothing Nothing Nothing Nothing Nothing) "VOTE" "voteOpts"
yesHosts = CacheKey [] "VOTE" "yesHosts" :: PersistentKey [String]
noHosts = CacheKey [] "VOTE" "noHosts" :: PersistentKey [String]

-- | The commands a user can give in the channel.
commandChar = '!'
commands = [ ("vote", vote)
           , ("yes", castVote VYes)
           , ("no", castVote VNo)
           , ("voteinfo", voteInfo)
           ]

-- | Actions that can be voted on, and their vote lengths in seconds.
acts = ["kick", "ban", "unban"]
actl = [("kick", 5), ("ban", 3600), ("unban", 3600)]

-- | Clear the current vote (for dev purposes).
onStartup :: SEvent -> Bot ()
onStartup Startup = do
    setGlobal' voteActive False

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
    isVote <- getGlobal' voteActive
    if isVote then sendMessage ch "Vote already active"
    else case parse parseVoteCmd "" text of
            Left err -> putLogError . show $ err
            Right opt -> if isNothing $ actionO opt
                         then sendMessage ch "Need an action."
                         else if not $ (fromJust . actionO $ opt) `elem` acts
                         then sendMessage ch $ "'" ++ (fromJust . actionO $ opt) ++ "' is not a valid action."
                         else if isNothing $ targetO opt
                         then sendMessage ch "Need a target."
                         else findTarget opt

-- | Look up the target - a whois\/whowas for kick\/banning, or checking if they're in the channel for a kick.
findTarget :: VoteOpt -> Bot ()
findTarget o@(VoteOpt (Just act) (Just targ) _ _ _) =
    if act == "kick"
    then getGlobal' voteChan >>= \c -> getNames c $ \namesL ->
        if targ `elem` [n | (User n _ _) <- namesL] then startVote o
                              else sendMessage c $ "Target '" ++ targ ++ "' not in channel."
    else getWhois (User targ Nothing Nothing) $ \mbUser ->
        case mbUser of
          Just (User n _ (Just h)) -> startVote $ o { hostO = Just h }
          _ -> getWhowas (User targ Nothing Nothing) $ \mbUser ->
            case mbUser of
              Just (User n _ (Just h)) -> startVote $ o { hostO = Just h }
              _ -> getGlobal' voteChan >>= \c -> do sendMessage c $ "Can't find target '" ++ targ ++ "'."

-- | Start the given vote.
startVote :: VoteOpt -> Bot ()
startVote o@(VoteOpt (Just act) (Just targ) _ _ _) = do
    ch <- getGlobal' voteChan
    sendMessage ch $ "Starting vote to " ++ fromJust (actionO o) ++ "."
    putLogInfo $ "Starting vote: " ++ show o
    let t = fromJust . lookup act $ actl
    runInS t voteEnd
    setGlobal' yesHosts []
    setGlobal' noHosts []
    setGlobal' voteOpts o
    setGlobal' voteActive True

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
    isVote <- getGlobal' voteActive
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

-- | Return info about the current vote.
voteInfo ev = return ()


-- | End the current vote, taking the action if successful.
voteEnd :: Bot ()
voteEnd = do
    o <- getGlobal' voteOpts
    putLogInfo $ "Ending vote: " ++ show o
    setGlobal' voteActive False


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

-- | Vote options. Not all possible combinations are valid votes - they may have
-- @Nothing@s for necessary fields.
data VoteOpt = VoteOpt { actionO :: Maybe String, targetO :: Maybe String
                       , lenO :: Maybe (Either Char Int), reasonO :: Maybe String
                       , hostO :: Maybe String }
    deriving (Eq, Read, Show)

-- | Parse a !vote command, returning vote options.
parseVoteCmd :: Parser VoteOpt
parseVoteCmd = do start <- parseUntil' " "
                  action <- optionMaybe $ parseUntil' " "
                  target <- optionMaybe $ parseUntil' " "
                  reason <- optionMaybe $ parseUntil' "-" >>= return . rstrip
                  len <- optionMaybe . try $ do char 'l'
                                                optionMaybe $ char ' '
                                                (char 'p' >>= return . Left) <|> (decimal >>= return . Right)
                  if start == "!vote"
                  then return $ VoteOpt action target len reason Nothing
                  else parserFail $ "Parse fail in parseVoteCmd - " ++ start
