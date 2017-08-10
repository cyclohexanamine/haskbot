module Scripts.Vote where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Prim (parserFail)
import Data.Maybe

import Bot
import Data.List

voteChan = CacheKey (RChannel "") "VOTE" "voteChan"
voteActive = CacheKey False "VOTE" "voteActive"
voteOpts = CacheKey (VoteOpt Nothing Nothing Nothing Nothing) "VOTE" "voteOpts"

commandChar = '!'
callbacks = [ processCommand ]

-- | Listen to all channel messages, and extract those which are
-- commands for this bot.
processCommand :: SEvent -> Bot()
processCommand ev@(SPrivmsg (SUser nick _ _) ch@(RChannel _) text)
    | length text == 0 = return ()
    | head text /= commandChar = return ()
    | otherwise = do
    vChan <- getGlobal' voteChan
    if ch /= vChan then return ()
    else case parse parseCommand "" . tail $ text of
        Left err -> putLogError $ "Vote command parse error: " ++ show err
        Right act -> act ev


commands = [ ("vote", vote)
           , ("yes", voteYes)
           , ("no", voteNo)
           , ("voteinfo", voteInfo)
           ]
           
parseUntil' s = do x <- parseUntil s
                   many $ oneOf s
                   return x

parseCommand :: Parser (SEvent -> Bot ())
parseCommand = do cmd <- parseUntil' " "
                  if cmd == ""
                  then return $ \ev -> return ()
                  else case lookup cmd commands of
                         Just act -> return act
                         Nothing -> return $ \ev -> do
                            vc <- getGlobal' voteChan
                            sendMessage vc $ "Invalid vote command '" ++ cmd ++ "'"

data VoteOpt = VoteOpt { actionO :: Maybe String, targetO :: Maybe String, lenO :: Maybe Int, reasonO :: Maybe String }
    deriving (Eq, Read, Show)
parseVoteCmd :: Parser VoteOpt
parseVoteCmd = do start <- parseUntil' " "
                  action <- optionMaybe $ parseUntil' " "
                  target <- optionMaybe $ parseUntil' " "
                  reason <- optionMaybe $ parseUntil' "-"
                  len <- optionMaybe . try $ do char 'l'
                                                optionMaybe $ char ' '
                                                decimal
                  if start == "!vote"
                  then return $ VoteOpt action target len reason
                  else parserFail $ "Parse fail in parseVoteCmd - " ++ start

acts = ["kick", "ban", "unban"]
actl = [("kick", 5), ("ban", 3600), ("unban", 3600)]
                  
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
                         else do sendMessage ch $ "Starting vote to " ++ fromJust (actionO opt) ++ "."
                                 startVote opt
                         
startVote :: VoteOpt -> Bot ()
startVote o@(VoteOpt (Just act) (Just targ) reasonMb lenMb) = do
    putLogInfo $ "Starting vote: " ++ show o
    let t = fromJust . lookup act $ actl
    runInS t voteEnd
    setGlobal' voteOpts o
    setGlobal' voteActive True

voteYes ev = return ()
voteNo ev = return ()
voteInfo ev = return ()

voteEnd :: Bot ()
voteEnd = do
    o <- getGlobal' voteOpts
    putLogInfo $ "Ending vote: " ++ show o
    setGlobal' voteActive False
    


