{-# LANGUAGE TemplateHaskell #-}
{- module: Bot.Msg.Splices

Helper splices for Msg - they need to be in a separate module due to
TemplateHaskell constraints.
-}

module Bot.Msg.Splices where

import Language.Haskell.TH

-- | A splice for generating a function that helps parse a message of the format
-- @:<source> <command> <target> [<args>]@
-- where there are @n@ args. The function takes a constructor, which is applied to the source,
-- target, and the arguments. For example:
--
-- > $(makeNMsg 2) :: (Sender -> Recipient -> String -> String -> SEvent) -- ^ The constructor, taking two additional arguments
-- >                  -> Maybe Sender -- ^ The sender, if present (if not, a parse error will result).
-- >                  -> [String] -- ^ The arguments in the message
-- >                  -> Parser SEvent -- ^ Resulting parser, returning the constructed SEvent
makeNMsg :: Integer -> Q Exp
makeNMsg n =
    [| \con s a ->
          case s of
            Nothing -> parserFail "No source for NMessage"
            Just sender ->
              if length a > (n+1) then parserFail $ "Wrong number of arguments to textual message; expecting "++show (n+1)++" but got "++show (length a)
              else case parse parseRecipient "" (a!!0) of
                    Left err -> parserFail . show $ err
                    Right recipient -> return ($(appN (varE 'con) $ (map varE ['sender, 'recipient])
                                                  ++ [ infixE (Just (varE 'a)) (varE '(!!)) (Just (litE (integerL i)))
                                                     | i <- [1..n] ]
                                                 ))
          |]
    where appN :: ExpQ -> [ExpQ] -> ExpQ
          appN a l = foldl appE a l
