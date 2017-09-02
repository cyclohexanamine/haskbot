{-# LANGUAGE TemplateHaskell #-}
{-| module: Bot.Msg.Splices

Helper splices for Msg - they need to be in a separate module due to
TemplateHaskell constraints.
-}

module Bot.Msg.Splices where

import Language.Haskell.TH

-- | Apply the given expression to the given list.
appN :: ExpQ -> [ExpQ] -> ExpQ
appN a l = foldl appE a l

-- | A splice for generating a function that helps parse a message of the format
-- @:\<source> \<command> \<target> [\<args>]@
-- where there are @n@ args. The function takes a constructor, which is applied to the source,
-- target, and the arguments. For example:
--
-- > $(makeSTMsg 2) :: (Sender -> Recipient -> String -> String -> SEvent) -- ^ The constructor, taking two additional arguments
-- >                  -> Maybe Sender -- ^ The sender, if present (if not, a parse error will result).
-- >                  -> [String] -- ^ The arguments in the message
-- >                  -> Parser SEvent -- ^ Resulting parser, returning the constructed SEvent
makeSTMsg :: Integer -> Q Exp
makeSTMsg n =
    [| \con s a ->
          case s of
            Nothing -> parserFail "No source for source/target message"
            Just sender ->
              if length a /= (n+1) then parserFail $ "Wrong number of arguments to textual message; expecting "++show (n+1)++" but got "++show (length a)
              else case parse parseRecipient "" (a!!0) of
                    Left err -> parserFail . show $ err
                    Right recipient -> return ($(appN (varE 'con) $ (map varE ['sender, 'recipient])
                                                  ++ [ infixE (Just (varE 'a)) (varE '(!!)) (Just (litE (integerL i)))
                                                     | i <- [1..n] ]
                                                 ))
    |]

-- | Like 'makeSTMsg', but doesn't want a target.
makeSMsg :: Integer -> Q Exp
makeSMsg n =
    [| \con s a ->
          case s of
            Nothing -> parserFail "No source for source message"
            Just sender ->
              if length a /= n then parserFail $ "Wrong number of arguments to textual message; expecting "++show n++" but got "++show (length a)
              else return ($(appN (varE 'con) $ [varE 'sender]
                           ++ [ infixE (Just (varE 'a)) (varE '(!!)) (Just (litE (integerL i))) | i <- [0..n-1] ]
                             ))
    |]

-- | A splice for parsing the first 'n' arguments of a command into the given constructor.
-- Agnostic about the presence of source, or any further arguments.
makeNMsg :: Integer -> Q Exp
makeNMsg n =
    [| \con a ->
          if length a < n then parserFail $ "Wrong number of arguments to textual message; expecting at least"++show n++" but got "++show (length a)
          else return ($(appN (varE 'con) $ [ infixE (Just (varE 'a)) (varE '(!!)) (Just (litE (integerL i)))
                                            | i <- [0..n-1] ]))
    |]
