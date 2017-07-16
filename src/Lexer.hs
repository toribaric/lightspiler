module Lexer
    ( lex'
    ) where

import Tokenizer ( Token, tokenize )

lex' :: String -> [Token]
lex' s = tokenize xs Nothing next
  where
    xs = words s
    next = Just $ head $ tokenize [head $ tail xs] Nothing Nothing
