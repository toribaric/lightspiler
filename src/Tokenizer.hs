module Tokenizer
    ( Token, code, kind, prev, next, tokenize, getCode, isKind, containsKind
    ) where

import Data.List (isInfixOf)

data Token = Token
  { code :: String
  , kind :: Maybe String
  , prev :: Maybe Token
  , next :: Maybe Token
  } deriving (Show, Eq)

isKind :: Maybe String -> Maybe Token -> Bool
isKind Nothing _ = False
isKind _ Nothing = False
isKind kind' (Just token) = kind' == kind token

containsKind :: Maybe String -> [Token] -> Bool
containsKind _ [] = False
containsKind Nothing _ = False
containsKind k (x:xs) = if k == kind x then True else containsKind k xs

getCode :: Maybe String -> [Token] -> String
getCode _ [] = ""
getCode Nothing _ = ""
getCode k (x:xs) = if k == kind x then code x else getCode k xs

findToken :: String -> [(String, v)] -> Maybe v
findToken key [] = Nothing
findToken key ((k,v):xs) = if k `isInfixOf` key
                           then Just v
                           else findToken key xs

tokenize :: [String] -> Maybe Token -> Maybe Token -> [Token]
tokenize [] _ _ = []
tokenize (x:xs) p n =
    Token {code = x, kind = findToken x tokens, prev = p, next = n}
    : tokenize xs prev next
  where
    tokens = [ ("const", "EXPSTART")
             , ("let", "EXPSTART")
             , ("=", "ASSIGN")
             , ("{", "BODYSTART")
             , ("}", "BODYEND")
             , ("...", "SPREAD")
             ]
    prev = if null xs
           then Nothing
           else Just $ head $ tokenize [x] Nothing Nothing
    next = if length xs == 1
           then Nothing
           else Just $ head $ tokenize [head $ tail xs] Nothing Nothing
