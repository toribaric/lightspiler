module Parser
    ( parse
    ) where

import Tokenizer
import Lexer

parseInner :: Token -> String
parseInner token
  | kind token == Just "EXPSTART" = "var"
  | otherwise                     = code token

parseOuter :: [Token] -> [String]
parseOuter history
  | containsKind (Just "SPREAD") history = ["for (var prop in " ++ spreadVar ++ ") {\
                                            \if (" ++ spreadVar ++ ".hasOwnProperty(prop)) {\
                                            \" ++ expVar ++ "[prop] = " ++ spreadVar ++ "[prop]\
                                            \}\
                                            \}"]
  | otherwise                            = []
  where
    spreadVar = [ x | x <- getCode (Just "SPREAD") history, not (x `elem` ".,") ]
    expVar = code (history!!1)

parseAll :: [String] -> [Token] -> String -> [Token] -> [String]
parseAll acc history expr [] = acc ++ [expr]
parseAll acc history expr (x:xs)
  | isKind (Just "SPREAD") (Just x) = parseAll acc (history ++ [x]) expr xs
  | isKind (Just "EXPSTART") next'  = (expr ++ inner) : outer ++ parseAll acc [] "" xs
  | otherwise                       = parseAll acc (history ++ [x]) (expr ++ inner ++ " ") xs
  where
    next' = next x
    inner = parseInner x
    outer = parseOuter history

parse :: String -> [String]
parse s = parseAll [] [] "" . lex' $ s
