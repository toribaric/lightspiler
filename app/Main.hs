module Main where

import Data.List ( intercalate )
import Parser ( parse )

main :: IO ()
main = do
    putStrLn "Provide JS expression:"
    content <- getLine
    putStrLn $ intercalate "\n" (parse content)
