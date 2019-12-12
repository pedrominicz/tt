module Main where

import Eval
import Parse
import Syntax

main :: IO ()
main = do
    input <- parse <$> getLine
    case input of
        Just input -> putStrLn $ show $ eval input
        Nothing -> putStrLn "?"
    main
