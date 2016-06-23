module Main where

import JSON
import Model
import GLA
import Text.JSON
import System.Environment


main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    if length args /= 1 then
        fail ("Usage: " ++ progName ++ " <configuration file.json>")
    else do
        let filename = head args
        putStrLn ("Reading from: " ++ filename)
        content  <- readFile filename    
        let c = decode content :: Result Configuration
        case c of 
            Error str -> fail str
            Ok config -> putStrLn (show (runGLA config))
