module Main where

import Prelude
import System.Environment
import System.Exit


main :: IO ()
main = do 
    args <- getArgs
    case (length args) of 
        1 -> do
            f <- readFile (head args)
            f_lines <- return (lines f)
            f_reversed <- return (reverse f_lines)
            (mapM_ putStrLn f_reversed)
            exitSuccess
        _ -> do
            (putStrLn ("usage: reverse filename"))
            exitFailure

