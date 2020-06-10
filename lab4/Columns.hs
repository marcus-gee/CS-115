module Main where

import Prelude
import Data.Char
import Data.List
import System.IO
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    -- make sure we have at least one positve column index
    if (length args < 2) || 
        (length (filter (\x -> x/= "0") (filter positiveCol args)) /= (length args) - 1)
    then 
        -- usage message
        putStrLn ("usage: columns n1 n2 ... filename (ni must be positive int)") 
        >> exitFailure
    else
        -- columns are from arg inputs
        let col = map (\x -> read x :: Int) (take ((length args) - 1) args) in
            -- is input from a file or read from terminal
            if last args == "-"
            then do 
                -- get terminal input
                input <- hGetContents stdin
                let l = lines input
                    -- keep necessary columns
                    output = map (dropColumns col) l in
                    -- output to terminal
                    putStrLn (intercalate "\n" output)
            else do
                f <- readFile (last args)
                let l = lines f
                    -- keep necessary columns
                    output = map (dropColumns col) l in
                    -- output to terminal
                    putStrLn (intercalate "\n" output)
                    
        
    
-- checks if all inputs representing columns are positive   
positiveCol :: String -> Bool
positiveCol []  = False
positiveCol [x] = isDigit x
positiveCol (x:xs) = isDigit x && positiveCol xs
        
-- string together lines for needed columns
dropColumns :: [Int] -> String -> String
dropColumns columns string = 
    let words_lst = words string 
        -- remove any column where no words would be
        valid_col = filter (\x -> x <= length (words_lst)) columns
        -- make the line of words that are in the columns we want
        word_lines = map (\c -> words_lst !! (c-1)) valid_col in
    intercalate " " word_lines
