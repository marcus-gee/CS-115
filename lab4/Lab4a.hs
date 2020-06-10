module Lab4a where

-- A.1
{- Desugar the following: 
 - myPutStrLn :: String -> IO ()
 - myPutStrLn "" = putChar '\n'
 - myPutStrLn (c:cs) = do
 -   putChar c
 -   myPutStrLn cs
 -}
 
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = (putChar c) >> (myPutStrLn cs)

-- A.2
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- A.3 
{- Ask the user for his/her name, then print a greeting.
 - greet2 :: IO ()
 - greet2 = do
 -   putStr "Enter your name: "
 -   name <- getLine
 -   putStr "Hello, "
 -   putStr name
 -   putStrLn "!"
 -}
-- simple desugaring
greet2 :: IO ()
greet2 = 
  putStr "Enter your name: " >>
  getline >>= \name -> (putStr "Hello, " >> putStr name >> putStrLn "!")
  
-- desugaring using case
greet2 :: IO ()
greet2 = 
  putStr "Enter your name: " >>
  getline >>= 
    \x -> case x of
        name -> (putStr "Hello, " >> putStr name >> putStrLn "!")
        _ -> fail "Pattern match failure in do expression"
  
{- no the both desugaring behave in the same manner as with the complex case, 
 - no pattern match would actually fail -} 
 
-- A.4
{- Need to import this to get the definition of toUpper:
 - import Data.Char

 - Ask the user for his/her name, then print a greeting.
 - Capitalize the first letter of the name.
 - greet3 :: IO ()
 - greet3 = do
 -  putStr "Enter your name: "
 -  (n:ns) <- getLine
 -  let name = toUpper n : ns
 -  putStr "Hello, "
 -  putStr name
 -  putStrLn "!"
 -}
import Data.Char
-- simple desugaring
greet3 :: IO ()
greeet3 = 
    putStr "Enter your name: " >>
    getline >>= \(n:ns) ->
        let name = toUpper n : ns in 
            (putStr "Hello, " >> putStr name >> putStrLn "!")
            
-- desugaring using case
greet3 :: IO ()
putStr "Enter your name: " >>
  getline >>= \x -> case x of
    (n:ns) -> let name = toUpper n : ns in
                (putStr "Hello, " >> putStr name >> putStrLn "!")
         _ -> fail "Pattern match failure in do expression"
  
{- in this case, yes the more complex desugaring has a different effect. in the 
 - first case, if the user enters nothing, then it would not be handled 
 - correctly whereas it would in the first. -}
 



  
