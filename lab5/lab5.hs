module Lab5 where

import Prelude
import Control.Monad

-- A.1
{-
 - hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
 - hr_solutions =
 -   [((i, l), (j, k), i^3 + l^3) |
 -    i <- [1..],
 -    j <- [1..i-1],
 -    k <- [1..j-1],
 -    l <- [1..k-1],
 -    i^3 + l^3 == j^3 + k^3]
 - 
 - rewrite with list monad
 -}

hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions = do i <- [1..]
                  j <- [1..i-1]
                  k <- [1..j-1]
                  l <- [1..k-1]
                  guard $ i^3 + l^3 == j^3 + k^3
                  return ((i, l), (j, k), i^3 + l^3)

-- A.2
{- 
 - result = sum [x | x <- [1..999], (mod x 3 == 0) || (mod x 5 == 0)]
 -    233168
 - rewrite with list monad
 -}
b6_solution1 :: Integer
b6_solution1 = sum (do 
               x <- [1..999]
               guard $ (mod x 3 == 0) || (mod x 5 == 0)
               return x)
-- b6_solution1 -> 233168
             
b6_solution2 :: Integer
b6_solution2 = sum (do 
               x <- [1..999]
               if (mod x 3 == 0) || (mod x 5 == 0)
               then return x
               else mzero)
-- b6_solution2 -> 233168
    
-- A.3           
isPalindrome :: Integer -> Bool
isPalindrome i = (show i) == (reverse (show i))

largestPalindrome :: Integer 
largestPalindrome = maximum (do
                   i <- [100..999]
                   j <- [i..999]
                   guard $ isPalindrome (i * j)
                   return (i * j))         
-- largestPalindrome -> 906609

-- A.4
type Expr = [Item]

data Item = N Int | O Op
    deriving Show

data Op = Add | Sub | Cat
    deriving Show
  
ops :: [Item]
ops = [O Add, O Sub, O Cat] 

-- a.
exprs :: [Expr]
exprs = do 
        op1 <- ops
        op2 <- ops
        op3 <- ops
        op4 <- ops
        op5 <- ops
        op6 <- ops
        op7 <- ops
        op8 <- ops
        return [N 1, op1, N 2, op2, N 3, op3, N 4, op4, N 5, op5, N 6, op6, 
                N 7, op7, N 8, op8, N 9]
                
-- b.                
normalize :: Expr -> Expr
normalize [] = []
normalize (N i : []) = [N i]
normalize (N i : O Cat : N j : end) = normalize (N ((10 * i) + j) : end)
normalize (N i : O op : end) = (N i) : (O op) : (normalize end)
normalize _ = error("invalid expression")

-- c.
evaluate :: Expr -> Int
evaluate [] = 0
evaluate (N i : []) = i 
evaluate (N i : O Add : N j : end) = evaluate (N (i + j) : end)
evaluate (N i : O Sub : N j : end) = evaluate (N (i - j) : end)
evaluate _ = error("invalid expression")

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs


{-
 - concatMap :: (a -> [b]) -> [a] -> [b]
 - concatMap f lst = concat (map f lst)
 -
 - (>>=) :: [a] -> (a -> [b]) -> [b]
 - mv >>= f = concatMap f mv
 - 
 -}
 
-- B.1
{-
 - do n1 <- [1..6]
 -    n2 <- [1..6]
 -    []
 -    return (n1, n2)
 -
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >>= \_ -> return (n1, n2)
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> concatMap (\_ -> return (n1, n2) [])
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> []
 - [1..6] >>= \n1 -> (concatMap (\n2 -> []) [1..6])
 - [1..6] >>= \n1 -> []
 - (concatMap (\n1 -> []) [1..6])
 - []
 -}
 
-- B.2
{- do n1 <- [1..6]
 -    n2 <- [1..6]
 -    return <anything>
 -    return (n1, n2)
 -
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> return (n1, n2)
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> \_ -> return (n1, n2)
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> 
                            concatMap (\_ -> return (n1, n2)) (return <anything>) 
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> concat (return (n1, n2))
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)
 -
 -
 - do n1 <- [1..6]
 -    n2 <- [1..6]
 -    return (n1, n2)
 -
 - [1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)
 -}
 
-- B.3
{- let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
 - do ['a', 'a', c1, c2, 'b', 'b'] <- s
 -     return [c1, c2]
 -
 - s >>= 
 -   \y -> case y of 
 -     ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
 -    _ -> fail []
 -
 - ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] >>= 
 -   \y -> case y of 
 -     ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
 -    _ -> fail []
 -
 - concatMap (\y -> case y of ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
 -             _ -> fail []) ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] 
 -
 - concat (map (\y -> case y of ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
 -             _ -> fail []) ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"])
 -
 - concat (return ['x', 'y'], return ['z', 'w'], [], return ['c', 'c'], [])
 - ["xy", "zw", "cc"]
 -
 - if "fail s = error s" was used instead of fail, then in the cases that did
 - not pattern match, an error would be thrown and break the program, not 
 - letting us get the return value we want.
 -}
 
-- B.4 
{- given m = [x1, x2, ...], 
 - 
 - k = foldr ((++) . k) [] m
 - k = foldr ((++) . k) [] [x1, x2, ...]
 - k = foldr (\y -> (++) k y) [] [x1, x2, ...]
 - k = [k x1, k x2, ...]
 -
 - k = concat (map k m)
 - k = concat (map k [x1, x2, ...])
 - k = concat ([k x1, k x2, ...])
 - k = [k x1, k x2, ...]
 -}
 
-- B.5 
{- this error is thrown because once the program gets to AnyNum (n + s), while
 - n and s are type AnyNum, the values bound to n and s are not of the same 
 - type, so applying the (+) operator to the values is invalid. The AnyNum 
 - datatype would need to be changed in the anySum method to resolve the error.
 -}
 

 
 
       
                
                

