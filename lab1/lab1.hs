module Lab1 where 

-- PART B: EXCERCISES
{- B.1
 - computes the sum of the squares of its arguments. 
 - Assume both arguments are `Double`s. 
 - Left-associative with precedence of 7. -}
(+*) :: Double -> Double -> Double
(+*) x y = (x ** 2.0) + (y ** 2.0)
infixl 7 +*

{- B.2
 - computes the exclusive-OR of its two (boolean) arguments.
 - right-associative with a precedence of 3 -}
(^||) :: Bool -> Bool -> Bool
(^||) False b = b
(^||) True b = (not) b
infixr 3 ^||

{- takes two Integers and computes the product of all the integers in the
 - range from one integer to the other (inclusive). -}
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x > y     = error "bad inputs"
                 | x == y    = x
                 | otherwise = x * rangeProduct (x + 1) y 
                 
{- point-free function called prod that returns the product of all the
 - numbers in a list of Integers (or 1 if the list is empty). -}
prod :: [Integer] -> Integer
prod = foldr (*) 1 

{- non-recursive definition for rangeProduct -}
rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y | x > y     = error "bad inputs"
                  | otherwise = prod [x..y]
                  
{- maps a two-argument function over two lists. 
 - Write it as a recursive procedure -}
map2 :: (a -> a -> a) -> [a] -> [a] -> [a]
map2 f [] _ = []
map2 f _ [] = []
map2 f (h1:t1) (h2:t2) = (f h1 h2) : (map2 f t1 t2)   

{- function that will work for functions of 3 arguments. -}
map3 :: (a -> a -> a -> a) -> [a] -> [a] -> [a] -> [a]
map3 f [] _ _ = []
map3 f _ [] _ = []
map3 f _ _ [] = []
map3 f (h1:t1) (h2:t2) (h3:t3) = (f h1 h2 h3) : (map3 f t1 t2 t3)   

{- Write out a short (less than 10 lines) evaluation showing that dot lst1
 - lst2 using the point-free definition is equivalent to the explicit
 - (point-wise) definition given above. 
 -
 - dot :: [Integer] -> [Integer] -> Integer
 - dot = (sum .) . map2 (*)
 - 
 - dot lst1 lst2 
 - ((sum .) . map2 (*)) lst1 lst2
 - ((sum .) (map2 (*))) lst1 lst2
 - ((\x → sum . x) (map2 (*))) lst1 lst2
 - (sum (map2 (*)))) lst1 lst2
 - sum (((map2 (*)) lst1) lst2)
 - sum ((map2 (*) lst1) lst2)
 - sum (map2 (*) lst1 lst2)
 -}

result = sum [x | x <- [1..999], (mod x 3 == 0) || (mod x 5 == 0)]
-- 233168

{- Calculate the sum of all the prime numbers below 10000 -}
sieve :: [Int] -> [Int] 
sieve [] = []
sieve (h:t) = h : sieve (filter (\x -> mod x h /= 0) t)

primes = sieve [2..]
answer = sum $ takeWhile (< 10000) primes
-- 5736396


-- PART C: PITFALLS
{- the last line of the original code calls the functions 'head' and 'tail' 
 - on the input list 'lst', and instead of this the input should just be 
 - written in the form (h:t) -}
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (h:t) = h + sumList t
 
{- the original code could be using pattern matching to match on the format of
 - the inputs rather than the length of the inputs. -}
-- Return the largest value in a list of integers.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest (h:[]) = h
largest (h:t)  = max h (largest t)
 

-- PART D: EVALUATION
{- Fibonaacci 
 - fib :: Integer -> Integer
 - fib 0 = 0
 - fib 1 = 1
 - fib n = fib (n - 1) + fib (n - 2)
 -
 - fib 3
 - --> fib (3 - 1) + fib (3 - 2)
 - --> fib 2 + fib (3 - 2)
 - --> fib (2 - 1) + fib (2 - 2) + fib (3 - 2)
 - --> fib 1 + fib (2 - 2) + fib (3 - 2)
 - --> 1 + fib (2 - 2) + fib (3 - 2)
 - --> 1 + fib 0 + fib (3 - 2)
 - --> 1 + 0 + fib (3 - 2)
 - --> 1 + fib (3 - 2)
 - --> 1 + fib 1
 - --> 1 + 1
 - --> 2
 -}
 
{- Factorial 
 - fact :: Integer -> Integer
 - fact n = n * fact (n - 1)
 - fact 0 = 1
 - 
 - fact 3
 - 3 * (fact (3 - 1))
 - 3 * ((3 - 1) * fact (3 - 1 - 1))
 - 3 * ((3 - 1) * ((3 - 1 - 1) * fact (3 - 1 - 1 - 1)))
 - 3 * ((3 - 1) * ((3 - 1 - 1) * ((3 - 1 - 1 - 1) * fact (3 - 1 - 1 - 1 - 1))))
 - and this will continue on infinitely...
 -
 - this can be fixed by swapping the base case line with the recursive line in 
 - the function definition. this fixed the issue bc then 0 is not matched w/ n,
 - but rather w/ 0 as intended.
 -
 - fact :: Integer -> Integer
 - fact 0 = 1
 - fact n = n * fact (n - 1)
 -}
 
{- reverse 
 - reverse :: [a] -> [a]
 - reverse xs = iter xs []
 -   where
 -     iter :: [a] -> [a] -> [a]
 -     iter [] ys = ys
 -     iter (x:xs) ys = iter xs (x:ys)
 -
 - reverse [1,2,3]
 - iter [1,2,3] []
 - iter [2,3] (1 : [])
 - iter [3] (2 : (1 : []))
 - iter [] (3 : (2 : (1 : [])))
 - (3 : (2 : (1 : [])))
 - (3 : (2 : [1]))
 - (3 : [2, 1])
 - [3,2,1]
 -
 - The asymptotic time complexity of this function is O(n), where n is the 
 - length of the input. Each element in the input is taken off the front and
 - then added to the front of the new list. This happens to each element once. 
 -}
 
{- reverse again
 - reverse :: [a] -> [a]
 - reverse [] = []
 - reverse (x:xs) = reverse xs ++ [x]
 - 
 - (++) :: [a] -> [a] -> [a]
 - (++) []     ys = ys
 - (++) (x:xs) ys = x : (xs ++ ys)
 -
 - reverse [1,2,3]
 - (reverse [2,3] ++ [1])
 - ((reverse [3] ++ 2) ++ [1])
 - (((reverse [] ++ [3]) ++ [2]) ++ [1])
 - ((([] ++ [3]) ++ [2]) ++ [1])
 - ((([3]) ++ [2]) ++ [1])
 - 3 : (([] ++ [2]) ++ [1])
 - 3 : ([2] ++ [1])
 - 3 : 2 : ([] ++ [1])
 - 3 : 2 : [1]
 - [3,2,1]
 -
 - The correct asymptotic time complexity of this function is O(n^2), where n 
 - is the length of the input. This is because the concatenation operation is 
 - actually O(n), not O(1). Thus for each reversal of the n elements, there is 
 - also an O(n) concatenation.
 -}
 
{- insertion sort
 - isort :: [Integer] -> [Integer]
 - isort [] = []
 - isort (n:ns) = insert n (isort ns)
 -   where
 -     insert :: Integer -> [Integer] -> [Integer]
 -     insert n [] = [n]
 -     insert n m@(m1:_) | n < m1 = n : m
 -     insert n (m1:ms) = m1 : insert n ms
 -
 - head :: [a] -> a
 - head [] = error "empty list"
 - head (x:_) = x
 - 
 - head (isort [3, 1, 2, 5, 4])
 - head (insert 3 (isort [1, 2, 5, 4]))
 - head (insert 3 (insert 1 (isort [2, 5, 4])))
 - head (insert 3 (insert 1 (insert 2 (isort [5, 4]))))
 - head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
 - head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
 - head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
 - head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
 - head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
 - head (insert 3 (insert 1 (2 : (4 : insert 5 [])))) 
 - head (insert 3 (1 : (2 : (4 : insert 5 [])))) 
 - head (1 : (insert 3 (2 : (4 : insert 5 [])))) 
 - 1
 -}
 
{- foldr and foldl
 - foldr :: (a -> b -> b) -> b -> [a] -> b
 - foldr _ init [] = init
 - foldr f init (x:xs) = f x (foldr f init xs)
 -
 - foldr max 0 [1, 5, 3, -2, 4]
 - max 1 (foldr max 0 [5, 3, -2, 4])
 - max 1 (max 5 (foldr max 0 [3, -2, 4]))
 - max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
 - max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
 - max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
 - max 1 (max 5 (max 3 (max -2 (max 4 0))))
 - max 1 (max 5 (max 3 (max -2 4)))
 - max 1 (max 5 (max 3 4))
 - max 1 (max 5 4)
 - max 1 5
 - 5
 -
 - foldl :: (a -> b -> a) -> a -> [b] -> a
 - foldl _ init [] = init
 - foldl f init (x:xs) = foldl f (f init x) xs
 -
 - foldl max 0 [1, 5, 3, -2, 4]
 - foldl max (max 0 1) [5, 3, -2, 4]
 - foldl max (max (max 0 1) 5) [3, -2, 4]
 - foldl max (max (max (max 0 1) 5) 3) [-2, 4]
 - foldl max (max (max (max (max 0 1) 5) 3) -2)  [4]
 - foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
 - (max (max (max (max (max 0 1) 5) 3) -2) 4)
 - (max (max (max (max 1 5) 3) -2) 4)
 - (max (max (max 5 3) -2) 4)
 - (max (max 5 -2) 4)
 - (max 5 4)
 - 5
 -
 - Both foldr and foldl have a space complexity that is linear in the length of
 - the input. This is because the lazy evaluation cause the expressions to not 
 - be evaluated until they are needed.
 -}
 
 

 
