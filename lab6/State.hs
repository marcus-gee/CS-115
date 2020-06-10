module State where

import Control.Monad
import Control.Monad.State
import Data.IORef

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO ()
whileIO test block =
  do b <- test
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body =
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)
              
-- A.1
factIO :: Integer -> IO Integer
factIO z = 
    if z < 1
    then error "input must be positive integer"
    else do 
        counter <- newIORef z
        product <- newIORef 1
        whileIO 
            (do counter' <- readIORef counter
                return (counter' /= 0))
            (do counter' <- readIORef counter
                product' <- readIORef product
                writeIORef product (product' * counter')
                writeIORef counter (counter' - 1))
        readIORef product
        
-- A.2 
helper_fact :: State (Integer, Integer) Integer
helper_fact = do
    whileState (\(count, _) -> count /= 0)
        (do (count, prod) <- get 
            put ((count - 1), (count * prod)))
    (_, prod) <- get
    return prod
    
factState :: Integer -> Integer
factState z = 
    if z < 1
    then error "input must be positive integer"
    else evalState helper_fact (z, 1)

-- A.3
fibIO :: Integer -> IO Integer
fibIO z = 
    if z < 0
    then error "input must be greater than zero"
    else do 
        counter  <- newIORef z
        fib_prev <- newIORef 1
        fib_curr <- newIORef 0
        whileIO
            (do counter' <- readIORef counter
                return (counter' /= 0))
            (do counter'  <- readIORef counter
                fib_prev' <- readIORef fib_prev
                fib_curr' <- readIORef fib_curr
                writeIORef counter  (counter' - 1)
                writeIORef fib_prev (fib_curr')
                writeIORef fib_curr (fib_curr' + fib_prev'))
        readIORef fib_curr

-- A.4
helper_fib :: State (Integer, Integer, Integer) Integer
helper_fib = do 
    whileState (\(count, _, _) -> count /= 0)
        (do (count, fib_prev, fib_curr) <- get
            put ((count - 1), fib_curr, (fib_prev + fib_curr)))
    (_, _, fib_curr) <- get
    return fib_curr

fibState :: Integer -> Integer
fibState z = 
    if z < 0 
    then error "input must be greater than zero"
    else evalState helper_fib (z, 1, 0)

-- B.1
{-
 - To start, we will derive the >>= operator followed by the return. Assume, 
 - we have the following two functions in the Reader monad with the type 
 - signatures:
 - f :: a -> Reader r b
 - g :: b -> Reader r c
 - and they can be composed as follows:
 - h :: a -> Reader r c
 - In non-monadic form, these functions would like:
 - f' :: (a, r) -> b
 - g' :: (b, r) -> c
 - h' :: (a, r) -> c
 - Now, we can write h' in terms of f', g', which will then allow us to write 
 - the h in terms of f, g.
 - h' :: (a, r) -> c
 - h' (i, r) = 
 -      let j = f' (i, r)
 -          k = g' (j, r)
 -      in (k, r)
 - So, now we can have:
 - h = f >=> g
 - h i = f i >=> g
 - h i = f i >=> (\j -> g j)
 - h i = do j <- f i 
                 g y
 - Now, if we curry our initial declarations of f' and g',
 - f'' :: a -> r -> b
 - g'' :: b -> r -> c
 - which can be writter in terms of f' and g', respectively
 - f'' :: a -> r -> b
 - f'' i r = f' (i, r)
 - f'' i = \r -> f' (i, r)
 - 
 - g'' :: b -> r -> c
 - g'' j r = g' (j, r)
 - g'' j = \r -> g' (j, r)
 - This allows us to get f and g in terms of f' and g' by using the Reader
 - constructor with the RHS of f'' and g''
 - f :: a -> Reader r b
 - f i = Reader (\r -> f' (i, r))
 - g :: b -> Reader r c
 - g j = Reader (\r -> g' (j, r))
 - We can also do the same for h.
 - h :: a -> Reader r c
 - h i = Reader (\r -> h' (i, r))
 - We know that this expression is equivalent to that on line 109, so,
 - h i = f >=> g = Reader (\r -> h' (i, r))
 - f >=> g = Reader (\r -> h' (i, r))
 - f >=> g = Reader (\r -> let j = f' (i, r)
 -                             k = g' (j, r)
 -                         in (k, r))
 - f >=> g = Reader (\r -> let j = f' (i, r)
 -                         in g' (j, r))
 - f >=> g = Reader (\r -> let (Reader ff) = f i
 -                             j = ff r
 -                         in g' (j, r))
 - f >=> g = Reader (\r -> let (Reader ff) = f i
 -                             j = ff r
 -                             (Reader gg) = g j 
 -                         in gg r)
 - Now, we can make substitutions that will allow us to get the above 
 - expression in the desired form.
 - Substitute:
 -      mx -> f i
 -      f  -> g
 -      g  -> ff
 -      x  -> j
 -      h  -> gg
 - Giving:
 - mx >=> f = Reader (\r -> 
 -              let (Reader g) = mx
 -                  x = g r
 -                  (Reader h) = f x
 -              in h r)
 -
 - For the reader function, "we can write the characteristic monadic functions
 - in the reader monad as":
 - a -> Reader r b
 - and the non-monadic reader functions have type declarations of the form:
 - (a, r) -> b
 - So, the identity function would look like:
 - id (x, r) -> x
 - id' x -> (\r -> x)
 - Written in the reader monad, this looks like:
 - id :: a -> Reader r a
 - id x -> Reader (\r -> x)
 - and is in the form we want 
 -}

