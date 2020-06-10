module Lab3ab where

-- A.1
{- 
data Nat = Zero | Succ Nat

instance Eq Nat where 
    Zero   == Zero   = True
    Succ x == Succ y = (x == y)
    _      == _      = False
    
instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ n) = "Succ (" ++ show x ++ ")"
-} 

-- A.2 
data Nat = Zero | Succ Nat 
    deriving (Eq, Show)
    
-- A.3   
instance Ord Nat where
    Zero <= _    = True
    _    <= Zero = False
    Succ x <= Succ y = (x <= y)

{- Yes, Haskell could derive the Ord instance for us, becuase Zero is less than
 - any Succ, and Succ makes each value larger than the previous. 
 -}
    
-- A.4
data SignedNat =
    Neg Nat | Pos Nat
    deriving (Show)
    
instance Eq SignedNat where 
    Neg Zero == Pos Zero = True
    Pos Zero == Neg Zero = True
    Neg n    == Neg x    = (n == x)
    Pos p    == Pos x    = (p == x)
    Neg _    == Pos _    = False
    Pos _    == Neg _    = False
    
instance Ord SignedNat where 
    Neg _ <= Pos _ = True
    Pos p <= Neg n | (p == Zero) && (n == Zero) = True
                   | otherwise = False
    Neg n    <= Neg x = (n >= x)
    Pos p    <= Pos x = (p <= x)
  
{- No, in this case we cannot let Haskell derive the Ord instance as it makes 
 - all Neg values less than Pos ones, however in our case, we have both Neg Zero
 - and Pos Zero which are in fact equal. 
 -}
 
-- A.5     
addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat n Zero = n
addNat (Succ n) (Succ m) = Succ (Succ (addNat n m))

subNat :: Nat -> Nat -> Nat
subNat n Zero = n
subNat (Succ n) (Succ m) | n == m    = Zero 
                         | otherwise = subNat n m
subNat _ _ = error("cannot subtract larger value from smaller one for Nats")

multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat _ Zero = Zero
multNat (Succ n) (Succ m) = addNat (Succ m) (multNat n (Succ m))

absSignedNat :: SignedNat -> SignedNat
absSignedNat (Neg n) = (Pos n)
absSignedNat (Pos n) = (Pos n)

signumSignedNat :: SignedNat -> SignedNat
signumSignedNat (Neg Zero) = Pos Zero 
signumSignedNat (Pos Zero) = Pos Zero 
signumSignedNat (Neg _) = Neg (Succ Zero)
signumSignedNat (Pos _) = Pos (Succ Zero)

natFromInteger :: Integer -> Nat
natFromInteger i | i == 0    = Zero
                 | i > 0     = Succ (natFromInteger (i - 1))
                 | otherwise = Succ (natFromInteger (i + 1))
                 
signedNatFromInteger :: Integer -> SignedNat
signedNatFromInteger i | i == 0    = Pos Zero
                       | i > 0     = Pos (natFromInteger i)
                       | otherwise = Neg (natFromInteger i)
                 
addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos p1) (Pos p2) = Pos (addNat p1 p2)
addSignedNat (Neg n1) (Neg n2) = Neg (addNat n1 n2)
addSignedNat (Pos p) (Neg n) | p == n = (Pos Zero)
                             | p > n  = Pos (subNat p n)
                             | otherwise = Neg (subNat n p)
addSignedNat (Neg n) (Pos p) | n == p = (Pos Zero)
                             | n > p  = Neg (subNat n p)
                             | otherwise = Pos (subNat p n)

negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Pos p) = (Neg p)
negateSignedNat (Neg n) = (Pos n)

subSignedNat :: SignedNat -> SignedNat -> SignedNat
subSignedNat p1@(Pos _) p2@(Pos _) = addSignedNat p1 (negateSignedNat p2)
subSignedNat n1@(Neg _) n2@(Neg _) = addSignedNat n1 (negateSignedNat n2)
subSignedNat  p@(Pos _)  n@(Neg _) = addSignedNat p (negateSignedNat n)
subSignedNat  n@(Neg _)  p@(Pos _) = addSignedNat n (negateSignedNat p)

multSignedNat :: SignedNat -> SignedNat -> SignedNat
multSignedNat (Pos p1) (Pos p2) = Pos (multNat p1 p2)
multSignedNat (Neg n1) (Neg n2) = Pos (multNat n1 n2)
multSignedNat (Pos p)  (Neg n)  = Neg (multNat p n)
multSignedNat (Neg n)  (Pos p)  = Neg (multNat n p)     
        
instance Num SignedNat where 
    (+) = addSignedNat
    (-) = subSignedNat 
    (*) = multSignedNat
    abs = absSignedNat
    signum = signumSignedNat
    negate = negateSignedNat 
    fromInteger = signedNatFromInteger
    
-- A.6
signedNatToInteger :: SignedNat -> Integer   
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger (Pos (Succ p)) = 1 + (signedNatToInteger (Pos p))
signedNatToInteger (Neg (Succ n)) = -1 + (signedNatToInteger (Neg n))

-- A.7
{- One thing that is a bit strange with the SignedNat datatype is that it 
 - duplicates the the representation of zero. Another thing is that it is 
 - somewhat ugly to define all values with either Pos or Neg.
 -
 - We could fix it using a unary encoding where we can represent Pos and Neg 
 - values around zero
 -
 - data UnaryInteger = Prev | Zero | Succ     
 -}
      
-- A.8
factorial :: (Num a, Ord a) => a -> a
factorial n | n == 0 = 1
            | n > 0  = n * (factorial (n - 1))
            | otherwise = error("cannot have factorial of number less than 0")


            

-- B.1 
-- a
{- >#<
 - This operator is non-associative. This is because its arguments are required
 - to be integers, and its return type is a string. Thus, if one tried to chain 
 - the expression, the data types would not be compatible and there would be a 
 - type error.
 -}

-- b
{- +|
 - This operator could be either infixl or infixr, but we will declare it to be 
 - left-associative. This is because we if we were to sum a group of numbers at
 - once, we would get the same last digit were we to sum the last digits of a 
 - group of numbers (i.e. 7 + 6 + 5 + 8 = 26 -> last digit = 6 and (((7 +| 6)
 - +| 5) +| 8) = ((3 +| 5) +| 8) = (8 +| 8) = 6). Becuase of this, the operator
 - would work as right-associative too.
 -}

-- c
{- &<
 - This operator must be declared as infixl. This is because it has type 
 - [Integer] -> Integer -> [Integer]. Since it returns a list and its first
 - argument is list it must be left-associatve. For example, [1, 2] &< 3 &< 4
 - must be taken like this (([1, 2] &< 3) &< 4), not like this 
 - ([1, 2] &< (3 &< 4)) since "3" is not type [Integer].
 -}

-- d
{- This operator must be declared as infixr. This is because it has type 
 - [Integer -> Integer] -> [Integer]. Since it returns a list and its second
 - argument is list it must be right-associatve. For example, 0 >&& 1 >&& [2, 3]
 - must be taken like this (0 >&& 1( >&& [2, 3])), not like this 
 - ((0 >&& 1) >&& [2, 3]) since "1" is not type [Integer].
 -}
 
-- B.2
{- 2 + 800 = 3
 - take (2 + 800 + 1000) = 1802, so answer should be 4.
 - infixl: ((2 + 800) + 1000) = (3 + 1000) = 4 
 - infixr: (2 + (800 + 1000)) = (2 + 4) = 1  
 -      * correct answer is 4, so right-associative does not work
 - now, take (2 + 1000 + 800) = 1802, answer should still be 4.
 - infixr: ((2 + 1000) + 800) = (3 + 800) = 3
 - infixl: (2 + (1000 + 800)) = (2 + 4) = 1
 -      * correct answer is 4, so right and left associative do not work
 - 
 - In order to properly type-check, the operator *could* be either left or right
 - associative since its return value is an Integer and both arguments are 
 - Integers, so it would properly type check in either case. However, it 
 - *should* be non-associative. We can see examples above of how in chained
 - examples of the operator, neither l or r associativity work properly. This is
 - due to the fact that the output of the operator represents a length and the
 - inputs represent values to be summed, so passing in the length value as input
 - does not work properly.
 -}
