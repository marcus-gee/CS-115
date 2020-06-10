module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)
  

-- C.1
sparseMatrix :: (Eq a, Num a) =>
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix _ (r, c) | (r < 1) || (c < 1) = error("invalid bounds")
sparseMatrix [] bound = SM bound S.empty S.empty M.empty
sparseMatrix lst b@(r,c) 
    | all (\((i, j), _) -> (i <= r) && (j <= c)) lst /= True = error("invalid"
                                                                  ++  " bounds")
    | otherwise = SM b rows columns (M.fromList vals)
      where 
        vals = filter (\(_, v) -> v /= 0) lst  -- non-zero values
        rows = S.fromList (map (\((i, _), _) -> i) vals)
        columns = S.fromList (map (\((_, j), _) -> j) vals)
 
-- C.2        
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM b1 _ _ v1) (SM b2 _ _ v2) 
    | b1 /= b2  = error("bounds must be equal")
    | otherwise = SM b1 rows columns vals
      where 
        vals = M.filter (/= 0) (M.unionWith (+) v1 v2) -- sum the union of maps
        rows = S.fromList (map (\(i, _) -> i) (M.keys vals))
        columns = S.fromList (map (\(_, j) -> j) (M.keys vals))
-- C.3       
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b r c v) = SM b r c (M.map (\x -> -x) v)

-- C.4
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM sm1 sm2 = addSM sm1 (negateSM sm2)

-- C.5
mulHelp :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> 
                                                    Integer -> Integer -> a
mulHelp (SM _ _ _ v1) (SM _ _ _ v2) i j = (sum products)
    where
        r = M.filterWithKey (\(r, _) _ -> r == i) v1 -- row to be multiplied
        c = M.filterWithKey (\(_, c) _ -> c == j) v2 -- col to be multiplied
        r_intersect = M.mapKeys (\(_, c) -> c) r -- find where the rows and cols
        c_intersect = M.mapKeys (\(r, _) -> r) c -- intersect and vals are mult
        -- multiply the correspond row/col intersection values
        products = M.elems (M.intersectionWith (*) r_intersect c_intersect)
        
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM s1@(SM (r1,c1) ri _ _) s2@(SM (r2,c2) _ ci _) 
    | c1 /= r2 = error("invalid dimensions for matrix multiplication")
    | otherwise = (sparseMatrix m_lst (r1, c2))
      where 
        non_empty_r = S.toList ri
        non_empty_c = S.toList ci
        m_lst = [((i, j), (mulHelp s1 s2 i j)) 
                | i <- non_empty_r, j <- non_empty_c]
        
        
-- C.6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (r,c) _ _ v) (ri, ci)
    | (ri > r) || (ri < 1) || (ci > c) || (ci < 1) = error("invalid index")
    | otherwise = M.findWithDefault 0 (ri,ci) v

rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM (r, _) _ _ _) = r

colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM (_, c) _ _ _) = c

-- C.7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<|!|>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<|!|>) = getSM 

-- C.8 
{- It would not make much sense to define the SparseMatrix as an instance of the
 - Num type class as that class is used for number representations, whereas, our
 - SparseMatrix is a collection of numbers with properties and attributes that
 - are not typical of normal numbers. 
 -}

















 
 
  
