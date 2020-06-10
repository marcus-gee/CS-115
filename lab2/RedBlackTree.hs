module RedBlackTree where


-- A color is either red or black.
data Color = Red | Black
  deriving Show

-- A red-black tree is either a leaf or a tree node with a color,
-- two branches, both of which are trees, and a value of type a.
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
  deriving Show
  
  
  
-- A.1
-- Return True if the given element is in the tree.
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member e (Node _ l val r) | e == val  = True
                          | e < val   = member e l
                          | otherwise = member e r
                                     
-- A.2    
-- Convert a tree to a list.
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ l e r) = (toList l) ++ (e : (toList r))

-- A.3                               
-- Insert a new element into a tree.
insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t)
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right)
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red a x b) y c) z d =
        Node Red (Node Black a x b) y (Node Black c z d) --case 1
    balance Black (Node Red a x (Node Red b y c)) z d =
        Node Red (Node Black a x b) y (Node Black c z d) --case 2
    balance Black a x (Node Red (Node Red b y c) z d) = 
        Node Red (Node Black a x b) y (Node Black c z d) --case 3
    balance Black a x (Node Red b y (Node Red c z d)) = 
        Node Red (Node Black a x b) y (Node Black c z d) --case 4  
    balance color l e r = Node color l e r  -- no balancing needed

-- A.4
-- Convert a list to a tree.
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert (Leaf) 

-- A.5
minDepth :: Tree a -> Int
minDepth Leaf = 0 
minDepth (Node _ l _ r) = 1 + min (minDepth l) (minDepth r)

maxDepth :: Tree a -> Int
maxDepth Leaf = 0 
maxDepth (Node _ l _ r) = 1 + max (maxDepth l) (maxDepth r)

-- A.6 
{- inputs are tree and 'global' min and max values that subtree elements must
 - also be less or greater than -}
testInvariant1_help :: Ord a => Tree a -> Maybe a -> Maybe a -> Bool 
{- single leaf -> True -}
testInvariant1_help Leaf _ _ = True
testInvariant1_help (Node _ l e r) Nothing Nothing 
            = (testInvariant1_help l Nothing (Just e)) && 
              (testInvariant1_help r (Just e) Nothing)
testInvariant1_help (Node _ l e r) (Just min) Nothing 
    | (e > min) = (testInvariant1_help l (Just min) (Just e)) && 
                  (testInvariant1_help r (Just e) Nothing)
    | otherwise = False
testInvariant1_help (Node _ l e r) Nothing (Just max)
    | (e < max) = (testInvariant1_help l Nothing (Just e)) && 
                  (testInvariant1_help r (Just e) (Just max))
    | otherwise = False
testInvariant1_help (Node _ l e r) (Just min) (Just max)
    | (e < max) && (e > min) = (testInvariant1_help l (Just min) (Just e)) && 
                               (testInvariant1_help r (Just e) (Just max))
    | otherwise = False

testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 tree = testInvariant1_help tree Nothing Nothing

-- A.7 
testInvariant2 :: Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node Black l _ r)   = (testInvariant2 l) && (testInvariant2 r)
testInvariant2 (Node Red (Node Red _ _ _) _ _) = False
testInvariant2 (Node Red _ _ (Node Red _ _ _)) = False
testInvariant2 (Node Red l@(Node Black _ _ _) _ r@(Node Black _ _ _)) = 
    (testInvariant2 l) && (testInvariant2 r)
{- all other patterns are true -}
testInvariant2 _ = True
    
-- A.8
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = (leafCounts left (n + 1)) ++ 
                                             (leafCounts right (n + 1)) 
    leafCounts (Node Red left _ right) n = (leafCounts left n) ++ 
                                           (leafCounts right n)

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False

-- B
-- We define Set as a type synonym for Tree.
type Set a = Tree a

-- Empty set.
empty :: Set a
empty = Leaf

-- Convert a list to a set
toSet :: Ord a => [a] -> Set a
toSet = fromList

-- B.1
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset s1 s2 = all (\x -> member x s2) (toList s1)

-- B.2
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = (isSubset s1 s2) && (isSubset s2 s1)

-- B.3
union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr insert s1 (toList s2)

-- B.4
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = foldr (\x r -> if member x s1
                                    then insert x r 
                                    else r) empty (toList s2) 
                                    
-- B.5
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = foldr (\x r -> if not (member x s2)
                                  then insert x r 
                                  else r) empty (toList s1)



