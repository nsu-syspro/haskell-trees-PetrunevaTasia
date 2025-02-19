{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..))

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving Show

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare x y
  | x == y = EQ
  | x < y = LT
  | x > y = GT
  | otherwise = error "Can't compare"

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST _ [] = Leaf
listToBST _ [x] = Branch x Leaf Leaf
listToBST func (x : xs) = loop func (Branch x Leaf Leaf) xs


loop :: Cmp a -> Tree a -> [a] -> Tree a
loop _ tree [] = tree
loop func tree [x] = tinsert func x tree
loop func tree (x : xs) = loop func (tinsert func x tree) xs


instance Eq Ordering where
  (==) :: Ordering -> Ordering -> Bool
  (==) LT LT = True
  (==) EQ EQ = True
  (==) GT GT = True
  (==) _ _ = False

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList Leaf = []
bstToList (Branch x y z) = bstToList y ++ [x] ++ bstToList z

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST func tree = compareList func (bstToList tree)

compareList :: Cmp a -> [a] -> Bool
compareList _ [] = True
compareList _ [_] = True
compareList func (x : y : xs) = func x y == LT && compareList func (y : xs)

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup func x (Branch y left right)
  | func x y == EQ  = Just y
  | func x y == LT = tlookup func x left
  | func x y == GT = tlookup func x right
  | otherwise = error "Can't do tlookup"

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ x Leaf = Branch x Leaf Leaf
tinsert func x (Branch y left right)
  | func x y == EQ = Branch x left right
  | func x y == LT = Branch y (tinsert func x left) right
  | func x y == GT = Branch y left (tinsert func x right)
  | otherwise = error "Can't do tinsert"

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete func x (Branch y left right)
  | func x y == LT = Branch y (tdelete func x left) right
  | func x y == GT = Branch y left (tdelete func x right)
  | isNotEmpty left && isNotEmpty right = Branch (findMin right) left (tdelete func (findMin right) right)
  | isNotEmpty left = left
  | isNotEmpty right = right
  | otherwise = Leaf

isNotEmpty :: Tree a -> Bool
isNotEmpty Leaf = False
isNotEmpty _ = True

findMin :: Tree a -> a
findMin (Branch x Leaf _) = x 
findMin (Branch _ left _) = findMin left
findMin _ = error "Can't find min"