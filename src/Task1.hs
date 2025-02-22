{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving Show

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- WAS "A.B.."
-- NOW "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- WAS ".A.B."
-- NOW ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- WAS "...BA"
-- NOW "...BA"
--
torder :: Order    -- ^ Order of resulting traversal
       -> Maybe a  -- ^ Optional leaf value
       -> Tree a   -- ^ Tree to traverse
       -> [a]      -- ^ List of values in specified order
torder PreOrder opleaf tree = preOrder opleaf tree
torder InOrder opleaf tree = inOrder opleaf tree
torder PostOrder opleaf tree = postOrder opleaf tree



preOrder :: Maybe a -> Tree a -> [a]
preOrder opleaf Leaf  = maybeToList opleaf
preOrder opleaf (Branch val left right) = [val] ++ preOrder opleaf left ++ preOrder opleaf right


inOrder :: Maybe a -> Tree a -> [a]
inOrder opleaf Leaf  = maybeToList opleaf
inOrder opleaf (Branch val left right) = inOrder opleaf left ++ [val] ++ inOrder opleaf right


postOrder :: Maybe a -> Tree a -> [a]
postOrder opleaf Leaf  = maybeToList opleaf
postOrder opleaf (Branch val left right) = postOrder opleaf left ++ postOrder opleaf right ++ [val]


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]




-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- WAS WAS ".|C..|A.B.."
-- WAS NOW ".C..A.B.."
-- NOW ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- WAS WAS ".|.C.|.A.B."
-- WAS NOW "..C..A.B."
-- NOW ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- WAS WAS ".|..C|...BA"
-- WAS NOW "...C...BA"
-- NOW ".|..C|...BA"
--
forder :: Order     -- ^ Order of tree traversal
       -> Maybe a   -- ^ Optional separator between resulting tree orders
       -> Maybe a   -- ^ Optional leaf value
       -> Forest a  -- ^ List of trees to traverse
       -> [a]       -- ^ List of values in specified tree order
forder _ _ _ [] = []
forder order _ opleaf [x] = torder order opleaf x       
forder order separator opleaf (x:xs) = torder order opleaf x ++ maybeToList separator ++ forder order separator opleaf xs

