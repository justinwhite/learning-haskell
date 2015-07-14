module Tree
( Tree(..)
, treeInsert
, treeElem
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- if same value, return unaltered tree
-- recursively insert into tree otherwise
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node a left right) 
	| x == a = tree
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right)

-- recursively check if elem is in tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x == a = True
	| x < a = treeElem x left
	| x > a = treeElem x right

-- given 
-- 		let nums = [8,6,4,1,7,3,5]  
-- why does
-- 		let numsTree = foldr treeInsert EmptyTree nums  
-- work but 
-- 		let numsTree = foldl treeInsert EmptyTree nums  
-- 	fails?


