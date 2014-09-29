{-# OPTIONS -Wall -Werror #-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

makeNode :: (Ord a) => a -> Tree a
makeNode x = Node x EmptyTree EmptyTree 

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = makeNode x
insert x (Node y left right)
	| x == y = Node y left right
	| x < y = Node y (insert x left) right
	| otherwise = Node y left (insert x right)

elem' :: (Ord a) => a -> Tree a -> Bool
elem' _ EmptyTree = False
elem' x (Node y left right)
	| x == y = True
	| x < y = elem' x left
	| otherwise = elem' x right

