module Exercises where

data Tree a = Leaf | Tree (Tree a) a (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert key Leaf = Tree Leaf key Leaf
insert key (Tree l k r)
  | key < k = Tree (insert key l) k r
  | key > k = Tree l k (insert key r)
  | otherwise = Tree l k r

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member key (Tree l k r) = key == k || (member key l) || (member key r)

height :: Tree a -> Integer
height Leaf = 0
height (Tree l _ r) = 1 + (maximum $ map height [l, r])


main = do
  print $ foldr insert Leaf "tspipfbst"
  print $ foldr insert Leaf "abcdefghi"
