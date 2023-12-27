module MyLib where

data Tree = Leaf | Node Tree Int Tree
  deriving (Show)

toList :: Tree -> [Int]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

isBST :: Tree -> Bool
isBST Leaf = True
isBST (Node l x r) = all (< x) (toList l) && all (> x) (toList r) && isBST l && isBST r

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = Node l y r

size :: Tree -> Int
size Leaf = 0
size (Node l _ r) = 1 + size l + size r

member :: Int -> Tree -> Bool
member _ Leaf = False
member x (Node l y r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True