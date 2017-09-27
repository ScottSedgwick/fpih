module Main where

data Tree = Leaf | Node Int Tree Tree deriving (Show)

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x l r) = x + treeSum l + treeSum r

isSortedTree :: Tree -> Bool
isSortedTree t = ist t minBound maxBound

ist :: Tree -> Int -> Int -> Bool
ist Leaf _ _ = True
ist (Node x l r) minVal maxVal = x >= minVal && x < maxVal && ls && rs
  where
    ls = ist l minVal x
    rs = ist r x maxVal

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
addNewMax (Node x t1 t2)   = Node x t1 (addNewMax t2)

main :: IO ()
main = do
    let t1 = Node 4 (Node 3 (Node 2 Leaf Leaf) (Node 1 Leaf Leaf)) (Node 5 Leaf Leaf)
    print $ treeDepth t1
    print $ treeSum t1
    print $ isSortedTree t1
    let t2 = (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
    print $ isSortedTree t2
    print t2
    print $ addNewMax t2
