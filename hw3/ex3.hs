data Tree = Nil | Node Int Tree Tree

illustration = Node 2 (Node 1 Nil Nil) (Node 6 (Node 4 (Node 3 Nil Nil) (Node 5 Nil Nil)) (Node 7 Nil Nil))

treeTraversal :: Tree -> [Int]

treeTraversal (Node val left right) = treeTraversal left ++ [val] ++ treeTraversal right
treeTraversal Nil = []

treeLeaves :: Tree -> [Int]

treeLeaves (Node val left right)
    | isLeaf left right = [val]
    | otherwise = treeLeaves left ++ treeLeaves right
    where 
        isLeaf Nil Nil = True 
        isLeaf _ _ = False 
treeLeaves Nil = []

showTree :: Tree -> String

showTree (Node val left right) = "(" ++ showTree left ++ (show val) ++ showTree right ++ ")"
showTree Nil = "()"

instance Show Tree where show = showTree

sumToBranch :: Tree -> Tree

sumToBranch Nil = Nil
sumToBranch (Node val left right) = Node val (sumToBranch (addVal left val)) (sumToBranch (addVal right val)) 
    where getVal (Node val _ _)  = val
          getLeft (Node _ left _)  = left
          getRight (Node _ _ right)  = right
          addVal Nil preval = Nil
          addVal t preval = Node (getVal t + preval) (getLeft t) (getRight t)

data BinTree a = Empty | BinNode a (BinTree a) (BinTree a)

fromTreeToBinTree :: Tree -> BinTree Int

fromTreeToBinTree (Node val left right) = BinNode val (fromTreeToBinTree left) (fromTreeToBinTree right)
fromTreeToBinTree Nil = Empty

showBinTree :: (Show a) => BinTree a -> String
showBinTree Empty = "()"
showBinTree (BinNode val left right) =  "(" ++ showBinTree left ++ (show val) ++ showBinTree right ++ ")"

instance (Show a) => Show (BinTree a) where show = showBinTree

listToBranch :: BinTree a -> BinTree [a]
listToBranch t = helper t []
    where helper (BinNode val left right) vals = BinNode (vals ++ [val]) (helper left (vals ++ [val])) (helper right (vals ++ [val]))
          helper Empty vals = Empty