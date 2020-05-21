data Tree = Leaf | Node Tree Tree deriving (Eq)
countNode :: Tree -> Int
countNode Leaf = 0
countNode (Node left right) = 1 + countNode left + countNode right


removeDuplicate :: (Eq a) => [a] -> [a]
removeDuplicate = foldl (\xs x -> if x `elem` xs 
                                    then xs
                                    else x : xs) []

genTreeHelper :: Int -> [Tree]
genTreeHelper num 
  | num == 0 = [Leaf]
  | num == 1 = [Node Leaf Leaf]
  | otherwise = treeMaker (genTree (num - 1))
    where 
      updateParents (Node Leaf Leaf) update = [(Node update Leaf), (Node Leaf update)]

      updateParents (Node right Leaf) update = [(Node right update)] ++ map (\t -> Node t Leaf) (updateParents right update)

      updateParents (Node Leaf right) update = [(Node update right)] ++ map (\t -> Node Leaf t) (updateParents right update)

      updateParents (Node right left) update = (map (\(x1, x2) -> Node x1 x2) (zip(updateParents right update) [left])) ++ (map (\(x1, x2) -> Node x1 x2) (zip [right] (updateParents left update)))

      treeMaker (p: parents) = updateParents p (Node Leaf Leaf) ++ (treeMaker parents)
      treeMaker [] = []

genTree :: Int -> [Tree]

genTree num = removeDuplicate (genTreeHelper num)

mirrorTree :: Tree -> Tree
mirrorTree (Node left right) = 
              Node (mirrorTree right) (mirrorTree left)
mirrorTree Leaf = Leaf

genSymTree :: Int -> [Tree]
genSymTree num = filter (\t -> t == mirrorTree t)  (genTree num)

fromTreeToWordHelper :: Tree -> String
fromTreeToWordHelper (Node Leaf Leaf) = ""
fromTreeToWordHelper (Node left Leaf) = "XY" ++ (fromTreeToWordHelper left)
fromTreeToWordHelper (Node Leaf right) = "YX" ++ (fromTreeToWordHelper right)
fromTreeToWordHelper (Node left right) = "XX" ++ (fromTreeToWordHelper left) ++ "YY" ++ (fromTreeToWordHelper right)

fromTreeToWord :: Tree -> String
fromTreeToWord t = "X" ++ (fromTreeToWordHelper t) ++ "Y"

genDyckWord :: Int -> [String]
genDyckWord num = map fromTreeToWord (genTree num)

permutate :: [String] -> [String] -> [String] -> String -> Int -> Int -> [String]
permutate [] [] store curr _ _ = curr : store
permutate (x:xs) [] store curr _ _ = permutate xs [] store (curr ++ x) 0 0
permutate [] (y:ys) store curr _ _ = permutate [] ys store (curr ++ y) 0 0
permutate (x:xs) (y:ys) store curr prevX prevY = if prevX > prevY 
then (permutate xs (y:ys) store (curr ++ x) (prevX+1) prevY) ++ (permutate (x:xs) ys store (curr ++ y) prevX (prevY+1))
else permutate xs (y:ys) store (curr ++ x) (prevX+1) prevY

genDyckWordNative :: Int -> [String]
genDyckWordNative num = removeDuplicate (permutate (take num (cycle ["x"])) (take num (cycle ["y"])) [] "" 0 0)

instance Show Tree where show = fromTreeToWord