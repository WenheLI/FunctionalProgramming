quicksort ::(Ord a) => [a] -> [a]

quicksort x
    | null x = x
    | otherwise = 
        let 
            pivot = head x
            rest = tail x
            leftPart = filter (\val -> val <= pivot) rest
            rightPart = filter (\val -> val > pivot) rest
        in
            quicksort leftPart ++ [pivot] ++ quicksort rightPart

-- The core mechanism of quicksort is to compare two elements, thereofore, as long as the input type is comparable, we can apply quick sort.
-- Thus, we can rewrite a more generic type (Ord a) to ensure comparable.