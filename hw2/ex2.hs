eratoSieve :: Int -> [Int] -> [Int]

eratoSieve m intList = filter (\a -> not (mod a m == 0)) intList

listOfPrimes :: Int -> [Int]

listOfPrimes n =
        let
            table = [2..n]
        in
            helper table
        where
            helper table
                | null table = []
                | otherwise  = head table : helper (eratoSieve (head table) (tail table))

listOfPrimesPowerOfTwoMinusOne :: Int -> [Int]

listOfPrimesPowerOfTwoMinusOne n =
            helper (listOfPrimes n) []
        where
            isInt x = x == fromInteger (round x)

            helper sources store
                | null sources = store
                | isInt (logBase 2.0 (fromIntegral ((head sources) + 1))) = (head sources) : helper (tail sources) store
                | otherwise = helper (tail sources) store

decompInPrimes :: Int -> [Int]

decompInPrimes n = 
            helper (listOfPrimes n) n []
        where
            findMultiple (x:xs) target
                | null xs = -1
                | mod target x == 0 = x
                | otherwise = findMultiple xs target

            helper sources target store
                | elem target sources = target : store
                |  otherwise = (findMultiple sources target) : helper sources (div target  (findMultiple sources target)) store