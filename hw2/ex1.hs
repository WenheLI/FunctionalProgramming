opStrandSeq :: String -> String

opStrandSeq [] = []
opStrandSeq (nuc:remain)
                | nuc == 'A' = "T" ++ opStrandSeq remain
                | nuc == 'T' = "A" ++ opStrandSeq remain
                | nuc == 'G' = "C" ++ opStrandSeq remain
                | nuc == 'C' = "G" ++ opStrandSeq remain
                | otherwise  = ""

isPrefix :: String -> String -> Bool

isPrefix [] [] = True
isPrefix [] str2 = True
isPrefix str1 [] = False
isPrefix (curr1: remain1) (curr2:remain2)
            | curr1 == curr2 = True && isPrefix remain1 remain2
            | otherwise  = False

isSubString :: String -> String -> Bool

isSubString str1 str2 = helper str1 str2 str1
            where helper [] [] _ = True
                  helper [] str2 _ = True
                  helper str1 [] _ = False
                  helper (curr1:remain1) (curr2:remain2) ori
                    | curr1 == curr2 = helper remain1 remain2 ori
                    | otherwise = helper ori remain2 ori

isSubSeq :: String -> String -> Bool

isSubSeq [] [] = True
isSubSeq [] str = True
isSubSeq str [] = False
isSubSeq (curr1:remain1) (curr2:remain2)
            | curr1 == curr2 = isSubSeq remain1 remain2
            | otherwise = isSubSeq (curr1:remain1) remain2

slice :: Int -> Int -> String -> String

slice start end text = take (end - start) (drop start text)

listOfSubString :: String -> [String]

listOfSubString str =
        helper str 0 1 []
      where helper str start end store
              | end == length str = helper str (start+1) (start+2) (slice start end str:store)
              | start == length str = store
              | otherwise = helper str start (end+1) (slice start end str:store)

listOfSubSeq :: String -> [String]

listOfSubSeq str =
        helper str [] []
      where
        helper str curr store
          | null str && null curr = store
          | null str = reverse curr : store
          | otherwise = helper (tail str) curr store ++ helper (tail str) (head str: curr) store

listOfEmb :: String -> String -> [[Int]]

listOfEmb str1 str2 =
          helper str1 str2 [] [] 0
        where
          helper str1 str2 curr store index
            | null str1 && not (null curr) = reverse curr : store
            | null str2 = store
            | head str1 == head str2 = helper (tail str1) (tail str2) ((index+1):curr) store (index+1) ++ helper str1 (tail str2) curr store (index+1)
            | otherwise  = helper str1 (tail str2) curr store (index+1)