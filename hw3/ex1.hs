import Data.Char (isLetter, toLower)

reverseList :: [a] -> [a]

reverseList a
    | null a = []
    | otherwise = reverseList (tail a) ++ [head a]

isPalindrome :: String -> Bool

isPalindrome x = x == reverseList x

isPalindromeStr :: String -> Bool

isPalindromeStr x = 
    let 
        processedX = map toLower (filter isLetter x)
    in
        processedX == reverseList processedX

-- `isPalindrome` does not constrain on the input type as long as the input is a list of any type and such type support `==` operator.
-- However, `isPalindromeStr` includes `toLower` & `isLetter` which force the input to be [Char]

