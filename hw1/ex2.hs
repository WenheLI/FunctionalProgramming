import Data.List

zero = \s z -> z
one = \s z -> s z
two = \s z -> s (one s z)
three = \s z -> s (two s z)

churchDecode church = church (\z -> z+1) 0

churchPrint church = church (\z -> "succ " ++ z) "zero"

churchOddEven church = church (\z -> mod (z + 1) 2) 0

churchInc church = \s z -> s (church s z)

churchAdd num1 num2 = \s z -> num1 s (num2 s z)

churchMult num1 num2 = \s z -> num1 (num2 s) z

churchPower num1 num2 = \s z -> (num2 num1) s z

churchCompare m n = let num1 = churchDecode m
                        num2 = churchDecode n
                    in
                        if num1 == num2 then EQ
                        else if num1 > num2 then GT
                        else LT

churchSort l = sortBy churchCompare l

printChurchList = map churchDecode