-- Едно положително цяло число е съвършено, ако е равно на сумата от своите делители (без самото число). 
-- Например, 6 е съвършено, защото 6 = 1+2+3; числото 1 не е съвършено. 
-- Да се дефинира функция, която проверява дали дадено положително цяло число е съвършено.

-- TODO refactor
isPerfect :: Int -> Bool
isPerfect n = n == sum (getDivisors n)
    where getDivisors divisor xs
            | getDivisors n 1 xs = 1:xs
            | getDivisors n divisor xs = if n `mod` divisor == 0
                                         then divisor:(getDivisors n (divisor - 1) xs)
                                         else getDivisors n (divisor - 1) xs    
           