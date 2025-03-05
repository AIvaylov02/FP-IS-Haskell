-- Едно положително цяло число е съвършено, ако е равно на сумата от своите делители (без самото число). 
-- Например, 6 е съвършено, защото 6 = 1+2+3; числото 1 не е съвършено. 
-- Да се дефинира функция, която проверява дали дадено положително цяло число е съвършено.

getDivisors :: (Integral a) => a -> [a]
getDivisors n = getDivisorsHelper n n []
    where
    getDivisorsHelper :: (Integral a, Eq a) => a -> a -> [a] -> [a]
    getDivisorsHelper n 1 xs = 1:xs
    getDivisorsHelper n divisor xs = if n `mod` divisor == 0 
                            then divisor:(getDivisorsHelper n (divisor - 1) xs)
                            else getDivisorsHelper n (divisor - 1) xs

isPerfect :: (Integral a) => a -> Bool
isPerfect n = sum (tail (getDivisors n)) == n
           