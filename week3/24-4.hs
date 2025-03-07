{- 24.4. Едно положително цяло число е съвършено, ако е равно на сумата от своите делители (без самото число). 
    Например, 6 е съвършено, защото 6 = 1+2+3; числото 1 не е съвършено. 
    Да се дефинира функция, която създава списък с всички съвършени числа, ненадминаващи дадено положително цяло число в параметър n
-}

sumDivisors :: Int -> Int
sumDivisors 1 = 1
sumDivisors n = sumDivisorsHelper n (n - 1)
  where sumDivisorsHelper :: Int -> Int -> Int
        sumDivisorsHelper _ 1 = 1
        sumDivisorsHelper n divisor = sumDivisorsHelper n (divisor - 1) + 
                                      if n `mod` divisor == 0 then divisor else 0 

isPerfect :: Int -> Bool
isPerfect 1 = False
isPerfect n = n == sumDivisors n

getPerfectNumbersLowerThan :: Int -> [Int]
getPerfectNumbersLowerThan n = reverse (getPerfectNumbersLowerThanHelper n)
  where getPerfectNumbersLowerThanHelper :: Int -> [Int]
        getPerfectNumbersLowerThanHelper n
          | n == 1 = []
          | isPerfect n = n : getPerfectNumbersLowerThanHelper (n - 1)
          | otherwise = getPerfectNumbersLowerThanHelper (n - 1)