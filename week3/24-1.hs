-- 24.1. Да се съставят следните списъци:
--  а) Първите n четни числа;
firstN = [2,4..]

--  б) Първите n члена на аритметична прогресия с първи член a и разлика d;
arithmeticProgression n a d = take n [a, a + d..] -- за да е само редица, то интерактивно се подават, а не се извежда като функция

--  в) [1!, 2!, ..., n!] за дадено n;
factorial 1 = 1
factorial n = n * factorial (n - 1)

factorialsList 1 = [1]
factorialsList n = factorialsList (n - 1) ++ [factorial n] -- will cycle the recursion down to the very bottom

factorialsList' 1 = [1]
factorialsList' n = let previousElements = factorialsList' (n - 1) in
                    previousElements ++ [(last previousElements) * n] -- only bottleneck is that we need to get to the last element of the list
    
-- lets store them in reverse (constant time to first element that way)
factorialsList'' n = reverse $ factorialsListHelper n
factorialsListHelper 1 = [1]
factorialsListHelper n = let previousElements = factorialsListHelper (n - 1) in
                        ((head previousElements) * n) : previousElements

--  г) Всички четни числа;
evenNumbers = [2,4..]

--  д) Всички членове на аритметична прогресия с първи член a и разлика d;
arithmeticProgressionInfinite a d = [a, a + d..]

--  е) [1!, 2!, ...] (безкраен списък)
-- This time we will start from the bottom up and won't reverse the list

{- This is slow because of ++, DONT use ++ on infinite lists
infiniteFactorialsList = infiniteFactorialsListHelper 2 [1] 
infiniteFactorialsListHelper n currList = let newNumber = (n * last currList) in
                                          infiniteFactorialsListHelper (n + 1) (currList ++ [newNumber])
-}

infiniteFactorialsList :: [Integer]
infiniteFactorialsList = factorialHelper 1 1
    where
    factorialHelper n productUntilNow = productUntilNow : factorialHelper (n + 1) (productUntilNow * (n + 1))