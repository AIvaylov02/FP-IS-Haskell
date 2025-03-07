{- 25.4. Да се дефинира функция pivot l x, която за списъка от числа l и числото x връща наредена двойка (l1, l2), 
където l1 е списък от елементите на l, по-малки от x, а l2 е списък от елементите на l, по-големи или равни на x.
-}

-- Idea is to implement the pivot function from the quick sort algorithm - when we take a number for comparison, 
-- give the 2 lists of numbers which are greater or smaller than current

pivot :: (Ord a, Num a) => [a] -> a -> ([a], [a])
pivot [] _ = ([], [])
pivot (h:t) x
  | h < x = (h:smallerList, biggerList) -- append to left list
  | otherwise = (smallerList, h:biggerList) -- append to right list
  where resultUntilNow = pivot t x
        smallerList = fst resultUntilNow
        biggerList = snd resultUntilNow