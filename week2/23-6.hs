-- 23.6. Да се реализира функция duplicates l, която проверява дали в списъка l има повтарящи се елементи.

-- Again sort then checking (O(N*logN + N)) is faster than naive search (O(N^2))

import Data.List (sort)
duplicates l = sortedListHasDuplicates (sort l)
    where
    sortedListHasDuplicates [] = False -- empty list has no duplicates
    sortedListHasDuplicates (x1:x2:xs)
        | x1 == x2 = True
        | otherwise = sortedListHasDuplicates (x2:xs)
    sortedListHasDuplicates (x:xs) = False; -- a list with only 1 element has no duplicates. NB - it should be after the pattern with 2 elements as it is more specific
