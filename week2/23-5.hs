-- 23.5. Да се реализира функция common l1 l2, която преброява колко от елементите на l1 са елементи и на l2

-- We will modify the helper function from previous taks 23-4.hs and still use the list sort mechanism
-- common is a biective function since it doesn't matter if we compare the l1 'a' to the l2 'a' or vice versa (who starts the comparison)

import Data.List (sort) -- will import only the sort method from Data.List
common l1 l2 = let sortedL1 = sort l1
                   sortedL2 = sort l2
                in commonCountOfSortedLists sortedL1 sortedL2
                where 
                commonCountOfSortedLists [] l2 = 0 -- if a list still has elements, while the other is empty, than common is 0
                commonCountOfSortedLists l1 [] = 0
                commonCountOfSortedLists (x1:l1) (x2:l2)                
                    | x1 == x2 = 1 + commonCountOfSortedLists l1 l2
                    | x1 < x2 = commonCountOfSortedLists l1 (x2:l2) -- since the lists are sorted, element x1 may be skipped in order to align them again (l2 should be kept the same)
                    | otherwise = commonCountOfSortedLists (x1:l1) l2 -- don't pop the first element, it may be contained later. Skip the current element of l2