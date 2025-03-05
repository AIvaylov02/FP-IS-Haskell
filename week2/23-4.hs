-- 23.4. Да се реализира функция sublist l1 l2, която проверява дали всички елементи на 
-- l1 са елементи и на l2.

-- Some problems in the task - l1 and l2 are not sorted, they may be multisets
-- Blindly searching for each element is O(N^2), we have to sort them first
-- Since we have to incorporate searches, we can assume the lists are finite

-- Naive approach is to iterate through the smaller list and copy the big list 
--       (cross out seen already elements, so we have to dedicate a number for already seen)

-- I am lazy to write my own merge sort, so I will use the stl one
import Data.List (sort) -- will import only the sort method from Data.List

-- not needed since we must compare them in the helper function
--sublist [] l2 = True -- will cover the case [] [] also
--sublist l1 [] = False
sublist l1 l2 = let sortedL1 = sort l1
                    sortedL2 = sort l2
                in sublistOfSortedLists sortedL1 sortedL2
                where 
                sublistOfSortedLists [] l2 = True 
                sublistOfSortedLists l1 [] = False
                sublistOfSortedLists (x1:l1) (x2:l2)
                    | x1 == x2 = sublistOfSortedLists l1 l2
                    | x1 < x2 = False -- since the lists are sorted, element x1 will never be present
                    | otherwise = sublistOfSortedLists (x1:l1) l2 -- don't pop the first element, it may be contained later