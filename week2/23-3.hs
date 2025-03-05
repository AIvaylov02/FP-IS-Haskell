-- 23.3 Да се реализира функция index x l, която намира поредния номер на първото срещане 
--      на елемента x в l. Например index 7 [1,2,7,3,2] -> 3.

-- note we count from 1 and not from 0, operator !! counts from 0. Here we have to implement findIndex
index :: (Eq a) => a -> [a] -> Int 
index x l = indexHelper x l 0 -- alternative is to start with 1 here instead of line 10, which has '+ 1'
    where
    indexHelper x [] curr = -1 -- element is not present
    indexHelper x (h:xs) curr
        | x == h = curr + 1 -- since the task wants to index from 0 and not from 0
        | otherwise = indexHelper x xs (curr + 1)

