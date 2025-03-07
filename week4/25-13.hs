{- 25.13. Да се дефинира функция remove x l, която премахва: 
(а) първото срещане на елемента x от списъка l
(б) всички срещания на елемента x от списъка l.
-}

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (h:t) = if x == h
                 then t -- if the current element is the one searched, return only the tail (skip current)
                 else h:(remove x t) -- if not, then prepend the head to the list with the removed element 
                 -- (remove x t only will just skip the entire part before the match)

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (h:t) = if x == h 
                    then removeAll x t 
                    else h:(removeAll x t)