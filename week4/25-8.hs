-- 25.8. Да се дефинира функция groupsof l x, която разделя списъка l на групи от по x елемента.
--    Например, groupsof [1, 2, 3, 4, 5, 6, 7, 8] 3 → [[1, 2, 3], [4, 5, 6], [7, 8]].

groupsof :: [a] -> Int -> [[a]] -- we assume x is greater than 0, just apply the take function
groupsof [] _ = []
groupsof l x = (take x l) : groupsof (drop x l) x