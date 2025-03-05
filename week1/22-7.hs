-- 22.7. Да се напише функция, която по дадени естествено число x и едноцифрено числа k намира 
--   а) дали k се среща в десетичния запис на x
--   б) колко пъти k се среща в десетичния запис на x.
isPresent x k
    | x < 10 = x == k
    | otherwise = if x `mod` 10 == k then True else isPresent (x `div` 10) k

timesPresent :: Integral a => a -> a -> Int
timesPresent x k
    | x < 10 = if x == k then 1 else 0
    | otherwise = timesPresent (x `div` 10) k + if x `mod` 10 == k then 1 else 0