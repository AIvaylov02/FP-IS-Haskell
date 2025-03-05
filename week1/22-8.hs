--22.8. Да се напише функция, която проверява дали дадена година е високосна.
-- Wikipedia : This extra leap day occurs in each year that is a multiple of 4, except for years evenly divisible by 100 but not by 400.

-- So a number which is either divisable by 400 or only by 4 (not by 100)
isLeap :: Integral a => a -> Bool  -- if definition not given, then error occurs
isLeap y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | otherwise = y `mod` 4 == 0