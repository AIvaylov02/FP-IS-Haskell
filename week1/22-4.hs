--22.4. Да се напише функция, която намира броя на цифрите в десетичниязапис на дадено естествено число.

digitsCount :: Integer -> Integer 
digitsCount x = if x < 10 
                then 1 
                else 1 + digitsCount(div x 10)

-- Cleaner alternative
-- digitsCount x
--    | x < 10 = 1
--    | otherwise = 1 + digitsCount(div x 10) 