--22.5. Да се напише функция, която намира сумата на цифрите в десетичния запис на дадено естествено число.
sumDigits :: Int -> Int
sumDigits x
    | x < 10 = x
    | otherwise = x `mod` 10 + sumDigits (x `div` 10)