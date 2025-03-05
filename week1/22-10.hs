-- Да се дефифира функция, която по естествени числа n и k намира, дали n е точна степен на числото k.
-- isExactPower k k = True -> this is a conflict
isExactPower 0 0 = True
isExactPower 1 k = True -- 1 is a power to each number, since k^0 is exactly 1
isExactPower n 1 = False -- cannot be any other number than 1
isExactPower n 0 = False -- other than 0^k = 0, no other matches it
isExactPower n k 
    | n == k = True
    | n `mod` k /= 0 = False
    | otherwise = isExactPower (n `div` k) k