{- 25.12. Да се дефинира функция encode l :: [a] -> [(Int, a)], която получава списък и връща списък от двойки, където първият елемент на двойката 
     е броят на последователните еднакви елементи от входния списък, а вторият елемент е самият елемент.
     Например, encode [1, 1, 1, 2, 2, 3, 4, 4, 4, 4] -> [(3, 1), (2, 2), (1, 3), (4, 4)].
-}

-- tough part is when we do the recursive step between the groups (what is the times value)
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode l = encodeHelper l 0
  where
    encodeHelper :: (Eq a) => [a] -> Int -> [(Int, a)]
    encodeHelper (h:[]) times = [(times + 1, h)] -- we count from 0, offset by 1
    encodeHelper (h1:h2:t) times
      | h1 == h2 = encodeHelper (h2 : t) (times + 1)
      | otherwise = (times + 1, h1) : encodeHelper (h2 : t) 0 -- since we cound from 0, offset times by 1


-- Let's try to count from 1, like most normal people - Splendid, it works :D
encodeFromOne :: (Eq a) => [a] -> [(Int, a)]
encodeFromOne [] = []
encodeFromOne l = encodeFromOneHelper l 1 -- since the list is non-empty, we can safely assume it contains at least one element
  where
    encodeFromOneHelper :: (Eq a) => [a] -> Int -> [(Int, a)]
    encodeFromOneHelper (h:[]) times = [(times, h)]
    encodeFromOneHelper (h1:h2:t) times
      | h1 == h2 = encodeFromOneHelper (h2 : t) (times + 1)
      | otherwise = (times, h1) : encodeFromOneHelper (h2 : t) 1