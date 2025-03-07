{-
25.10. Да се дефинира функция decode l :: [(Int, a)] → [a], която получава списък от двойки и връща списък, 
който съдържа всички елементи на входния списък, като всеки елемент се повтаря толкова пъти, колкото е указано в първия елемент на двойката.
-}

decode :: [(Int, a)] -> [a]
decode [] = []
decode (hPair:t) = (expand hPair) ++ decode t
  where expand :: (Int, a) -> [a]
        expand (0, _) = []
        expand (times, ch) = ch : expand (times - 1, ch) 