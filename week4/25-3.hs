{- 25.3. Да се дефинира функция countEvenOddl, която за списъка от цели числа l връща 
     наредена двойка от броя на четните и броя на нечетните числа в l.
-}

countEvenOddl :: (Integral a) => [a] -> (a, a)
countEvenOddl [] = (0,0)
countEvenOddl (h:t)
  | h `mod` 2 == 0 = (firstArg + 1, secondArg)
  | otherwise = (firstArg, secondArg + 1)
  where resultCountTillNow = countEvenOddl t
        firstArg = fst resultCountTillNow
        secondArg = snd resultCountTillNow