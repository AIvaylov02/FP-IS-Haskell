{-
24-5 Да се дефинира функция histogram, която по символен низ s връща списък от двойки (ci , ni ), 
където ci са различните символи от s, а ni е броя на срещания на ci в s. 
Например, histogram ”abracadabra” → [(a, 5), (b, 2), (r, 2), (c, 1), (d, 1)]. 
Използвайте помощни функции.
-}

--
-- fastest way is to use sort and just traverse neighbours
countSymbols :: [Char] -> [Char] -> [Int]
countSymbols [] _ = []
countSymbols (hSymbols:tSymbols) str = (countSymbol hSymbols str) : (countSymbols tSymbols str)  

countSymbol :: Char -> [Char] -> Int
countSymbol _ [] = 0
countSymbol ch (h:t) = (countSymbol ch t) + if ch == h then 1 else 0

-- there is operator "=="" defined for the lists
-- nub is the stl function
getSymbols :: [Char] -> [Char]
-- if it is not reversed, than it will order the symbols from right to left, which is unappropriate
getSymbols str = getSymbolsHelper (reverse str) []

-- KEY IS that we have to reverse the list first in order to start picking chars from the beginning
getSymbolsHelper :: [Char] -> [Char] -> [Char]
getSymbolsHelper [] charsTillNow = charsTillNow
getSymbolsHelper (strH:remainingStr) charsTillNow
    | elem strH remainingStr = getSymbolsHelper remainingStr charsTillNow
    | otherwise = getSymbolsHelper remainingStr (strH:charsTillNow)

histogram :: String -> [(Char,Int)]
histogram str = let symbolsArr = getSymbols str
                    countTimes = countSymbols symbolsArr str in 
                zip symbolsArr countTimes