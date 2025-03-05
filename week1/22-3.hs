--22.3. Да се дефинира функция, която има стойност истина, ако посоченото 
--  условие е вярно и стойност - лъжа, в противен случай:

-- Todo add declarations to the functions

-- а) цялото число p се дели на 4 или на 7;
isDivisable :: Integral a => a -> Bool
isDivisable p = p `mod` 4 == 0 || p `mod` 7 == 0

-- б) уравнението ax2 + bx + c = 0(a ̸= 0) няма реални корени;
hasRealRoots :: (Num a, Ord a) => a -> a -> a -> Bool
hasRealRoots a b c = let discriminant = b * b - 4 * a * c in
                     discriminant < 0

-- в) точка с координати (a, b) лежи във вътрешността на кръг с радиус 5 и център (0, 1); 
dist x1 y1 x2 y2 = let xDiff = x1 - x2
                       yDiff = y1 - y2
                    in sqrt (xDiff * xDiff + yDiff * yDiff)
-- distance to center needs to be less than radius
isInsideCircle a b = let realDist = dist 0 1 a b in 
                     realDist < 5

-- г) точка с координати (a, b) лежи извън кръга с център (c, d) и радиус f;
isInsideGeneralisedCircle a b c d f = let realDist = dist c d a b in 
                                      realDist < f

laysOnGeneralisedCircle a b c d f = let realDist = dist c d a b in 
                                      realDist == f

isOutsideGeneralisedCircle a b c d f = not ((isInsideGeneralisedCircle a b c d f)
                                       || (laysOnGeneralisedCircle a b c d f))


-- г) точка принадлежи на частта от кръга с център (0, 0) и радиус 5 в трети квадрант;
-- Check is inside circle and if is in 3rd quadrant
isInsideThirdQuadrantAndBasicCircle a b = a < 0 && b < 0 && isInsideGeneralisedCircle a b 0 0 5

-- д) точка принадлежи на венеца с център (0, 0) и радиуси 5 и 10;
-- is not outside the big circle (may lay on the edge) and is not in the small circle
-- isInsideRing a b = not (isOutsideGeneralisedCircle a b 0 0 10) && not (isInsideGeneralisedCircle a b 0 0 5)
-- => by Applying de Morgan not (outside big circle or inside small circle)
isInsideRing a b = not ((isOutsideGeneralisedCircle a b 0 0 10) || (isInsideGeneralisedCircle a b 0 0 5))

-- е) x принадлежи на отсечката [0, 1];
isInsideZeroAndOne :: (Ord a, Num a) => a -> Bool
isInsideZeroAndOne x
   | x > 1 || x < 0 = False
   | otherwise = True

-- ж) x е равно на max {a, b, c};
xIsMaximum :: Ord a => a -> a -> a -> a -> Bool
xIsMaximum x a b c = x == max a (max b c)

-- з) x е различно от max { a, b, c};
isDifferentFromMaximum x a b c = not $ xIsMaximum x a b c

-- и) нито едно от числата a, b и c не е положително;
-- maybe there is a way to put all the arguments into an array and use recursive list checks for the head
allAreNonPositive :: (Num a, Ord a) => a -> a -> a -> Bool
allAreNonPositive a b c
   | a > 0 || b > 0 || c > 0 = False
   | otherwise = True

-- к) цифрата 7 влиза в записа на положителното трицифрено число p;
-- p is greater than 0, we can iteratively delete integerly by 10
sevenIsPresent :: Int -> Bool
sevenIsPresent x
   | x == 0 = False
   | x `mod` 10 == 7 = True
   | otherwise = sevenIsPresent (x `div` 10)

-- л) цифрите на трицифреното число m са различни;
-- optimally a result list would be created and we will just compare the values (to be able to generate the general case)
digitsAreDifferent x = let first = x `div` 100
                           second = (x `div` 10) `mod` 10
                           third = x `mod` 10 in
                        not (first == second || second == third || first == third)

-- м) поне две от цифрите на трицифреното число m са равни помежду си;
someDigitsAreSame x = not (digitsAreDifferent x)

-- н) цифрите на трицифреното естествено число x образуват строго растяща или строго намаляваща редица;
convertNumberToList x = reverse (getDigits x)
   where getDigits x 
            | x `div` 10 == 0 = [x `mod` 10]
            | otherwise = (x `mod` 10):(getDigits (x `div` 10))

-- [x:xs] is incorrect as it means a list containg a single element x:xs creates a list by default, so in square brackets means a list of list
-- (x:xs) decomposses the list into a head (x) and tail part (xs)
digitsCreateStrictSeries x = let convertedNumber = convertNumberToList x in
                              seriesIsStrictlyAscendingOrDescending convertedNumber True || seriesIsStrictlyAscendingOrDescending convertedNumber False
                                 where seriesIsStrictlyAscendingOrDescending [] _ = True -- Using pattern matching is faster and clearer than using guards (|) -> empty list
                                       seriesIsStrictlyAscendingOrDescending [_] _ = True -- list with only one element, since we don't care for the isAscending and the element itself, we use _
                                       seriesIsStrictlyAscendingOrDescending (x:xs) isAscending
                                          | isAscending == True = if x >= head xs then False else seriesIsStrictlyAscendingOrDescending xs isAscending -- it will do the decomposing himself
                                          | otherwise = if x <= head xs then False else seriesIsStrictlyAscendingOrDescending xs isAscending 

-- о) десетичните записи на трицифрените естествени числа x и y са симетрични;
listToNumber (x:xs)
   | null xs = x
   | otherwise = x * 10 ^ (length xs) + listToNumber xs 

reverseNumber x = listToNumber (reverse (convertNumberToList x))

areSymetrical x y = x == reverseNumber y

-- п) естественото число x, за което се знае, че е по-малко от 23, е просто.
isPrime :: Int -> Bool -- the semantic must be as expected in the declaration, convert types afterwards if needed
isPrime x
   | x <= 1 = False
   | otherwise = isPrime' x 2
      where isPrime' :: Int -> Int -> Bool
            isPrime' x divisor 
               | (fromIntegral divisor) == (sqrt (fromIntegral x)) = True --divisor > (sqrt x) = True -- optimisation from numbers theory (we don't have to iterate through x, just to above sqrt x)
               | otherwise = if x `mod` divisor == 0 
                  then False
                  else isPrime' x (divisor + 1)