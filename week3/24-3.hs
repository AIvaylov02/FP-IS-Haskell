-- 24.3. (*) Да се дефинира функция, която по дадено естествено число n връща списък с цифрите му,
--  четени отдясно на ляво, без повторения на елементите на списъка.

-- A bit more tricky than last one, since we have to have an additional list, which keeps track of seen numbers
-- to see if a digit is inside it, just use elem -> checking for a digit is worst case O(10)

getDigitsWithoutRepetition n = getDigitsWithoutRepetitionHelper n []
    where
    getDigitsWithoutRepetitionHelper n alreadySeenDigits
        | n < 10 = if elem n alreadySeenDigits -- end of recursion, we don't care for the already seen 
                   then [] 
                   else [n]
        | otherwise = let digit = n `mod` 10 in
                          -- the below line is invalid since we change dynamically in each case the alreadySeenDigits
                      {-    restOfDigits = getDigitsWithoutRepetitionHelper (n `div` 10) alreadySeenDigits in
                      if elem digit alreadySeenDigits 
                      then restOfDigits
                      else digit : restOfDigits
                      -}
                      if elem digit alreadySeenDigits 
                      then getDigitsWithoutRepetitionHelper (n `div` 10) alreadySeenDigits
                      else digit : getDigitsWithoutRepetitionHelper (n `div` 10) (digit:alreadySeenDigits)
{-The lines 21 and 22 cannot be refactored into 1 since, they differ on 2 different places, which makes if appending impossible
-}
