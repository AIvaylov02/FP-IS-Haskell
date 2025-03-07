-- 25.5. Да се дефинира функция, която в даден низ замества всички малки латински букви със съответните им големи латински букви.
import Data.Char (ord, chr)

toUpper :: [Char] -> [Char]
toUpper [] = []
toUpper (h:t) = (if (isSmallLetter h) then capitalize h else h) : toUpper t
  where isSmallLetter ch = let code = ord ch
                               firstLowerCode = ord 'a'
                               lastLowerCode = ord 'z' in
                            firstLowerCode <= code && code <= lastLowerCode
        capitalize ch = let code = ord ch
                            smallToBigLetterOffset = (ord 'a') - ord 'A' in
                        chr (code - smallToBigLetterOffset)