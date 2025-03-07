-- 25.6. Да се дефинира функция, която проверява дали даден низ е палиндром, т.е. дали се е еднакъв при четене от ляво на дясно и от дясно наляво.

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome str = str == reverse str 