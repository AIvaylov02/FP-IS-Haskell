-- 25.2. Да се дефинира функция, която по два списъка намира дължината на най-дългия им общ префикс

-- Meaning when given 2 lists, find the longest common part among both, when starting from the front

longestCommonPrefix :: (Eq a) => [a] -> [a] -> Int
longestCommonPrefix [] _ = 0
longestCommonPrefix _ [] = 0
longestCommonPrefix (h1:t1) (h2:t2)
  | h1 /= h2 = 0
  | otherwise = 1 + longestCommonPrefix t1 t2