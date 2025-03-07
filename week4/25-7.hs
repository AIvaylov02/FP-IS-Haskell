-- 25.7. Да се дефинира функция, която по даден списък от цели числа l връща списък от всички двойки (a, b) от l, 
--   за които a и b са съседни елементи в l и a < b.

orderedNeighbourPairs :: [Int] -> [(Int, Int)]
-- orderedNeighbourPairs [] = []
-- orderedNeighbourPairs [_] = []
orderedNeighbourPairs (h1:h2:t) = if (h1 < h2) 
                                  then (h1, h2) : orderedNeighbourPairs (h2:t) 
                                  else orderedNeighbourPairs (h2:t)
orderedNeighbourPairs _ = [] -- covers the top 2 cases