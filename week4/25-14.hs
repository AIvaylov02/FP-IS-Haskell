-- 25.14. Да се дефинира функция removeDuplicates l, която премахва всички повторения на елементите на списъка l.

-- without sorting

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates l = removeDupsHelper l []
  where 
    removeDupsHelper :: (Eq a) => [a] -> [a] -> [a]
    removeDupsHelper [] _ = []
    removeDupsHelper (h:t) alreadySeenList -- NB - the checks in the already seen list are slow, that is why sorting is faster, but we will lose the original placement
      | elem h alreadySeenList = removeDupsHelper t alreadySeenList -- element is already present, skip it
      | otherwise = h:(removeDupsHelper t (h:alreadySeenList))-- element is new