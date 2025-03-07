-- 25.9. Да се дефинира функция flatten l :: [[a]] → [a], която получава списък от списъци и 
--   връща списък, който съдържа всички елементи на входните списъци.

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten l = helperFlatten $ reverse l -- Key point is where the reverse is - not on the RESULT LIST BUT MOREOVER ON THE ENTRY LIST
  where helperFlatten :: [[a]] -> [a]
        helperFlatten (subArr:[]) = subArr
        helperFlatten (hArr:tailOfArrays) = (helperFlatten tailOfArrays) ++ hArr 
-- '++' cannot be escaped since we always merge lists with non-scalar values into a single one