{- 25.11. Да се дефинира функция pack l :: [a] → [[a]], която получава списък и връща списък от списъци, 
      като всеки от тях съдържа всички последователни еднакви елементи на входния списък.
      Например, pack [1, 1, 1, 2, 2, 3, 4, 4, 4, 4] → [[1, 1, 1], [2, 2], [3], [4, 4, 4, 4]].
-}

-- How can we solve the problem? Maybe create tuples of (element, count) which will later be expanded into a subarray
-- -> for that we can take the 25-12.hs file and just refactor the result at the end from encode for it to be split into correct subarrays

-- repElements creates a subArray when invoked from what element and how many repetitions of it
repElement :: a -> Int -> [a]
repElement _ 0 = []
repElement elem times = elem : repElement elem (times - 1)

pack :: Eq a => [a] -> [[a]]
pack [] = [] -- the edge case should be handled in the top function
pack l = createArrayOfSubArrays l 1
  where
    -- the helper function will traverse the whole list - when a pack is changed, it will create and apped a sequence of the currently placed elements
    createArrayOfSubArrays :: (Eq a) => [a] -> Int -> [[a]] -- pack helper function. KEY POINT - COUNT FROM 1
    createArrayOfSubArrays (currElem:[]) times = [repElement currElem times] -- we should count from 1
    createArrayOfSubArrays (h1:h2:t) times
      | h1 == h2 = createArrayOfSubArrays (h2 : t) (times + 1)
      | otherwise = (repElement h1 times) : createArrayOfSubArrays (h2 : t) 1



-- Alternative solution using the definition for encoding from task 25-12.hs
pack' :: Eq a => [a] -> [[a]]
pack' l = transformEncodedArrayIntoPackedOne (encodeFromOne l)
  where
    -- custom function to transform an encoded array to packed one
    transformEncodedArrayIntoPackedOne :: [(Int, a)] -> [[a]]
    transformEncodedArrayIntoPackedOne [] = []
    transformEncodedArrayIntoPackedOne ((times, elem) : t) = 
      (repElement elem times) : transformEncodedArrayIntoPackedOne t

    -- function from 25-12.hs to transform a list into an encoded one (by array I meant list, I know they are not the same :D)
    -- Careful for the identitations, we don't want to nest something which is not necessarily part of the wrapper function
    encodeFromOne :: (Eq a) => [a] -> [(Int, a)]
    encodeFromOne [] = []
    encodeFromOne l = encodeFromOneHelper l 1 -- since the list is non-empty, we can safely assume it contains at least one element
      where
        encodeFromOneHelper :: (Eq a) => [a] -> Int -> [(Int, a)]
        encodeFromOneHelper (h:[]) times = [(times, h)]
        encodeFromOneHelper (h1:h2:t) times
          | h1 == h2 = encodeFromOneHelper (h2 : t) (times + 1)
          | otherwise = (times, h1) : encodeFromOneHelper (h2 : t) 1