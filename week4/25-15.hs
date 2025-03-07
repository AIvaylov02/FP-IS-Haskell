{- 25.15. За даден списък L, да се намерят елементите на списъка, чиято стойност е по-голяма от сумата на предхождащите ги елементи. 
        Пример [1, 2, 5, 9, 16] → [1, 2, 5, 9].
-}

{- One solution is to make a temporary list of tuples (list_element, sum_till_now), 
than filter the tuples where the list_elem is > sum_till_now and map only the list_elems. Let's do it with recursion without temp lists
-}

elementsGreaterThanAllPreviousCombined :: (Ord a, Num a) => [a] -> [a]
elementsGreaterThanAllPreviousCombined (h:[]) = [h]
{-  A nasty edge case, since if we throw a list of [0] it will treated as invalid value in the below expression.
    You should not select a different default sum value than 0, since this is the neutral element of the operation addition.
-}
elementsGreaterThanAllPreviousCombined l = currElementGreaterThanPreviousCombined l 0
  where
    currElementGreaterThanPreviousCombined :: (Ord a, Num a) => [a] -> a -> [a]
    currElementGreaterThanPreviousCombined [] _ = []
    currElementGreaterThanPreviousCombined (h:t) accumulatedSum =
        let newSum = accumulatedSum + h
            restOfList = currElementGreaterThanPreviousCombined t newSum in
        if h > accumulatedSum then h:restOfList else restOfList