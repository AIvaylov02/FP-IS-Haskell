-- Да се напише функция, която по дадено естествено число n (n ≥ 1) намира броя на тези
-- елементи от серията числа i^3 + 13 × i × n + n^3 , i = 1, 2, ..., n, които са кратни на 5 или на 9.

-- Task is when given a fixed n, then calculate the nums S(i,n) from the formula = i^3 + 13 * i * n + n^3
-- and get the count of the ones, which are mod 5 == 0 || mod 9 == 0

calculateSeriesNumber :: (Integral a) => a -> a -> a
calculateSeriesNumber n i = i^3 + 13 * i * n + n^3

countDivisorsFromSeries :: (Integral a) => a -> Int
countDivisorsFromSeries n = length (filterDivisors (getSeriesNumbers n n))
    where
    filterDivisors :: (Integral a) => [a] -> [a]
    filterDivisors [] = []
    filterDivisors (h:t) = if h `mod` 5 == 0 || h `mod` 9 == 0 
                           then h:(filterDivisors t) 
                           else filterDivisors t

    getSeriesNumbers :: (Integral a) => a -> a -> [a]
    getSeriesNumbers n 1 = [calculateSeriesNumber n 1]
    --getSeriesNumbers n i = (calculateSeriesNumber n i):(getSeriesNumbers n (i - 1))
    -- to get it into right order, we can append with ++
    getSeriesNumbers n i = (getSeriesNumbers n (i - 1)) ++ [calculateSeriesNumber n i]
    -- TAKEAWAY - ':' (cons operator) appends a head (scalar value) to a list (tail) <head_value>:<tail_list>
    -- WHILE ++ operator concatenates 2 lists (appends the second to the end of the first one) -> so we use always append the current value to end of list. That way the list is sorted without reverse
    -- When we calculate a number, we have to create a list for it and append the list to the end of the current one