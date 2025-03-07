{-
25.16. Да се дефинира функция mergeevenodd l1 l2, която получава два списъка от цели числа l1 и l2 и връща списък, 
чиито елементи на четни позиции са елементите на l1, а тези на нечетни позиции са елементите на l2. 
Пример: mergeevenodd [1,2,3] [4,5,6] -> [1,4,2,5,3,6].
-}

{- Analogous to the myMerge function from lectures, but here we have some constraints.
   1) Elements from l1 are on even indexes (counting from 0), elements from l2 are on odd => no possibility for len l2 > l1
   -- worst case l1 could have an element more than l2 (nothing more)
   2) The lists are from Integers
-}

mergeevenodd :: [Int] -> [Int] -> [Int]
mergeevenodd [] _ = []
mergeevenodd (h1:t1) [] = [h1] -- case list one has more elements (we care only for the first, since we have no defined placeholders for the other indecies)
mergeevenodd (h1:t1) l2 = h1:(mergeevenodd l2 t1) -- we will put elements from first list only 