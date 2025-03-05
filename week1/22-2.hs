-- 22.2. Да се дефинира функция, която по двойка (x, y) коррдинати на точка
--  от равнината намира на кой квадрант принадлежи точката. Да се разгледат
--  случаите, когато точката принадлежи на някоя от координатните оси 
--  или съвпада с центъра на координатната система.

-- NB - When testing, one should input negative numbers with paranthesis, for example whichQuadrant' (-1.2) 3, as otherwise only -1.2 will throw an exception. Alternative is to use
-- function with infix notation using backticks (-1.2) `whichQuadrant'` 3

whichQuadrant x y = if x > 0
                    then
                        if y > 0 then 1 else 4
                    else
                        if x == 0 
                        then -1
                        else if y > 0 then 2 else 3

whichQuadrant' x y
    | x * y == 0 = -1
    | x * y > 0 = if x > 0 then 1 else 3
    | otherwise = if x > 0 then 4 else 2