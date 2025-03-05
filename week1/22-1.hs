-- 22.1. Да се дефинира функция, която намира лицето на триъгълник по дадени: 
--    а) дължини на страна и височина към нея; 
--    б) три страни.

-- a)
triangleAreaByHeight :: Fractional a => a -> a -> a
triangleAreaByHeight side height = side * height / 2
-- b)
triangleArea :: Double -> Double -> Double -> Double
triangleArea a b c = 
    let halfPerimeter = (a + b + c) / 2
        areaOnPow2 = halfPerimeter * (halfPerimeter - a) * (halfPerimeter - b) * (halfPerimeter - c)
        in sqrt areaOnPow2