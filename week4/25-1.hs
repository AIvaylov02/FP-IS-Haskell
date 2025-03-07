-- 25.1. Дефинирайте функция say, която по едноцифрено цяло число връща неговото наменование. Например, say 3 → ”three”.

say :: Int -> [Char]
say 0 = "zero"
say 1 = "one"
say 2 = "two"
say 3 = "three"
say 4 = "four"
say 5 = "five"
say 6 = "six"
say 7 = "seven"
say 8 = "eight"
say 9 = "nine"

-- alternative better syntax for long cases on only a single operand (using case of instead of pattern matching on function arguments)
-- case of applies pattern matching on expressions
say' :: Int -> [Char]
say' n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"