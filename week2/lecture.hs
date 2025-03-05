mylen :: [a] -> Int
mylen l = if null l then 0 else 1 + mylen (tail l) 

count x l = if null l
               then 0
               else
                    let remainingX = count x (tail l) in
                    if head l == x then 1 + remainingX else remainingX

onlyevens :: Integral a => [a] -> [a]
onlyevens l = if mylen l == 0
               then []
               else
                    let restEvens = onlyevens (tail l) -- remember to call the right function
                        curr = head l -- you could write many let and ins before and after each variable, which uses the others, but in the end, we need the value in the if only!
                    -- Key Point !!!! mod is a function, not an operator!!!
                    -- Another key point is the identation - the definitions must be nested between let and in (by convention the line ends with 'in')
                        isEven = mod curr 2 == 0 in
                    if not isEven then restEvens else curr:restEvens


-- merge l1 l2 taka che da se reduvat, kato pochvame ot l1
merge :: [a] -> [a] -> [a]
-- Either we have bool parameter, who is on turn or hear me out, we swap the arrays, so that we take only from the first array
merge l1 l2 = if null l1 && null l2
              then [] -- both l1 and l2 are empty, stop merging them
              else
                   if null l1 then l2 -- only l1 is empty, just merge all remaining symbols from l2
                   else if null l2 then l1
                   else -- we have elements in both lists, take from the first
                        let curr = head l1
                            restOfL1 = tail l1
                            rest = merge l2 restOfL1 in
                        curr:rest   
