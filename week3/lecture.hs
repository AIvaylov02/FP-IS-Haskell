endless = (0,0) : endless

myZip [] _ = []
myZip _ [] = []
myZip (h1:t1) (h2:t2) = (h1,h2) : myZip t1 t2

get4of6 (_,_,_,x,_,_) = x -- our 4th of 6 in a 6 element vector

-- Function to return true for lists with head 7, false otherwise with pattern matching
mag7 (7 : _) = True
mag7 _ = False

-- Function to tell if list has exactly 1 element with pattern
has1Element (_:[]) = True -- has1Element [_] = True is alternative
has1Element _ = False

-- Function to sum the tail of a list with pattenrs
sumtail (_:t) = sum t

-- Function to shrint a list by summing each 2 adjacent elements (1 iteration)
-- a) if list is of odd len skip last elem
shrink (x1:x2:t) = (x1 + x2):shrink t
shrink _ = []
-- b) keep the last element if the list is of odd len
shrink' (x1:x2:t) = (x1 + x2):shrink' t
shrink' (x:_) = [x]
shrink' _ = []