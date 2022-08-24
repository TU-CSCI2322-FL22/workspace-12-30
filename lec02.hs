lst = [7,3,5]
lst2 = 8:lst

isEmptySlow lst = 
  if length lst > 0 
  then False 
  else True

isEmpty [] = True
isEmpty (x:xs) = False

hundred = [1..100]
hundredOdd = [1,3..100]

naturals = [1..]

weirdNums = [x | x <- hundred, x `mod` 11 == 0, x `mod` 3 == 1]

lst3 = [-20, 7,-5, 13, 8, -1]
lst4 = [if x < 0 then x*(-1) else x | x <- lst3]
absLst lst = [if x < 0 then x*(-1) else x | x <- lst]









