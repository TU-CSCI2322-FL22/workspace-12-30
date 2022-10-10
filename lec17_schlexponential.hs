import Debug.Trace
foo [] = 0
foo (x:xs) = 
  let result = foo xs
  in result + (2 + result) 

uniques [] = []
uniques (x:xs) = 
  if x `elem` xs
  then uniques xs
  else x:(uniques xs)
