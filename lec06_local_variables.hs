myEven x | x `mod` 2 == 0 = True
         | otherwise      = False

{-equvalent to
myEven x = 
  if x `mod` 2 == 0 
  then True 
  else False
myEven x = x `mod` 2 == 0
-}

mileage (miles, gallons)
    | (mpg <= 10.0) = "Get a new car"
    | (mpg <= 20.0) = "You're doing okay."
    | (mpg <= 30.0) = "You eco-warrior you."
    | otherwise     = "You're lying, aren't you?"
  where mpg = miles /gallons
        gpm = gallons/miles

firstStr = "The first!"
lexCmp :: Ord a => [a] -> [a] -> String 
lexCmp (x:xs) (y:ys) 
  | x < y     = firstStr
  | x > y     = "The second!"
  | otherwise = "I dunno."
lexCmp [] (y:ys) = firstStr
lexCmp (x:xs) [] = "The second!"
lexCmp [] [] = "I dunno"
  
cylinder r h = 
  let topArea = pi*r^2
      sideArea = h*2*pi*r
  in 2*topArea + sideArea

sOfL strs len = [str | str <- strs, length str == len]
stringsOfLengths strs lens = [(len, lenStrs) | len <- lens, let lenStrs = sOfL len strs]

stringsOfLengths2 :: [String] -> [Int] -> [(Int, [String])]
stringsOfLengths2 strs lens = [(len, stringsFor len) | len <- lens]
  where stringsFor len = [str | str <- strs, length str == len]
