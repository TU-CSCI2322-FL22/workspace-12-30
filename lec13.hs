evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) = 
  if (x `mod` 2) == 0
  then x:(evens xs)
  else evens xs

evens2 lst = filter even lst
evens3 lst = filter p lst
  where p x = x `mod` 2 == 0
evens4 lst = filter (\x -> x `mod` 2 == 0) lst
evens5 = filter even 

quiet :: String -> String
quiet "" = ""
quiet (c:cs) = 
  if c `elem` ' ':['a'..'z']
  then c:(quiet cs)
  else quiet cs

collect :: (a -> Bool) -> [a] -> [a]
collect p [] = []
collect p (x:xs) = 
  if p x 
  then x:(collect p xs)
  else collect p xs

add7 x = x + const
  where const = 7

f = (\x -> x + 3)

singletons lst = map (\x -> [x]) lst

addedNums = zipWith (+) [7,3,1] [0,2,5]

scores = [10, 45, 13, 92, 42, 19]
names = ["Jack", "Jane", "Joe", "Josh", "Jonas", "Jimothy"]
report = zipWith (++) (map (++ ": ") names) (map show scores) 
report2 = zipWith (\name score -> name ++ ": " ++ show score) names scores
--make a list report tht has ["Jack: 10", "Jane: 45", ...]
