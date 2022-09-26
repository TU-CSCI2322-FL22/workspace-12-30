add13, add7, add9, sub2 :: Int -> Int
add13 x = x + 13
add7 x = x + 7
add9 x = x + 9
sub2 x = x - 2

addY y x = x + y

add22 x = addY 22 x

addYAll ::Int -> [Int] -> [Int]
addYAll y [] = []
addYAll y (x:xs) = (x+y):(addYAll y xs)

gradedFunction :: Int -> (Int -> Int)
gradedFunction a b = (addY 100 a) `div` (addY 20 b)

add13All :: [Int] -> [Int]
add13All [] = []
add13All (x:xs) = (x+13):(add13All xs)

absAll :: [Int] -> [Int]
absAll [] = []
absAll (x:xs) = (abs x):(absAll xs)

singletons :: [a] -> [[a]]
singletons [] = []
singletons (x:xs) = ([x]):(singletons xs)
singletons2 lst = map makeList lst
  where makeList x = [x]

applyAll :: (a -> b) -> [a] -> [b]
applyAll f [] = []
applyAll f (x:xs) = (f x):(applyAll f xs)

cutOff :: [Int] -> [Int]
{-cutOff [] = []
cutOff (x:xs) = (max 0 x):(cutOff xs) -}
cutOff lst = map (max 0) lst 
cutOff2 lst = [max 0 x | x <- lst]

--shoutNumbers :: [Int] -> [String]
shoutNumbers lst = map (++"!") (map show lst)
shoutNumbers2 lst = [(show x)++"!" | x <- lst]
shoutNumbers3 lst = map shoutF lst
  where shoutF x = (show x)++"!"
