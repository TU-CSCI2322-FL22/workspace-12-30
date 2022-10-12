import Data.List (nub)

intersect lstA lstB = [x | x <- lstA, x `elem` lstB]
union lstA lstB = nub (lstA ++ lstB)
crossProduct lstA lstB = [ (x,y) | x <- lstA, y <- lstB]
subset lstA lstB = lstA == lstA `intersect` lstB
subset2 lstA lstB = null [x | x <- lstA, x `notElem` lstB]
powerset lstS = [[]] ++ [ [x] | x <- lstS] ++ [ [x,y] | x <- lstS, y <- lstS, x < y] 
                ++ [ [x,y,z] | x<-lstS, y<-lstS, z<-lstS, x<y, y<z ]++ [lstS]

len :: [a] -> Int
len [] = 0
len (x:xs) = 1+len xs

numNonZero :: [Int] -> Int
numNonZero [] = 0
numNonZero (0:xs) = numNonZero xs
numNonZero (x:xs) = 1+ numNonZero xs

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

prodPositive :: (Num a, Ord a) => [a] -> a
prodPositive [] = 1
prodPositive (x:xs) = 
  let ppXs = prodPositive xs
  in if x < 0 then ppXs else x*ppXs

biggest :: Ord a -> [a] -> a
biggest [] = 0
biggest (x:xs) =
  let bigXs = biggest xs
  in max x bigXs 
