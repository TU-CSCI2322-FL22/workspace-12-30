isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (_:_) = False

isZero :: Integer -> Bool
isZero 0 = True
isZero x = False

lucky :: Integer -> String
lucky 3 = "Lucky"
lucky 7 = "So every lucky"
lucky 21 = "The luckiest"
lucky _ = "Not very lucky."

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors vectA vectB = (fst vectA + fst vectB, snd vectA + snd vectB)

type Vector = (Double, Double)
plusVectors :: Vector -> Vector -> Vector
plusVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

addPairs :: [(Int, Int)] -> [Int]
addPairs lst = [x + y | (x,y) <- lst] 


addDoubletons :: [[Int]] -> [Int]
addDoubletons lst = [x + y | [x,y] <- lst] 

startsWithSeven (7:_) = True
startsWithSeven lst = False
--startsWithSeven (x:xs) = False

tell :: Show a => [a] -> String
tell [] = "The list is empty."
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:xs) = "The list has many elements, the first two are: " ++ show x ++ " and " ++ show y

{- endsWithSeven (lst ++ [7]) = True
endsWithSeven _ = False
-}

drop5 [] = []
drop5 lst@(x:xs) = if x == 5 then xs else lst


