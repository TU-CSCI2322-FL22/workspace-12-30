import Debug.Trace

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + (mySum xs)
mySum2 nums = katamari (+) 0 nums

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs
myProduct2 nums = katamari (*) 1 nums

myConcat :: [String] -> String
myConcat [] = ""
myConcat (s:strs) = s ++ (myConcat strs)
myConcat2 strs = katamari (++) "" strs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
myLength2 :: [a] -> Int
myLength2 elems = katamari (\x acc -> 1+acc) 0 elems

combineNums, combineNums2 :: [Int] -> String
combineNums [] = ""
combineNums (x:xs) = (show x) ++ "," ++ (combineNums xs)
combineNums2 nums = foldr (\x acc -> (show x) ++ "," ++ acc) "" nums
combineNums3 nums = foldl (\acc x -> (show x) ++ "," ++ acc) "" nums

katamari :: (a -> b -> b) -> b -> [a] -> b
katamari f b [] = b
katamari f b (x:xs) = x `f` katamari f b xs

iramatak :: (b -> a -> b) -> b -> [a] -> b
iramatak f b [] = b
iramatak f b (x:xs) = iramatak f (b `f` x) xs
