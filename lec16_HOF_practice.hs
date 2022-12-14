--Higher order functions and their types
{- ● foldl :: (b -> a -> b) -> b -> [a] -> b
-  ● foldr :: (a -> b -> b) -> b -> [a] -> b
-  ● map :: (a -> b) -> [a] -> [b]
-  ● filter :: (a -> Bool) -> [a] -> [a]
-}

-- 1. Define addPairs :: [(Int, Int)] -> [Int] to add the elements of each pair in a list.
addPairs lst = map (\elem -> fst elem + snd elem) lst
addPairs2 lst = map (\(a,b)-> a + b) lst
addPairs3 lst = map addPair lst
  where addPair (a,b) = a + b

-- 2. Define prodList :: [Int] -> Int that finds the product of all elements.
prodList lst = foldr (\x prodXs -> x * prodXs) 1 lst 

-- 3. Define zeroDetector :: [Int] -> Bool to check if there is a zero in a list
  -- a. (Optional) Using a filter and built-in functions

  -- b. Using a fold
zeroDetector [] = False
zeroDetector (x:xs) = if x == 0 then True else zeroDetector xs

zeroDetector2 lst = foldr (\x acc -> if x == 0 then True else acc) False lst
zeroDetector3 lst = foldr (\x acc -> (x == 0) || acc) False lst

-- 4. Using a fold, define positives :: [Int] -> [Int] that returns the positive elements.
positives lst= foldr (\x positivesInXs -> if x > 0 then x:positivesInXs else positivesInXs ) [] lst

-- 5. Using a fold, define range :: [Int] -> (Int,Int)

-- 6. Define splitOnParity :: [Int] -> ([Int], [Int]) to split a list into evens and odds.
  -- a. Using two filters
  -- b. Using one fold

-- 7. Define a higher-order function exists :: (a -> Bool) -> [a] -> Bool that takes a predicate, a list, and returns true if the predicate is true for anything in the list.
  -- a. Using recursion.
  -- b. (Optional) using a fold.

-- 8. Rewrite zeroDetector using only exists.

-- 9. Define sumValues :: [(a, Int)] -> Int that sums the values of an association list.

-- 10. Define bestKey :: [(a, Int)] -> a that finds the key with the best value.

-- 11. Using exists, define isPrime :: Int -> Bool
  -- a. First define isMultiple :: Int -> Int -> Bool that takes two Ints and returns True if the first can be divided by the second.
