import Debug.Trace

biggest :: (Show a, Ord a) => [a] -> a
biggest [] = error "There is no biggest element of an empty list."
biggest [x] = traceShow (x, "[]") x
biggest (x:xs) =
  let bigXs = biggest xs
  in traceShow (x, xs) $ max x bigXs

end :: [a] -> a
end = undefined
-- end "hello" 'o'

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) = 
  let countInYs = count x ys
  in if x == y 
     then 1+countInYs
     else countInYs

occurancesOfHead :: Eq a => [a] -> Int
occurancesOfHead [] = error "There is no head of the empty list"
occurancesOfHead (x:xs) = aux x xs -- simpler: 1 + count x xs or count x (x:xs)
  where aux x [] = 1
        aux x (y:ys) = 
          let occursInYs = aux x ys
          in if y == x then 1+occursInYs else occursInYs
        isThisIn x lst = x `elem` lst

--rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (traceShowId $ rev xs) ++ [x]
--rev "hello" 
--"olleh"
rev2 lst = aux [] lst
  where aux acc [] = acc
        aux acc (x:xs) = aux (x:acc) xs
