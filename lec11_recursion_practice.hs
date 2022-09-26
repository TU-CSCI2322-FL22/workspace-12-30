import Data.List
import Debug.Trace

sorted :: Ord a => [a] -> Bool
--sorted lst = (lst == sort lst)
sorted [] = True
sorted (x:lst) = aux x lst
  where aux x [] = True
        aux x (y:xs) = (x < y) && (aux y xs) 

--sorted should return true if the list is sorted smallest to largest

rle :: (Show a,Eq a, Ord a) => [a] -> [(a, Int)]
rle [] = []
rle (x:lst)= aux x 1 lst
  where aux :: (Show a, Eq a) => a -> Int -> [a] -> [(a, Int)]
        aux elem count [] = (elem, count):[]
        aux elem count (x:xs) = 
          if x == elem 
          then aux elem (count+1) xs
          else (elem, count):(aux x 1 xs)

rle2 :: (Eq a, Ord a) => [a] -> [(a, Int)]
rle2 [] = []
rle2 [x] = [(x,1)]
rle2 (x:xs)= 
  let (elem,count):rest = rle2 xs
  in if x == elem 
     then (elem, count+1):rest
     else (x,1):(elem,count):rest

--run length encoding turns a word into tuples of runs of a letter
--rle "aaabbbbcca" = [('a',3), ('b', 4), ('c', 2), ('a', 1)]
