import Debug.Trace

{- 1) If there are no students in a group, it is sorted.
 - 2) Picked a student to be the pivot (head)
 - 3) asked birthday and split the remaining students into younger and older
 - 4) recursively sort both younger and older 
 - 5) Put the younger students to the left, the pivot in the middle,
 - and the older student to the right.
 -}

quickSort :: (Show a, Ord a) => [a] -> [a]
quickSort [] = []
quickSort lst =
    let pivot   = head lst
        younger = [x | x <- tail lst, x <= pivot]
        older   = [x | x <- tail lst, x > pivot]
    in traceShow lst $ (quickSort younger) ++ [pivot] ++ (quickSort older)

egcd a b
  | (a == b)  = a
  | (a > b)   = traceShow (a,b) $ egcd (a-b) b
  | otherwise = traceShow (a,b) $ egcd a (b-a)

hanoi 0 s m t = []
hanoi n s m t =
  let firstHalf  = hanoi (n-1) s t m
      middleStep = [(n,s,t)]
      backHalf   = hanoi (n-1) m s t
  in firstHalf ++ middleStep ++ backHalf
