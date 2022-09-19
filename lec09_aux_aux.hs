gap :: [Int] -> Int
gap [] = error "Mind the gap."
gap (x:xs) = aux x x xs 
  where aux big smol [] = big - smol
        aux big smol (x:xs)  
          | x > big  = aux x smol xs
          | x < smol = aux big x xs
          | otherwise = aux big smol xs

betterGap :: [Int] -> Int
betterGap lst = 
  let (big, smol) = aux lst
  in big - smol
  where aux :: [Int] -> (Int, Int) -- or just call range!
        aux [] = error "Mind the better gap."
        aux [x] = (x, x)
        aux (x:xs) = 
          let (big, smol) = aux xs
          in (max x big, min x smol)

range :: [Int] -> (Int, Int)
range [] = error "Mind the better gap."
range [x] = (x, x)
range (x:xs)  
    | x > big  = (x, smol)
    | x < smol = (big, x)
    | otherwise = (big, smol)
  where (big, smol) = range xs


betterGap2 :: [Int] -> Int
betterGap2 lst = undefined
  where updateRange x (big, smol)  
            | x > big  = (x, smol)
            | x < smol = (big, x)
            | otherwise = (big, smol)
        aux :: [Int] -> (Int, Int)
        aux [] = error "Mind the better gap."
        aux [x] = (x, x)
        aux (x:xs) = updateRange x (aux xs)


hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule tasks = aux 0 tasks
  where aux minutesLeft [] = 0
        aux minutesLeft ((h,m):tasks) = 
          if m > minutesLeft
          then let hoursForTs = aux (60+minutesLeft-m) tasks
               in hoursForTs + h + 1
          else let hoursForTs = aux (minutesLeft-m) tasks
               in hoursForTs + h 


