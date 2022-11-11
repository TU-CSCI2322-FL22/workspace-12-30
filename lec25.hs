main :: IO ()
main = greeting

greeting :: IO ()
greeting = 
  do putStr "What is your name: "
     name <- getLine
     putStrLn $ "Hello there "  ++ name
