module Fortunes where
import Data.Char

main :: IO ()
main =
  do fortunesContents <- readFile "fortunes.txt"
     let fortunes = lines fortunesContents
     putStr "What is your name: "
     name <- getLine
     putStrLn $ "Hello " ++ name ++ " your fortune is:"
     putStrLn (fortunes !! (indexOfName name `mod` length fortunes))

indexOfName :: String -> Int
indexOfName name = sum [ord c | c <- name]
