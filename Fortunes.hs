module Fortunes where
import Data.Char

main :: IO ()
main =
  do fortunesContents <- readFile "fortunes.txt"
     let fortunes = lines fortunesContents
     name <- prompt "What is your name"
     putStrLn $ "Hello " ++ name ++ "!"
     index <- return $ indexOfName name `mod` length fortunes
     tellFortune fortunes index

tellFortune :: [String] -> Int -> IO ()
tellFortune fortunes index = do
     putStr "Your fortune is: "
     putStrLn (fortunes !! index)
     answer <- prompt "Would you like another"
     if map toLower answer `elem` ["yes", "okay", "y", "si", "yeah", "sure", "1"]
     then tellFortune fortunes (index+1)
     else return ()

prompt :: String -> IO String
prompt question = do
  putStr $ question ++ ": "
  response <- getLine
  return response


indexOfName :: String -> Int
indexOfName name = sum [ord c | c <- name]
