module Fortunes where
import System.IO
import System.Environment
import Text.Read (readMaybe)
import Data.Char
import System.Console.GetOpt


data Flag = Help | Once | Num String | Start String deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)    "Print usage information and exit."
          , Option ['o'] ["once"] (NoArg Once)    "Print fortunes once, and then exit."
          , Option ['n'] ["num"] (ReqArg Num "#")    "Print # fortunes at a time."
          , Option ['s'] ["start"] (ReqArg Start "#")    "Start at fortune #, instead of asking name."
          ]

getNum :: [Flag] -> Int
getNum [] = 1
getNum ((Num x):_) = 
  case readMaybe x of
    Nothing -> error "Invalid input to num flag"
    Just num -> num
getNum (_:flags) = getNum flags

getStart :: [Flag] -> IO Int
getStart ((Start x):_) = 
  case readMaybe x of
    Nothing -> error "Invalid input to start flag"
    Just num -> return num
getStart (_:flags) = getStart flags
getStart [] = 
  do name <- prompt "What is your name"
     putStrLn $ "Hello " ++ name ++ "!"
     return $ indexOfName name 


main :: IO ()
main =
  do args <- getArgs
     let (flags, inputs, error) = getOpt Permute options args
     let fname = if null inputs then "fortunes.txt" else head inputs
     fortunesContents <- readFile fname
     let fortunes = lines fortunesContents
     if Help `elem` flags || (not $ null error)
     then putStrLn $ usageInfo "Usage: Fortunes [options] [file]" options
     else do
       index <- getStart flags
       (chooseAction flags) index fortunes

chooseAction :: [Flag] -> Int -> [String] -> IO ()
chooseAction flags 
  | Once `elem` flags = tellFortunesOnce (getNum flags)
  | otherwise = tellFortune 

getFortunes :: Int -> Int -> [String] -> [String]
getFortunes count start fortunes =
  take count $ drop (start-1) (cycle fortunes)

tellFortunesOnce :: Int -> Int -> [String] -> IO ()
tellFortunesOnce count index fortunes = do
     putStr "Your fortune is: "
     putStrLn $ unlines $ getFortunes count index fortunes

tellFortune :: Int -> [String] -> IO ()
tellFortune index fortunes = do
     putStr "Your fortune is: "
     putStrLn $ unlines $ getFortunes 1 index fortunes
     answer <- prompt "Would you like another"
     if map toLower answer `elem` ["yes", "okay", "y", "si", "yeah", "sure", "1"]
     then tellFortune (index+1) fortunes 
     else return ()

prompt :: String -> IO String
prompt question = do
  putStr $ question ++ ": "
  hFlush stdout
  response <- getLine
  return response


indexOfName :: String -> Int
indexOfName name = sum [ord c | c <- name]
