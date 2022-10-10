import Data.Maybe
lookupVal :: Eq a => a -> [(a,b)] -> Maybe b
lookupVal key lst = 
  let results = [v | (k,v) <- lst, k == key]
  in if not $ null results
     then Just (head results)
     else Nothing

lettersToNums = (zip ['a'..'z'] [1..]) ++ (zip ['A'..'Z'] [1..])

wordScore :: String -> Int
wordScore [] = 0
wordScore (c:cs) = 
  case lookupVal c lettersToNums of
    Nothing  -> 1 + wordScore cs
    Just x -> x + (wordScore cs)

defaultZero :: Maybe Int -> Int
defaultZero (Just x) = x
defaultZero Nothing = 0

defaultX :: Int -> Maybe Int -> Int
defaultX x (Just y) = y
defaultX x Nothing = x

wordScore2 :: String -> Int
wordScore2 [] = 0
wordScore2 (c:cs) = (defaultX 1 $ lookup c lettersToNums) + (wordScore cs)
