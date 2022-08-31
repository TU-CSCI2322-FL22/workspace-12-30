aRectangle = [(0,0), (1,0), (1,1), (0,1)]
schedule = ("Jane McCrazyOverload", ["CSCI 2322", "CSCI 2320", "Math 3326", "CSCI 3320", "BIO 3446"])

aNum = 5
aString = "Hello"
aList = [1,2,3]
aFloat = 7.2
aTuple = (5, "Bob")
aTriple = (7,9,12)
aListOfTuples = [(x,2) | x <- [1..]]
etraFunList = reverse [1..]

removeUpper :: String -> String
removeUpper str = [x | x <- str, not (x `elem` ['A'..'Z']) ]

addThree :: Integer -> Integer -> Integer -> Integer
addThree x y z = x + y + z + aNum
addThreeTup :: (Integer, Integer, Integer) -> Integer
addThreeTup (x,y,z) = x + y + z + aNum


crossProduct :: [a] -> [b] -> [(a, b)]
crossProduct xs ys = [(x,y) | x <- xs, y <- ys]


