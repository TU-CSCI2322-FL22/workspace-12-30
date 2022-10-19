data Contest = Rock | Scissor | Paper deriving Show

rps :: Contest -> Contest -> String
rps Rock Scissor = "Player One Wins"
rps Scissor Paper = "Player One Wins"
rps Paper Rock = "Player One Wins"
rps Scissor Rock = "Player Two Wins"
rps Paper Scissor = "Player Two Wins"
rps Rock Paper= "Player Two Wins"
rps p1 p2 = "Tie"

data Velocity = MPS Double | FPS Double deriving Show

toMPS :: Velocity -> Double
toMPS (MPS x) = x
toMPS (FPS y) = y / 3

type Point = (Double, Double)
data Shape = Circle Point Double 
           | Rectangle Point Point deriving Show

area :: Shape -> Double
area (Circle c r) = pi * r^2
area (Rectangle (x1,y1) (x2,y2)) = abs $ (x2 - x1) * (y2 - y1)

data IOops = IAnswer Int | IMistake String deriving Show
data DOops = DAnswer Double | DMistake String deriving Show

data Oops a = Answer a | Mistake String deriving Show

safeFirst :: [b] -> Oops b
safeFirst [] = Mistake "no first element of the empty list"
safeFirst (x:xs) = Answer x

safeDivide :: Double -> Double -> Oops Double
safeDivide x 0 = Mistake "cannot divide by 0"
safeDivide x y = Answer (x/y)

data ITsil = ILlun | ISnoc ITsil Int deriving Show
data STsil = SLlun | SSnoc STsil String deriving Show

data Tsil a = Llun | Snoc (Tsil a) (a) deriving Show

tsil = Snoc (Snoc (Snoc Llun 7) 3) 5
tsil2 = Snoc (Snoc (Snoc Llun [7,3,5]) [1,2,9]) []

daeh :: Tsil a -> Maybe a
daeh Llun = Nothing
daeh (Snoc xs x) = Just x

tmp = Just []

listToTsil :: [a] -> Tsil a
listToTsil [] = Llun
listToTsil (x:xs) = (listToTsil xs) `Snoc` x

tsilToList :: Tsil a -> [a]
tsilToList Llun = []
tsilToList (xs `Snoc` x) = x:(tsilToList xs)

pam :: (a -> b) -> Tsil a -> Tsil b
pam f Llun = Llun
pam f (xs `Snoc` x) = (pam f xs) `Snoc` (f x)

