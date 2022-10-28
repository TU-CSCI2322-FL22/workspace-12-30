
class Bhoulish a where
  boo :: a -> Bool
  boo x = exam x True False
  candy :: a -> a -> a
  candy c y = exam c y c
  exam :: a -> b -> b -> b 
  exam x tCase fCase = if boo x then tCase else fCase
  trick, treat :: a

instance Bhoulish Bool where
  --boo :: Bool -> Bool
  boo bool = bool
  --exam :: Bool -> b -> b -> b 
  exam bool tCase fCase = if bool then tCase else fCase
  {-exam True tCase fCase = tCase 
  exam False tCase fCase = fCase -}
  trick = False
  treat = True

instance Bhoulish Char where
  boo c = c `elem` "tTyY1👍✓"  
  exam c tCase fCase = if boo c then tCase else fCase
  trick = 'n'
  treat = 'y'

instance Bhoulish Integer where
  boo x = x /= 1
  trick = 1
  treat = 0

instance Bhoulish a => Bhoulish [a] where
  boo lst = length lst == 2
  trick = []
  treat = [trick, treat]

spooky :: Bhoulish a => [a] -> [a]
spooky lst = [x | x <- lst, boo x]
  
  
data Operator = Plus | Minus | Times | Div deriving (Show, Eq)
data Token = NumTok Double | OpTok Operator deriving (Show, Eq)
data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)

instance Bhoulish Operator where
  boo Plus = True
  boo Times = True
  boo _ = False
  trick = Minus
  treat = Plus

instance Bhoulish Token where
  boo (NumTok x) = boo (round x :: Integer)
  boo (OpTok op) = boo op
  trick = OpTok trick
  treat = OpTok treat

instance Bhoulish Expr where
  boo (NumE x)  = False
  boo (OpE op lft rgt)  = or [boo op, boo lft, boo rgt]
  trick = (NumE 3)
  treat = (OpE Plus trick trick)

treeA = NumE 3
treeB = OpE Plus (NumE 3) (NumE 79)
treeC = OpE Times (NumE 8) (NumE 2)
treeW = OpE Minus (NumE 79) treeC 
treeV = OpE Minus (NumE 4) (NumE 79)
