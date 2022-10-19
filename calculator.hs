--This is un-inspectable, so we cannot derive show or pattern match our operators.
--data Token = Number Double | Oper (Double -> Double -> Double) 
--this is bad because it lacks fidelity
--data Token = Number Double | Op Char deriving Show

--this is valid, but we votd against it
data Token = NumTok Double | MultTok | DivTok | PlusTok | MinusTok deriving Show
data Expr = NumE Double | MultE Expr Expr | DivE Expr Expr | PlusE Expr Expr| MinusE Expr Expr   deriving (Show, Eq)

inputOne = "- + 3 79 * 8 2"
inputTwo = "* + 3 - 79 8 2"
data Token = NumTok Double | OpTok Operator deriving (Eq, Show)
data Operator  = Mult | Div | Plus | Minus deriving (Eq, Show)
data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)

--tokensOne = [Minus, Plus, Number 3, Number 79, Mult, Number 8, Number 2]
tokensOne = [OpTok Minus, OpTok Plus, NumTok 3, NumTok 79, OpTok Mult, NumTok 8, NumTok 2]

lexer :: String -> [Token]
lexer str = map lexWord (words str)
  where lexWord :: String -> Token
        lexWord "+" = OpTok Plus
        lexWord "-" = OpTok Minus
        lexWord "*" = OpTok Mult
        lexWord "/" = OpTok Div
        lexWord str = 
          if all (`elem` '.':['0'..'9']) str
          then NumTok (read str)
          else error $ "Invalid string in lexer: " ++ str


data BST = EmptyTree | Node Int BST BST deriving (Show, Eq)

treeA, treeB, treeC, treeOutput :: Expr
treeA = NumE 3
treeB = OpE Plus (NumE 3) (NumE 79)
treeC = OpE Mult (NumE 8) (NumE 2)
treeOne = OpE Minus treeB treeC 

type Value = Double

parse :: [Token] -> Expr
parse = undefined

eval :: Expr -> Value
eval = undefined

