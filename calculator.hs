import Debug.Trace
--This is un-inspectable, so we cannot derive show or pattern match our operators.
--data Token = Number Double | Oper (Double -> Double -> Double) 
--this is bad because it lacks fidelity
--data Token = Number Double | Op Char deriving Show

--this is valid, but we votd against it
{-
data Token = NumTok Double | MultTok | DivTok | PlusTok | MinusTok deriving Show
data Expr = NumE Double | MultE Expr Expr | DivE Expr Expr | PlusE Expr Expr| MinusE Expr Expr   deriving (Show, Eq)
-}

inputOne = "- + 3 79 * 8 2"
inputTwo = "* + 3 - 79 8 2"

--tokensOne = [Minus, Plus, Number 3, Number 79, Mult, Number 8, Number 2]

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

data Token = NumTok Double | OpTok Operator deriving (Eq, Show)
data Operator  = Mult | Div | Plus | Minus deriving (Eq, Show)
data Expr = NumE Double | OpE Operator Expr Expr deriving (Show, Eq)

tokensA = [NumTok 3]
tokensB = [OpTok Plus, NumTok 3, NumTok 79]
tokensW = [OpTok Minus, NumTok 79, OpTok Mult, NumTok 8, NumTok 2]
tokensV = [OpTok Minus, OpTok Mult, NumTok 8, NumTok 2, NumTok 79]
tokensOne = [OpTok Minus, OpTok Plus, NumTok 3, NumTok 79, OpTok Mult, NumTok 8, NumTok 2]

exprSize :: Expr -> Int
exprSize (NumE x) = 1
exprSize (OpE op lft rgt) = 1 + exprSize lft + exprSize rgt

parse :: [Token] -> Expr
parse [] = error "Cannot parse the empty string"
parse (NumTok x:ts) = {- traceShow (NumTok x:ts) $-} NumE x 
parse (OpTok op:ts) = 
  let lft = parse ts
      rgt = parse (drop (exprSize lft) ts)
  in OpE op lft rgt

--parse2 :: [Token] -> Expr
parse2 [] = error "Cannot parse the empty string"
parse2 toks = aux toks 
  where aux :: [Token] -> (Expr, [Token])
        aux (NumTok x:ts) = (NumE x, ts)
        aux (OpTok op:ts) = 
          let (lft, leftOvers) = aux ts
              (rgt, rightOvers) = aux leftOvers
          in (OpE op lft rgt, rightOvers)


treeA, treeB, treeC, treeOne :: Expr
treeA = NumE 3
treeB = OpE Plus (NumE 3) (NumE 79)
treeC = OpE Mult (NumE 8) (NumE 2)
treeW = OpE Minus (NumE 79) treeC 
treeV = OpE Minus treeC (NumE 79)
treeOne = OpE Minus treeB treeC 

type Value = Double

eval :: Expr -> Value
eval (NumE x) = x 
eval (OpE op lft rgt) = (evalOp op) (eval lft) (eval rgt)
  
evalOp :: Operator -> (Double -> Double -> Double)  
evalOp Plus = (+)
evalOp Minus = (-)
evalOp Mult = (*)
evalOp Div = (/)
{-
eval (OpE op lft rgt) = 
  let lftVal = eval lft
      rgtVal = eval rgt
  in case op of
      Plus -> lftVal + rgtVal
      Minus -> lftVal - rgtVal
      Mult -> lftVal * rgtVal
      Div -> lftVal / rgtVal


eval (OpE Plus lft rgt) = (eval lft) + (eval rgt)
eval (OpE Mult lft rgt) = (eval lft) * (eval rgt)
eval (OpE Minus lft rgt) = (eval lft) - (eval rgt)
eval (OpE Div lft rgt) = (eval lft) / (eval rgt}
-}
