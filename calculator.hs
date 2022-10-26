import Text.Read
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

lexer :: String -> Maybe [Token]
lexer str = sequence $ map lexWord (words str)

seqMaybe :: [Maybe a] -> Maybe [a]
seqMaybe [] = Just []
seqMaybe (x:xs) = 
  case (x,seqMaybe xs) of
    (Just val, Just xsVals) -> Just (val:xsVals)
    _ -> Nothing

lexWord :: String -> Maybe Token
lexWord "+" = Just $ OpTok Plus
lexWord "-" = Just $ OpTok Minus
lexWord "*" = Just $ OpTok Mult
lexWord "/" = Just $ OpTok Div
lexWord str = applyMaybe NumTok (readMaybe str)

applyMaybe :: (a -> b) -> Maybe a -> Maybe b
applyMaybe f Nothing = Nothing
applyMaybe f (Just x) = Just (f x)


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

unsafeParse :: [Token] -> Expr
unsafeParse toks = 
  case aux toks of
    (expr, []) -> expr
    (expr, toks) -> error $ "Too many tokens: " ++ show toks
  where aux :: [Token] -> (Expr, [Token])
        aux [] = error "Cannot parse the empty string"
        aux (NumTok x:ts) = (NumE x, ts)
        aux (OpTok op:ts) = 
          let (lft, leftOvers) = aux ts
              (rgt, rightOvers) =   aux leftOvers
          in (OpE op lft rgt, rightOvers)

parse :: [Token] -> Maybe Expr
parse toks = 
  case aux toks of
    Just (expr, []) -> Just expr
    Just (expr, toks) -> Nothing -- error $ "Too many tokens: " ++ show toks
    Nothing -> Nothing
  where aux :: [Token] -> Maybe (Expr, [Token])
        aux [] = Nothing -- error "Cannot parse the empty string"
        aux (NumTok x:ts) = Just (NumE x, ts)
        aux (OpTok op:ts) = 
          case aux ts of
            Nothing -> Nothing
            Just (lft, leftOvers) -> 
              case aux leftOvers of
                Nothing -> Nothing
                Just (rgt, rightOvers) -> Just (OpE op lft rgt, rightOvers)
{-
parseOld :: [Token] -> Expr
parseOld [] = error "Cannot parse the empty string"
parseOld (NumTok x:ts) = {- traceShow (NumTok x:ts) $-} NumE x 
parseOld (OpTok op:ts) = 
  let lft = parseOld ts
      rgt = parseOld (drop (exprSize lft) ts)
  in OpE op lft rgt
  -}

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

rep :: String -> Maybe Value
rep str = 
  case lexer str of
    Nothing -> Nothing
    Just tokens -> applyMaybe eval (parse tokens)

main = do
  str <- getLine
  case rep str of
    Just val -> putStrLn $ show val
    Nothing -> putStrLn "Invalid line. Please try again."
  main

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

