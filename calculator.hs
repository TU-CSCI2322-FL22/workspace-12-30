--This is un-inspectable, so we cannot derive show or pattern match our operators.
--data Token = Number Double | Oper (Double -> Double -> Double) 
--this is bad because it lacks fidelity
--data Token = Number Double | Op Char deriving Show

--this is valid, but we votd against it
--data Token = Number Double | Mult | Div | Plus | Minus deriving Show

data Token = Number Double | Op Operator deriving Show
data Operator  = Mult | Div | Plus | Minus deriving Show

inputOne = "- + 3 79 * 8 2"
--tokensOne = [Minus, Plus, Number 3, Number 79, Mult, Number 8, Number 2]
tokensOne = [Op Minus, Op Plus, Number 3, Number 79, Op Mult, Number 8, Number 2]

data Expr = Undefined2
type Value = Double

lexer :: String -> [Token]
lexer str = map lexWord (words str)
  where lexWord :: String -> Token
        lexWord = undefined

parse :: [Token] -> Expr
parse = undefined

eval :: Expr -> Value
eval = undefined

inputTwo = "* + 3 - 79 8 2"
