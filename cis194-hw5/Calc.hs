import ExprT
import Parser
import Data.Maybe
--module Calc where

--eval :: ExprT -> Integer
-- class ExprT a where
-- eval :: ExprT -> a

-- instance ExprT where
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr x
  | isNothing expr = Nothing
  | otherwise = Just $ eval $ fromJust expr
    where expr = parseExp Lit Add Mul x

class Expr a where
mul, add :: ExprT -> ExprT -> ExprT
lit :: Integer -> ExprT

instance Expr ExprT where
mul a b = (Mul a b)
add a b = (Add a b)
lit a = (Lit a)

-- evalStr :: String -> Maybe Integer
-- evalStr x = maybe Nothing eval (parseExp Lit Add Mul x)

-- instance Num ExprT where
--   (Lit a) + (Lit b)     = a + b
--   (Lit a) * (Lit b)     = a * b
--   abs (Lit a)           = abs a
--   signum (Lit a)        = signum a
--   fromInteger (Lit a)   = fromInteger a
--   negate (Lit a)        = negate a
