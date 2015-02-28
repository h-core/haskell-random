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
  mul, add :: a -> a -> a
  lit :: Integer -> a

instance Expr ExprT where
  mul a b = (Mul a b)
  add a b = (Add a b)
  lit a = (Lit a)

instance Expr Integer where
  mul a b = a * b
  add a b = a + b
  lit a = a

instance Expr Bool where
  lit a
    | a <= 0 = False
    | otherwise = True
  mul a b = a && b
  add a b = a || b

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Ord, Eq, Show)

instance Expr MinMax where
  mul a b
    | a > b = b
    | otherwise = a
  add a b
    | a < b = b
    | otherwise = a
  lit a = MinMax a

instance Expr Mod7 where
  mul (Mod7 a) (Mod7 b) = lit (mod (mul a b :: Integer) 7)
  add (Mod7 a) (Mod7 b) = lit (mod (add a b :: Integer) 7)
  lit a = (Mod7 a)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMinMax = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7

-- testInteger = testExp :: Maybe Integer
-- instance Expr Integer where

-- evalStr :: String -> Maybe Integer
-- evalStr x = maybe Nothing eval (parseExp Lit Add Mul x)

-- instance Num ExprT where
--   (Lit a) + (Lit b)     = a + b
--   (Lit a) * (Lit b)     = a * b
--   abs (Lit a)           = abs a
--   signum (Lit a)        = signum a
--   fromInteger (Lit a)   = fromInteger a
--   negate (Lit a)        = negate a
