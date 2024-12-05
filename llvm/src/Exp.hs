{-# LANGUAGE GADTs #-}
module Exp where

data UnaryOp = Negate
             | Abs
             | Exp
             | Log
             | Sin
             | Cos
             deriving (Eq, Show)

data Exp a where
  Const :: a -> Exp a
  Var :: Exp Double
  Unary :: UnaryOp -> Exp Double -> Exp Double
  Add :: Exp Double -> Exp Double -> Exp Double
  Sub :: Exp Double -> Exp Double -> Exp Double
  Mul :: Exp Double -> Exp Double -> Exp Double
  Div :: Exp Double -> Exp Double -> Exp Double

deriving instance Show a => Show (Exp a)

instance Num (Exp Double) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = Const . fromInteger
  negate = Unary Negate
  abs = Unary Abs
  signum = undefined

instance Fractional (Exp Double) where
  (/) = Div
  fromRational = Const . fromRational

instance Floating (Exp Double) where
  pi = Const pi
  exp = Unary Exp
  log = Unary Log
  sin = Unary Sin
  cos = Unary Cos
  asin = undefined
  acos = undefined
  atan = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

eval :: Exp a -> Double -> a
eval (Const k) _        = k
eval Var x              = x
eval (Unary Negate a) x = - eval a x
eval (Unary Abs a) x    = abs (eval a x)
eval (Unary Exp a) x    = exp (eval a x)
eval (Unary Log a) x    = log (eval a x)
eval (Unary Sin a) x    = sin (eval a x)
eval (Unary Cos a) x    = cos (eval a x)
eval (Add a b) x        = eval a x + eval b x
eval (Sub a b) x        = eval a x - eval b x
eval (Mul a b) x        = eval a x * eval b x
eval (Div a b) x        = eval a x / eval b x
