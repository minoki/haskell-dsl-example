data Exp = Const Double
         | Var
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         deriving Show

instance Num Exp where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = Const . fromInteger
  abs = undefined
  signum = undefined

instance Fractional Exp where
  (/) = Div
  fromRational = Const . fromRational

eval :: Exp -> Double -> Double
eval (Const k) _ = k
eval Var x = x
eval (Add a b) x = eval a x + eval b x
eval (Sub a b) x = eval a x - eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Div a b) x = eval a x / eval b x

main :: IO ()
main = do
  let f x = (x + 1)^10
  print $ eval (f Var) 3.0 -- 1048576.0
