{- cabal:
build-depends: base, unordered-containers, transformers
-}
{-# LANGUAGE BangPatterns #-}
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HM
import           System.Mem.StableName

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

--

type Name = StableName Exp

-- 状態：(次に使う識別子, これまでに定義した変数と式のリスト, StableNameから変数名への対応)
type M = StateT (Int, [(Id, SimpleExp)], HM.HashMap Name Id) IO

recoverSharing :: Exp -> IO ExpS
recoverSharing x = do
    (v, (_, revLets, _)) <- runStateT (go x) (1, [], HM.empty)
    pure $ foldl (\x (i, s) -> Let i s x) (Value v) revLets
  where
    makeSimpleExp :: Name -> SimpleExp -> M Value
    makeSimpleExp n s = do
      (!i, _, _) <- get
      modify $ \(_, acc, m) -> (i + 1, (i, s) : acc, HM.insert n i m)
      pure $ VarV i

    go :: Exp -> M Value
    go !x = do
      n <- lift $ makeStableName x
      (_, _, m) <- get
      case HM.lookup n m of
        Just i ->
          -- すでに出現した項であればそれを変数として参照する
          pure $ VarV i
        Nothing ->
          case x of
            Const k -> pure $ ConstV k
            Var -> pure $ VarV 0
            -- これまでに出現していない項であればletを作る
            Add y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ AddS y' z'
            Sub y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ SubS y' z'
            Mul y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ MulS y' z'
            Div y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ DivS y' z'

main :: IO ()
main = do
  let f x = (x + 1)^10
  e <- recoverSharing (f Var)
  print e
