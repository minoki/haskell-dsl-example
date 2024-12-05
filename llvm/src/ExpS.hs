{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
module ExpS where
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT (runStateT), get,
                                                   modify)
import qualified Data.HashMap.Strict as HM
import           Exp
import           System.Mem.StableName (StableName, makeStableName)

type Id = Int -- 変数の識別子

data Value a = ConstV a
             | VarV Id
             deriving Show

-- letの右辺の式
data SimpleExp a where
  UnaryS :: UnaryOp -> Value Double -> SimpleExp Double
  AddS :: Value Double -> Value Double -> SimpleExp Double
  SubS :: Value Double -> Value Double -> SimpleExp Double
  MulS :: Value Double -> Value Double -> SimpleExp Double
  DivS :: Value Double -> Value Double -> SimpleExp Double

deriving instance Show (SimpleExp a)

-- 共有を表現できる式
data ExpS a = Let Id (SimpleExp Double) (ExpS a)
            | Value (Value a)
            deriving Show

type Name = StableName (Exp Double)

-- 状態：(次に使う識別子, これまでに定義した変数と式のリスト, StableNameから変数名への対応)
type M = StateT (Int, [(Id, SimpleExp Double)], HM.HashMap Name Id) IO

recoverSharing :: Exp Double -> IO (ExpS Double)
recoverSharing expr = do
    (v, (_, revLets, _)) <- runStateT (go expr) (1, [], HM.empty)
    pure $ foldl (\x (i, s) -> Let i s x) (Value v) revLets
  where
    makeSimpleExp :: Name -> SimpleExp Double -> M (Value Double)
    makeSimpleExp n s = do
      (!i, _, _) <- get
      modify $ \(_, acc, m) -> (i + 1, (i, s) : acc, HM.insert n i m)
      pure $ VarV i

    go :: Exp Double -> M (Value Double)
    go !x = do
      n <- lift $ makeStableName x
      (_, _, m) <- get
      case HM.lookup n m of
        Just i -> pure $ VarV i
        Nothing ->
          case x of
            Const k -> pure $ ConstV k
            Var -> pure $ VarV 0
            -- これまでに出現していない項であればletを作る
            Unary u y -> do
              y' <- go y
              makeSimpleExp n $ UnaryS u y'
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
