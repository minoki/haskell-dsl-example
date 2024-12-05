{- cabal:
build-depends: base, vector, mtl
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
import Data.STRef
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST

-- 基本的な演算
data Node where
  InputNode :: Node
  UnaryNode :: Maybe Int -> (Double -> Double) -> Node
  BinaryNode :: Maybe Int -> Maybe Int -> (Double -> (Double, Double)) -> Node

-- 計算に使った「基本的な演算」のリスト
data Tape = Tape { currentLength :: Int, reversedNodes :: [Node] }

-- 順方向の計算に使うモナド
type M s = ReaderT (STRef s Tape) (ST s)

-- 順方向の計算に使うデータ型
data Reverse a = Reverse { primal :: a, sensitivityIndex :: Maybe Int }

addNode :: Node -> M s Int
addNode n = do
  r <- ask
  Tape i ns <- lift $ readSTRef r
  lift $ writeSTRef r (Tape (i + 1) (n : ns))
  pure i

run :: (forall s. Reverse Double -> M s (Reverse Double)) -> (Double -> (Double, Double -> Double))
run f x = runST $ do
  r <- newSTRef (Tape 0 [])
  let action = do
        i <- addNode InputNode
        f (Reverse x (Just i))
  Reverse y ys <- runReaderT action r
  case ys of
    Nothing -> pure (y, \_ys -> 0)
    Just yi -> do
      Tape m revNodes <- readSTRef r
      let fs ys = runST $ do
            -- テープを逆順に再生して微分を計算する
            v <- VM.replicate m (0 :: Double)
            VM.write v yi ys
            forM_ (zip [m-1,m-2..] revNodes) $ \(i,n) ->
              case n of
                InputNode -> pure ()
                UnaryNode (Just j) gs -> do
                  s <- VM.read v i
                  VM.modify v (+ gs s) j
                BinaryNode j k gs -> do
                  s <- VM.read v i
                  let (sj, sk) = gs s
                  maybe (pure ()) (VM.modify v (+ sj)) j
                  maybe (pure ()) (VM.modify v (+ sk)) k
            VM.read v 0
      pure (y, fs)

constant :: a -> M s (Reverse a)
constant x = pure $ Reverse x Nothing

add :: Reverse Double -> Reverse Double -> M s (Reverse Double)
add (Reverse x i) (Reverse y j) = do
  k <- addNode (BinaryNode i j (\s -> (s, s)))
  pure $ Reverse (x + y) (Just k)

mul :: Reverse Double -> Reverse Double -> M s (Reverse Double)
mul (Reverse x i) (Reverse y j) = do
  k <- addNode (BinaryNode i j (\s -> (y * s, x * s)))
  pure $ Reverse (x * y) (Just k)

f_ :: Reverse Double -> M s (Reverse Double)
f_ x = do
  k <- constant 1
  y <- add x k
  y2 <- mul y y
  y4 <- mul y2 y2
  y8 <- mul y4 y4
  mul y8 y2

f' :: Double -> (Double, Double -> Double)
f' = run f_

main :: IO ()
main = do
  let (y, ys) = f' 1
  print (ys 1)
