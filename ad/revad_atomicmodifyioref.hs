{- cabal:
build-depends: base, vector, mtl, reflection, parallel
ghc-options: -threaded -rtsopts -with-rtsopts=-N8
-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
import Data.Kind
import Data.Reflection
import Data.IORef
import Data.Proxy
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Control.Parallel.Strategies
import System.IO.Unsafe

-- 基本的な演算
data Node where
  InputNode :: Node
  UnaryNode :: Maybe Int -> (Double -> Double) -> Node
  BinaryNode :: Maybe Int -> Maybe Int -> (Double -> (Double, Double)) -> Node

-- 計算に使った「基本的な演算」のリスト
data Tape = Tape { currentLength :: Int, reversedNodes :: [Node] }

type M = ReaderT (IORef Tape) IO

-- 順方向の計算に使うデータ型
type Reverse :: Type -> Type -> Type
data Reverse s a = Reverse { primal :: !a, sensitivityIndex :: !(Maybe Int) }

addNode :: Node -> M Int
addNode n = do
  r <- ask
  lift $ atomicModifyIORef' r (\(Tape i ns) -> (Tape (i + 1) (n : ns), i))

runNode :: forall a s. Reifies s (IORef Tape) => M (Reverse s a) -> Reverse s a
runNode action = let r = reflect (Proxy :: Proxy s)
                 in unsafePerformIO (runReaderT action r)

run :: (forall s. Reifies s (IORef Tape) => Reverse s Double -> Reverse s Double) -> (Double -> (Double, Double -> Double))
run f x = unsafePerformIO $ do
  r <- newIORef (Tape 0 [])
  let action = do
        i <- addNode InputNode
        pure $! reify r $ \(proxy :: Proxy s) -> case f (Reverse x (Just i) :: Reverse s Double) of
          Reverse y ys -> (y, ys)
  (!y, ys) <- runReaderT action r
  case ys of
    Nothing -> pure (y, \_ys -> 0)
    Just yi -> do
      Tape m revNodes <- readIORef r
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

constant :: a -> Reverse s a
constant x = Reverse x Nothing

add :: Reifies s (IORef Tape) => Reverse s Double -> Reverse s Double -> Reverse s Double
add (Reverse x i) (Reverse y j) = runNode $ do
  k <- addNode (BinaryNode i j (\s -> (s, s)))
  pure $ Reverse (x + y) (Just k)

mul :: Reifies s (IORef Tape) => Reverse s Double -> Reverse s Double -> Reverse s Double
mul (Reverse x i) (Reverse y j) = runNode $ do
  k <- addNode (BinaryNode i j (\s -> (y * s, x * s)))
  pure $ Reverse (x * y) (Just k)

pow :: Reifies s (IORef Tape) => Reverse s Double -> Int -> Reverse s Double
pow x 0 = constant 1
pow x i = case i `quotRem` 2 of
  (j, 0) -> pow (mul x x) j
  (j, _) -> mul x (pow (mul x x) j)

f_ :: Reifies s (IORef Tape) => Reverse s Double -> Reverse s Double
f_ x = let xs = [pow x i | i <- [0..100]] `using` parList rseq
           -- `using` parList rseq でリストの要素を並列に評価する
       in foldl add (constant 0) xs

f' :: Double -> (Double, Double -> Double)
f' = run f_

main :: IO ()
main = do
  let (y, ys) = f' 1
  print (ys 1)
