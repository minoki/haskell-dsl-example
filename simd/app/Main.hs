import qualified Data.Vector.Storable as VS
import           SIMD

f :: Num a => a -> a
f x = (x + 1)^10

g :: (SIMD f, NumElement a) => f a -> f a
g x = (x + 1)^10

main :: IO ()
main = do
  print $ VS.map f (VS.fromList [0..10] :: VS.Vector Float)
  print $ SIMD.mapStorable f f (VS.fromList [0..10] :: VS.Vector Float)
  print $ SIMD.mapStorable' f (VS.fromList [0..10] :: VS.Vector Float)
  print $ SIMD.mapStorable' g (VS.fromList [0..10] :: VS.Vector Float)
  print $ VS.map f (VS.fromList [0..10] :: VS.Vector Double)
  print $ SIMD.mapStorable f f (VS.fromList [0..10] :: VS.Vector Double)
  print $ SIMD.mapStorable' f (VS.fromList [0..10] :: VS.Vector Double)
  print $ SIMD.mapStorable' g (VS.fromList [0..10] :: VS.Vector Double)
