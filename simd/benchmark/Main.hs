import           Criterion.Main
import qualified Data.Vector.Storable as VS
import           SIMD

f :: Num a => a -> a
f x = (x + 1)^(10 :: Int)
{-# INLINE f #-}

g :: Num a => a -> a
g x = let y = x + 1
          y2 = y * y
          y4 = y2 * y2
          y8 = y4 * y4
      in y8 * y2
{-# INLINE g #-}

main :: IO ()
main = defaultMain
  [ bgroup "Float"
    [ bgroup "f"
      [ bench "scalar" $ whnf (VS.map f) vf
      , bench "vector" $ whnf (SIMD.mapStorable f f) vf
      , bench "vector (unified)" $ whnf (SIMD.mapStorable' f) vf
      ]
    , bgroup "g"
      [ bench "scalar" $ whnf (VS.map g) vf
      , bench "vector" $ whnf (SIMD.mapStorable g g) vf
      , bench "vector (unified)" $ whnf (SIMD.mapStorable' g) vf
      ]
    ]
  , bgroup "Double"
    [ bgroup "f"
      [ bench "scalar" $ whnf (VS.map f) vd
      , bench "vector" $ whnf (SIMD.mapStorable f f) vd
      , bench "vector (unified)" $ whnf (SIMD.mapStorable' f) vd
      ]
    , bgroup "g"
      [ bench "scalar" $ whnf (VS.map g) vd
      , bench "vector" $ whnf (SIMD.mapStorable g g) vd
      , bench "vector (unified)" $ whnf (SIMD.mapStorable' g) vd
      ]
    ]
  ]
  where
    vf :: VS.Vector Float
    vf = VS.fromList [0..10000]
    vd :: VS.Vector Double
    vd = VS.fromList [0..10000]
