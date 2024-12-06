{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module SIMD where
import           Data.Functor.Identity (Identity (Identity, runIdentity))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign.Storable
import           GHC.Exts
import           GHC.IO

data family X4 a
data instance X4 Float = FloatX4 FloatX4#
data instance X4 Double = DoubleX2X2 DoubleX2# DoubleX2#

class Broadcast f a where
  broadcast :: a -> f a

instance Broadcast X4 Float where
  broadcast (F# x) = FloatX4 (broadcastFloatX4# x)
  {-# INLINE broadcast #-}

instance Broadcast X4 Double where
  broadcast (D# x) = let v = broadcastDoubleX2# x
                     in DoubleX2X2 v v
  {-# INLINE broadcast #-}

class MonoMap f a where
  monoMap :: (a -> a) -> f a -> f a

instance MonoMap X4 Float where
  monoMap f (FloatX4 v) = case unpackFloatX4# v of
    (# x0, x1, x2, x3 #) ->
      let !(F# y0) = f (F# x0)
          !(F# y1) = f (F# x1)
          !(F# y2) = f (F# x2)
          !(F# y3) = f (F# x3)
      in FloatX4 (packFloatX4# (# y0, y1, y2, y3 #))

instance MonoMap X4 Double where
  monoMap f (DoubleX2X2 v0 v1) = case unpackDoubleX2# v0 of
    (# x0, x1 #) -> case unpackDoubleX2# v1 of
      (# x2, x3 #) ->
        let !(D# y0) = f (D# x0)
            !(D# y1) = f (D# x1)
            !(D# y2) = f (D# x2)
            !(D# y3) = f (D# x3)
        in DoubleX2X2 (packDoubleX2# (# y0, y1 #)) (packDoubleX2# (# y2, y3 #))

class Broadcast f a => NumF f a where
  plusF :: f a -> f a -> f a
  minusF :: f a -> f a -> f a
  timesF :: f a -> f a -> f a
  absF :: f a -> f a
  signumF :: f a -> f a
  negateF :: f a -> f a

instance NumF X4 Float where
  plusF (FloatX4 v) (FloatX4 w) = FloatX4 (plusFloatX4# v w)
  minusF (FloatX4 v) (FloatX4 w) = FloatX4 (minusFloatX4# v w)
  timesF (FloatX4 v) (FloatX4 w) = FloatX4 (timesFloatX4# v w)
  negateF (FloatX4 v) = FloatX4 (negateFloatX4# v)
  absF = monoMap abs
  signumF = monoMap signum
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
  {-# INLINE signumF #-}

instance NumF X4 Double where
  plusF (DoubleX2X2 v0 v1) (DoubleX2X2 w0 w1) = DoubleX2X2 (plusDoubleX2# v0 w0) (plusDoubleX2# v1 w1)
  minusF (DoubleX2X2 v0 v1) (DoubleX2X2 w0 w1) = DoubleX2X2 (minusDoubleX2# v0 w0) (minusDoubleX2# v1 w1)
  timesF :: X4 Double -> X4 Double -> X4 Double
  timesF (DoubleX2X2 v0 v1) (DoubleX2X2 w0 w1) = DoubleX2X2 (timesDoubleX2# v0 w0) (timesDoubleX2# v1 w1)
  negateF (DoubleX2X2 v0 v1) = DoubleX2X2 (negateDoubleX2# v0) (negateDoubleX2# v1)
  absF = monoMap abs
  signumF = monoMap signum
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
  {-# INLINE signumF #-}

instance (NumF X4 a, Num a) => Num (X4 a) where
  (+) = plusF
  (-) = minusF
  (*) = timesF
  abs = absF
  signum = signumF
  negate = negateF
  fromInteger = broadcast . fromInteger
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}

class Storable a => StorableF f a where
  peekElemOffF :: Ptr a -> Int -> IO (f a)
  pokeElemOffF :: Ptr a -> Int -> f a -> IO ()

instance StorableF X4 Float where
  peekElemOffF (Ptr addr) (I# i) = IO $ \s ->
    case readFloatOffAddrAsFloatX4# addr i s of
      (# s', v #) -> (# s', FloatX4 v #)
  pokeElemOffF (Ptr addr) (I# i) (FloatX4 v) = IO $ \s ->
    (# writeFloatOffAddrAsFloatX4# addr i v s, () #)
  {-# INLINE peekElemOffF #-}
  {-# INLINE pokeElemOffF #-}

instance StorableF X4 Double where
  peekElemOffF (Ptr addr) (I# i) = IO $ \s ->
    case readDoubleOffAddrAsDoubleX2# addr i s of
      (# s', v0 #) ->
        case readDoubleOffAddrAsDoubleX2# addr (i +# 2#) s' of
          (# s'', v1 #) -> (# s'', DoubleX2X2 v0 v1 #)
  pokeElemOffF (Ptr addr) (I# i) (DoubleX2X2 v0 v1) = IO $ \s ->
    case writeDoubleOffAddrAsDoubleX2# addr i v0 s of
      s' -> (# writeDoubleOffAddrAsDoubleX2# addr (i +# 2#) v1 s', () #)
  {-# INLINE peekElemOffF #-}
  {-# INLINE pokeElemOffF #-}

mapStorable :: (StorableF X4 a, StorableF X4 b) => (X4 a -> X4 b) -> (a -> b) -> VS.Vector a -> VS.Vector b
mapStorable fv f !v = unsafePerformIO $ do
  let !n = VS.length v
  !result <- VSM.unsafeNew n
  VS.unsafeWith v $ \ !inputPtr ->
    VSM.unsafeWith result $ \ !resultPtr -> do
      let loopVector !i | i + 4 > n = loopScalar i
                        | otherwise = do
                          !a <- peekElemOffF inputPtr i
                          pokeElemOffF resultPtr i (fv a)
                          loopVector (i + 4)
          loopScalar !i | i >= n = pure ()
                        | otherwise = do
                          !a <- peekElemOff inputPtr i
                          pokeElemOff resultPtr i (f a)
                          loopScalar (i + 1)
      loopVector 0
  VS.unsafeFreeze result
{-# INLINE mapStorable #-}

type NumElement a = (Num a, NumF X4 a)
type StorableElement a = (Storable a, StorableF X4 a)

class (forall a. NumElement a => Num (f a)) => SIMD f
instance SIMD Identity
instance SIMD X4

mapStorable' :: (StorableF X4 a, StorableF X4 b) => (forall f. SIMD f => f a -> f b) -> VS.Vector a -> VS.Vector b
mapStorable' f = mapStorable f (runIdentity . f . Identity)
{-# INLINE mapStorable' #-}
