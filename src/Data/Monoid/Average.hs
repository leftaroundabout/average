
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : stable
-- Portability : non-portable
--
--  Provides a monoid for calculating arithmetic means.
--
-------------------------------------------------------------------------------------

module Data.Monoid.Average (
    Average(..),
    getAverage,
    mayAverage
  ) where

import Prelude hiding ((**))

import Data.Typeable
import Data.Maybe
import Data.Function (on)
import Data.Semigroup
import Data.AdditiveGroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative

-- |
-- A monoid for 'Average' values.
--
-- Represented by a /weight/ (i.e., how many values have already been
-- considered) and by their average.
--
-- >>> getAverage $ foldMap averageDatum [1,2,3]
-- 2.0
--
data Average a = Average { averageWeight :: !Int, runningAverage :: !a }
  deriving (Show, Typeable, Functor)

instance Applicative Average where
  pure = Average 1
  Average wf f <*> Average wx x = Average (wf*wx) (f x)

instance (VectorSpace v, Fractional (Scalar v)) => Semigroup (Average v) where
  Average 0 _ <> b = b
  a <> Average 0 _ = a
  Average wa a <> Average wb b
      = Average wΣ $ (fromIntegral wa*^a ^+^ fromIntegral wb*^b)
                        ^/ fromIntegral wΣ
   where wΣ = wa + wb

instance (VectorSpace v, Fractional (Scalar v)) => Monoid (Average v) where
  mempty = Average 0 zeroV
  mappend = (<>)
    
instance (Fractional a, Eq a) => Eq (Average a) where
  a == b = getAverage a == getAverage b

instance (Fractional a, Ord a) => Ord (Average a) where
  a `compare` b = getAverage a `compare` getAverage b

instance Num a => Num (Average a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  
instance (Fractional a, Num a) => Fractional (Average a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational

instance (Real a, Fractional a) => Real (Average a) where
  toRational = toRational . getAverage

instance Floating a => Floating (Average a) where
  pi = pure pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh
  
instance AdditiveGroup a => AdditiveGroup (Average a) where
  zeroV = pure zeroV
  (^+^) = liftA2 (^+^)
  negateV = fmap negateV

instance VectorSpace a => VectorSpace (Average a) where
  type Scalar (Average a) = Scalar a
  s *^ v = liftA2 (*^) (pure s) v

instance AffineSpace a => AffineSpace (Average a) where
  type Diff (Average a) = Average (Diff a)
  p1 .-. p2 = liftA2 (.-.) p1 p2
  p .+^ v   = liftA2 (.+^) p v

{-
instance Arbitrary a => Arbitrary (Average a) where
  arbitrary = fmap Average arbitrary
-}

-- | Return the average of all monoidal components. If given 'mempty', return zero.
getAverage :: Average a -> a
getAverage = runningAverage

-- | Return the average of all monoidal components. If given 'mempty', return 'Nothing'.
mayAverage :: Average a -> Maybe a
mayAverage (Average 0 _) = Nothing
mayAverage (Average _ x) = Just x
