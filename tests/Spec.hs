{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.AdditiveGroup
import Data.Monoid.Average
import Data.VectorSpace
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (applicative, functor, monoid)
import Data.Function (on)

#define INFXONAVG(op,numT) (op :: Average (numT) -> Average (numT) -> Average (numT))

instance Arbitrary (Average Rational) where
  arbitrary = Average <$> arbitrary

instance Arbitrary (Average (Rational -> Rational)) where
  arbitrary = Average <$> arbitrary

instance EqProp (Average Rational) where
  Average l =-= Average m = (l==m) =-= True

main :: IO ()
main =
  hspec $
    describe "Average" $ do
      testBatch $ monoid (undefined :: Average Rational)
      testBatch $ functor (undefined :: Average (Rational, Rational, Rational))
      testBatch $ applicative (undefined :: Average (Rational, Rational, Rational))
      describe "laws for: Num" $ do
        describe "addition" $ do
          it "associativity" . property $ isAssoc (INFXONAVG((+),Rational))
          it "commutative" . property $ isCommut (INFXONAVG((+),Rational))
          it "left identity" . property $ leftId (INFXONAVG((+),Rational)) 0
          it "right identity" . property $ rightId (INFXONAVG((+),Rational)) 0
        describe "multiplication" $ do
          it "associativity" . property $ isAssoc (INFXONAVG((*),Rational))
          it "commutative" . property $ isCommut (INFXONAVG((*),Rational))
          it "left identity" . property $ leftId (INFXONAVG((*),Rational)) 1
          it "right identity" . property $ rightId (INFXONAVG((*),Rational)) 1
      describe "laws for: vector space" $ do
        it "associativity" . property $ isAssoc (INFXONAVG((^+^),Rational))
        it "commutative" . property $ isCommut (INFXONAVG((^+^),Rational))
        it "left identity" . property $ leftId (INFXONAVG((^+^),Rational)) zeroV
        it "right identity" . property $ rightId (INFXONAVG((^+^),Rational)) zeroV
        describe "closure" $ do
          it "distributive: c u v" . property $ \(c, u, v :: Average Rational) ->
            c *^ (u ^+^ v) =-= (c *^ u) ^+^ (c *^ v)
          it "distributive: c d v" . property $ \(c, d, v :: Average Rational) ->
            (c ^+^ d) *^ v =-= c *^ v ^+^ d *^ v
          it "associativity" . property $ \(c, d, v :: Average Rational) ->
            c *^ (d *^ v) =-= (c * d) *^ v
          it "unitary" . property $ \(v :: Average Rational) ->
            1 *^ v =-= v
