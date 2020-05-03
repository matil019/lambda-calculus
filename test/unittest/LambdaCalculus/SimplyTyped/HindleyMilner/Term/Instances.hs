{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances where

import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import LambdaCalculus.SimplyTyped.DeBruijn
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Test.QuickCheck as Q

instance Arbitrary Term where
  arbitrary = do
    let constants = fmap Just arbitrary
    Q.NonNegative (Q.Small freeNum) <- arbitrary
    genTerm constants freeNum
