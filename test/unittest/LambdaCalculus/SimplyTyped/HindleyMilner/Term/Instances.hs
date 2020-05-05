{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances where

import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import LambdaCalculus.SimplyTyped.DeBruijn
import Test.QuickCheck (Arbitrary, arbitrary, genericShrink, shrink)

import qualified Test.QuickCheck as Q

instance Arbitrary Term where
  arbitrary = do
    let constants = fmap Just arbitrary
    Q.NonNegative (Q.Small freeNum) <- arbitrary
    genTerm constants freeNum

  shrink = filter positiveVar . genericShrink
    where
    positiveVar (Var x) = x > 0
    positiveVar _ = True

instance Arbitrary ClosedTerm where
  arbitrary = do
    let constants = fmap Just arbitrary
    genClosedTerm constants

  shrink = map ClosedTerm . shrink . unClosedTerm
