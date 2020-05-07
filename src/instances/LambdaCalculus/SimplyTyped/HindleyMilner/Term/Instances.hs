{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances where

import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term (TermRaw, termRaw)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import Test.QuickCheck (Arbitrary, arbitrary, genericShrink, shrink)

import qualified Test.QuickCheck as Q

instance Arbitrary TermRaw where
  arbitrary = fmap termRaw arbitrary

  shrink = genericShrink

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
