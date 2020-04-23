{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Instances where

import LambdaCalculus.SimplyTyped.HindleyMilner
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import Test.QuickCheck (Arbitrary, arbitrary)

instance Arbitrary Subst where
  arbitrary = Subst <$> arbitrary
