{-# LANGUAGE RankNTypes #-}
module LambdaCalculus.SimplyTyped.DeBruijnSpec where

import Control.Lens (Prism')
import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import LensLawSpec (prismLawSpec)
import Test.Hspec
import Test.QuickCheck.Instances.Natural ()

data SomeTypeSet

instance TypeSet SomeTypeSet where
  candidateConsts _ = [(VarType "i", "i"), (ConstType "a", "a"), (VarType "x" :-> VarType "y", "xy")]

spec :: Spec
spec = do
  describe "_closedTerm" $ do
    prismLawSpec (_closedTerm :: Prism' Term (ClosedTerm SomeTypeSet))

  describe "_churchNumber" $ do
    prismLawSpec _churchNumber
