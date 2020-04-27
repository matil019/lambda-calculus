{-# LANGUAGE TypeApplications #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.TypesSpec where

import Control.Lens (plate, set, view)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import LensLawSpec (lensLawSpec, traversalLawSpec)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "plate @MonoType" $ do
    describe "should follow the traversal law" $
      traversalLawSpec (plate @MonoType)

  describe "topMono" $ do
    describe "should follow the lens law" $
      lensLawSpec boundVars

  describe "boundVars" $ do
    prop "set boundVars [] should remove all bindings" $ \s ->
      set boundVars [] s `shouldBe` Mono (view topMono s)

    describe "should follow the lens law" $
      lensLawSpec boundVars
