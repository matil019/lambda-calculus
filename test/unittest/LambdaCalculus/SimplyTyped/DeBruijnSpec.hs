{-# LANGUAGE RankNTypes #-}
module LambdaCalculus.SimplyTyped.DeBruijnSpec where

import Control.Lens (Prism')
import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import LensLawSpec (prismLawSpec)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances.Natural ()

spec :: Spec
spec = do
  describe "interpretChurchNumber" $ do
    it "(\\ \\ 1) == #0" $
      interpretChurchNumber (Abs $ Abs $ Var 1) `shouldBe` Just 0

    it "(\\ \\ 2 1) == #1" $
      interpretChurchNumber (Abs $ Abs $ App (Var 2) (Var 1)) `shouldBe` Just 1

    -- the special case; eta-reduced form
    it "(\\ 1) == #1" $
      interpretChurchNumber (Abs $ Var 1) `shouldBe` Just 1

    it "(\\ \\ 2 (2 1)) == #2" $
      interpretChurchNumber (Abs $ Abs $ App (Var 2) $ App (Var 2) (Var 1)) `shouldBe` Just 2

    prop "interpretChurchNumber . encodeChurchNumber == Just" $ \n ->
      (interpretChurchNumber . encodeChurchNumber) n `shouldBe` Just n

  describe "_closedTerm" $ do
    describe "should follow the prism law" $
      prismLawSpec (_closedTerm :: Prism' Term ClosedTerm)
