{-# LANGUAGE RankNTypes #-}
module LambdaCalculus.SimplyTyped.DeBruijnSpec where

import Control.Lens (Prism')
import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import LensLawSpec (prismLawSpec)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative(NonNegative), discard)
import Test.QuickCheck.Instances.Natural ()

import qualified LambdaCalculus.InfList as InfList
import qualified Test.QuickCheck as Q

spec :: Spec
spec = do
  describe "incrementFreeVars" $ do
    prop "incrementFreeVars inc == substitute [(1+inc)..]" $ \m (NonNegative inc) ->
      incrementFreeVars inc m `shouldBe` substitute (fmap Var $ InfList.enumFrom (1 + inc)) m

  describe "decrementFreeVars" $ do
    prop "should be an inverse of incrementFreeVars" $ \m dec ->
      let expected = incrementFreeVars (-dec) m
      in case decrementFreeVars dec m of
        Just m' | m' /= m   -> Q.label "decremented" $ m' `shouldBe` expected
        Just m' | otherwise -> Q.label "noop" $ m' `shouldBe` expected
        Nothing -> discard

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
