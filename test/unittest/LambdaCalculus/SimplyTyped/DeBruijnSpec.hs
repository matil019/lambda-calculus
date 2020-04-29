{-# LANGUAGE RankNTypes #-}
module LambdaCalculus.SimplyTyped.DeBruijnSpec where

-- TODO no all-in
import Control.Lens
import LensLawSpec
import Test.QuickCheck as Q

import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances.Natural ()

prismLawSpec
  :: (Arbitrary s, Eq s, Show s, Arbitrary a, CoArbitrary a, Eq a, Function a, Show a)
  => Prism' s a
  -> Spec
prismLawSpec l = do
  prismLawOnlySpec l

  traversalLawSpec l

-- | Like 'prismLawSpec', but doesn't call 'traversalLawSpec'.
prismLawOnlySpec
  :: (Arbitrary s, Eq s, Show s, Arbitrary a, Eq a, Show a)
  => Prism' s a
  -> Spec
prismLawOnlySpec l = do
  prop "preview l (review l b) == Just b" $ \b ->
    preview l (review l b) `shouldBe` Just b

  prop "preview l s == Just a ==> review l a == s" $ \s ->
    case preview l s of
      Just a -> review l a `shouldBe` s
      Nothing -> pure ()

  prop "matching l s == Left t ==> matching l t == Left s" $ \s ->
    case matching l s of
      Left t -> matching l t `shouldBe` Left s
      Right _ -> pure () -- using @discard@ causes the test to fail if an @Iso@ was given

isoLawSpec
  :: (Arbitrary s, Eq s, Show s, Arbitrary a, CoArbitrary a, Eq a, Function a, Show a)
  => Iso' s a
  -> Spec
isoLawSpec f = do
  lensLawSpec f
  prismLawOnlySpec f

data SomeTypeSet

instance TypeSet SomeTypeSet where
  candidateConsts _ = [(VarType "i", "i"), (ConstType "a", "a"), (VarType "x" :-> VarType "y", "xy")]

spec :: Spec
spec = do
  describe "_closedTerm" $ do
    prismLawSpec (_closedTerm :: Prism' Term (ClosedTerm SomeTypeSet))

  describe "_churchNumber" $ do
    prismLawSpec _churchNumber
