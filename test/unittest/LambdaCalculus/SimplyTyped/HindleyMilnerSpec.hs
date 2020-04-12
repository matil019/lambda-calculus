module LambdaCalculus.SimplyTyped.HindleyMilnerSpec where

import LambdaCalculus.SimplyTyped.HindleyMilner
import LambdaCalculus.SimplyTyped.HindleyMilner.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "instance Monoid Subst" $ do
    prop "subst (a <> b) == subst a . subst b" $ \a b t ->
      subst (a <> b) t `shouldBe` (subst a . subst b) t

    prop "subst mempty == id" $ \t ->
      subst mempty t `shouldBe` t
