module LambdaCalculus.SimplyTyped.HindleyMilner.TermSpec where

import LambdaCalculus.SimplyTyped.HindleyMilner.Term
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "isClosed" $ do
    prop "The 'foldVars' implementation should be equivalent to the original" $ \m ->
      isClosed m `shouldBe` isClosedOriginal m
      where
      isClosedOriginal = go 0
        where
        go bound (Var x) = x <= bound
        go bound (Abs m) = go (bound+1) m
        go bound (App m n) = go bound m && go bound n
        go _ (Const _ _) = True
