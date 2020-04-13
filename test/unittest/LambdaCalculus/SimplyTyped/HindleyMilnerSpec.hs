module LambdaCalculus.SimplyTyped.HindleyMilnerSpec where

import Data.Maybe (fromJust, isJust)
import LambdaCalculus.SimplyTyped.HindleyMilner
import LambdaCalculus.SimplyTyped.HindleyMilner.Term
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import LambdaCalculus.SimplyTyped.HindleyMilner.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

shouldTypeCheckWith :: Term -> PolyType -> Expectation
shouldTypeCheckWith m p = infer m `shouldSatisfy` \mt ->
  if isJust mt then check (quantify (fromJust mt)) p else False

shouldNotTypeCheckWith :: Term -> PolyType -> Expectation
shouldNotTypeCheckWith m p = infer m `shouldNotSatisfy` \mt ->
  if isJust mt then check (quantify (fromJust mt)) p else False

spec :: Spec
spec = do
  describe "instance Monoid Subst" $ do
    prop "subst (a <> b) == subst a . subst b" $ \a b t ->
      subst (a <> b) t `shouldBe` (subst a . subst b) t

    prop "subst mempty == id" $ \t ->
      subst mempty t `shouldBe` t

  describe "infer" $ do
    it "should not infer types of recursive terms" $
      infer (Abs $ App (Var 1) (Var 1)) `shouldBe` Nothing

    it "infer 1 :: a" $
      Var 1 `shouldTypeCheckWith` (quantify $ VarType "a")

    it "infer (\\ 1) :: a -> a" $
      (Abs $ Var 1) `shouldTypeCheckWith` (quantify $ VarType "a" :-> VarType "a")

    it "infer (\\ \\ 2 1) :: (a -> b) -> a -> b" $
      (Abs $ Abs $ App (Var 2) (Var 1)) `shouldTypeCheckWith`
        (quantify $ (VarType "a" :-> VarType "b") :-> VarType "a" :-> VarType "b")

    it "infer (\\ \\ 2 (2 1)) :: (a -> a) -> a -> a" $
      (Abs $ Abs $ App (Var 2) $ App (Var 2) $ Var 1) `shouldTypeCheckWith`
        (quantify $ (VarType "a" :-> VarType "a") :-> VarType "a" :-> VarType "a")

    -- TODO this should fail but doesn't
    xit "*not* infer (\\ \\ 2 (2 1)) :: (a -> b) -> a -> b" $
      (Abs $ Abs $ App (Var 2) $ App (Var 2) $ Var 1) `shouldNotTypeCheckWith`
        (quantify $ (VarType "a" :-> VarType "b") :-> VarType "a" :-> VarType "b")
