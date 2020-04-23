module LambdaCalculus.SimplyTyped.HindleyMilnerSpec where

import LambdaCalculus.SimplyTyped.HindleyMilner
import LambdaCalculus.SimplyTyped.HindleyMilner.Term
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import LambdaCalculus.SimplyTyped.HindleyMilner.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

shouldTypeCheckWith :: Term -> MonoType -> Expectation
shouldTypeCheckWith m t' = infer m `shouldSatisfy` \mt ->
  case mt of
    Just t -> check t' t
    _ -> False

-- note that this is /not/ equiavelent to @not . shouldTypeCheckWith@
shouldNotTypeCheckWith :: Term -> MonoType -> Expectation
shouldNotTypeCheckWith m t' = infer m `shouldSatisfy` \mt ->
  case mt of
    Just t -> not $ check t' t
    _ -> False

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

    describe "should not infer types of polymorphic terms" $
      -- here 2 is @id :: a -> a@ which should not be polymorphic in simply typed lambda calculus
      -- if it were polymorphic, it would be inferred as @(a -> b -> t) -> a -> b -> t@
      it "infer (\\ \\ 1 (2 (\"a\" :: \"a\")) (2 (\"b\" :: \"b\"))) (\\ 1) :: <ill-typed>" $
        infer (App (Abs $ Abs $ App (App (Var 2) (Const (ConstType "a") "a")) (App (Var 2) (Const (ConstType "b") "b"))) (Abs $ Var 1))
          `shouldBe` Nothing

    it "infer 1 :: a" $
      Var 1 `shouldTypeCheckWith` (ConstType "a")

    it "infer (\\ 1) :: a -> a" $
      (Abs $ Var 1) `shouldTypeCheckWith` (ConstType "a" :-> ConstType "a")

    it "infer (\\ \\ 2 1) :: (a -> b) -> a -> b" $
      (Abs $ Abs $ App (Var 2) (Var 1)) `shouldTypeCheckWith`
        ((ConstType "a" :-> ConstType "b") :-> ConstType "a" :-> ConstType "b")

    it "infer (\\ \\ 2 (2 1)) :: (a -> a) -> a -> a" $
      (Abs $ Abs $ App (Var 2) $ App (Var 2) $ Var 1) `shouldTypeCheckWith`
        ((ConstType "a" :-> ConstType "a") :-> ConstType "a" :-> ConstType "a")

    it "*not* infer (\\ \\ 2 (2 1)) :: (a -> b) -> a -> b" $
      (Abs $ Abs $ App (Var 2) $ App (Var 2) $ Var 1) `shouldNotTypeCheckWith`
        ((ConstType "a" :-> ConstType "b") :-> ConstType "a" :-> ConstType "b")
