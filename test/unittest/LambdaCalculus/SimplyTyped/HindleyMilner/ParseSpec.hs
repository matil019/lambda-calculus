{-# LANGUAGE LambdaCase #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.ParseSpec where

import Control.Lens (transformM)
import Data.Char (isUpper, toLower, toUpper)
import LambdaCalculus.SimplyTyped.HindleyMilner.Parse
import LambdaCalculus.SimplyTyped.HindleyMilner.Term (Term(Var, App, Const), formatTerm)
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import LambdaCalculus.SimplyTyped.HindleyMilner.Types (MonoType(VarType, ConstType), formatMonoType)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import LambdaCalculus.Utils (isSimpleIdent)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Test.QuickCheck as Q

newtype ParsableMonoType = PMT MonoType
  deriving (Eq, Show)

instance Arbitrary ParsableMonoType where
  arbitrary = do
    t <- arbitrary
    fmap PMT $ transformM go t
    where
    go (VarType _) = do
      -- isSimpleIdent guarantees that @x@ is non-empty
      x <- genApproxIdent `Q.suchThat` isSimpleIdent
      pure $ VarType (toLower (head x) : tail x)
    go (ConstType _) = do
      x <- genApproxIdent `Q.suchThat` (\x -> isSimpleIdent x && isUpper (head x)) -- exclude a leading underscore
      pure $ ConstType (toUpper (head x) : tail x)
    go t = pure t
    -- generates a mostly valid identifier
    -- the intent is to generate a non-empty string consisting of alphabets/numbers/underscores
    -- but I'm too lazy to implement an exact generator for it
    genApproxIdent = Q.listOf1 $ Q.chooseEnum ('0', 'z')

newtype ParsableTerm = PT Term
  deriving (Eq, Show)

instance Arbitrary ParsableTerm where
  arbitrary = do
    m <- arbitrary
    fmap PT $ transformPMT m
    where
    transformPMT = transformM $ \case
      Const _ a -> arbitrary >>= \(PMT t) -> pure (Const t a)
      m -> pure m

spec :: Spec
spec = do
  prop "parseTerm . formatTerm == pure" $ \(PT m) ->
    (parseTerm . formatTerm) m `shouldBe` Just m

  describe "parsing Term applications" $ do
    it "parseTerm \"1 2\"" $
      parseTerm "1 2" `shouldBe` Just (App (Var 1) (Var 2))

    it "parseTerm \"1 2 3\"" $
      parseTerm "1 2 3" `shouldBe` Just (App (App (Var 1) (Var 2)) (Var 3))

    it "parseTerm \"1 (2 3)\"" $
      parseTerm "1 (2 3)" `shouldBe` Just (App (Var 1) (App (Var 2) (Var 3)))

  prop "parseMonoType . formatMonoType == pure" $ \(PMT t) ->
    (parseMonoType . formatMonoType) t `shouldBe` Just t
