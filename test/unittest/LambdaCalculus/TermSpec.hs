module LambdaCalculus.TermSpec where

import Control.Lens (ix, toListOf)
import Data.Set (Set)
import LambdaCalculus.Term
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Test.QuickCheck as Q

-- | A pair of free variables and a term. This enables defining an instance Arbitrary for testing.
newtype AnyTerm = AnyTerm (Set Var, Term)
  deriving Show

instance Arbitrary AnyTerm where
  arbitrary = do
    vars <- arbitrary
    m <- genTerm vars
    pure $ AnyTerm (vars, m)

newtype BetaReducibleTerm = BetaReducibleTerm (Set Var, Term)
  deriving Show

-- | Generates a pair of free variables and a beta-reducible term.
instance Arbitrary BetaReducibleTerm where
  arbitrary = do
    bound <- arbitrary
    free <- Q.suchThat arbitrary (bound `notElem`)
    m <- genTerm (Set.insert bound free)
    n <- genTerm (Set.insert bound free)
    pure $ BetaReducibleTerm (free, App (Abs bound m) n)

spec :: Spec
spec = do
  prop "length (linear m) == countTerm m" $ \(AnyTerm (_, m)) ->
    length (linear m) `shouldBe` countTerm m

  prop "ix is consistent with linear" $ \(AnyTerm (_, m)) ->
    NE.toList (linear m) `shouldBe` [ n | i <- [0..(countTerm m)], n <- toListOf (ix i) m ]

  describe "alphaEqv" $ do
    prop "m == n => m `alphaEqv` n" $ \(AnyTerm (_, m)) ->
      m `shouldSatisfy` (m `alphaEqv`)

    it "same free variables => alphaEqv" $
      Var "a" `shouldSatisfy` (Var "a" `alphaEqv`)

    it "different free variables => not alphaEqv" $
      Var "b" `shouldNotSatisfy` (Var "a" `alphaEqv`)

    it "simple Abs" $
      Abs "b" (Var "b") `shouldSatisfy` (Abs "a" (Var "a") `alphaEqv`)

    it "simple App" $
      App (Abs "b" (Var "b")) (Var "c") `shouldSatisfy` (App (Abs "a" (Var "a")) (Var "c") `alphaEqv`)
