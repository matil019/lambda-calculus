module LambdaCalculus.DeBruijnSpec where

import Control.Lens (ix, toListOf)
import LambdaCalculus.DeBruijn
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Data.List.NonEmpty as NE
import qualified LambdaCalculus.Main as Term  -- TODO move reduceBeta to LambdaCalculus.Term
import qualified LambdaCalculus.Term as Term
import qualified LambdaCalculus.TermSpec as Term
import qualified Test.QuickCheck as Q

-- | A pair of the number of free variables and a term.
--
-- This enables defining an instance Arbitrary for testing.
newtype AnyTerm = AnyTerm (Int, Term)
  deriving Show

instance Arbitrary AnyTerm where
  arbitrary = do
    Q.NonNegative (Q.Small freeNum) <- arbitrary
    m <- genTerm freeNum
    pure $ AnyTerm (freeNum, m)

spec :: Spec
spec = do
  prop "length (linear m) == countTerm m" $ \(AnyTerm (_, m)) ->
    length (linear m) `shouldBe` countTerm m

  prop "ix is consistent with linear" $ \(AnyTerm (_, m)) ->
    NE.toList (linear m) `shouldBe` [ n | i <- [0..(countTerm m)], n <- toListOf (ix i) m ]

  describe "conversion between the other notation" $ do
    prop "(toDeBruijn . fromDeBruijn) m == m" $ \(AnyTerm (num, m)) ->
      (snd . toDeBruijn . fromDeBruijn (map show [1..num])) m `shouldBe` m

    prop "(fromDeBruijn . toDeBruijn) m `alphaEqv` m" $ \(Term.AnyTerm (_, m)) ->
      (uncurry fromDeBruijn . toDeBruijn) m `shouldSatisfy` (`Term.alphaEqv` m)

    describe "reduceBeta" $ do
      prop "arbitrary Term.BetaReducibleTerm" $ \(Term.BetaReducibleTerm (_, m)) ->
        case m of
          Term.App (Term.Abs _ _) _ ->
            let (free, m') = toDeBruijn m
            in fromDeBruijn free (reduceBeta m') `shouldSatisfy` (`Term.alphaEqv` Term.reduceBeta m)
          _ -> Q.discard
