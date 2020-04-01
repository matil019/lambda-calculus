module LambdaCalculus.DeBruijnSpec where

import Control.Lens (ix, toListOf)
import LambdaCalculus.DeBruijn
import LambdaCalculus.Genetic (genCrossover, genMutant)
import LambdaCalculus.Utils (FiniteList(FiniteList))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, forAll)

import qualified Data.List.NonEmpty as NE
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
  prop "(isClosed . unClosedTerm) m" $ \m ->
    m `shouldSatisfy` (isClosed . unClosedTerm)

  prop "length (linear m) == countTerm m" $ \(AnyTerm (_, m)) ->
    length (linear m) `shouldBe` countTerm m

  prop "head (linear m) == m" $ \(AnyTerm (_, m)) ->
    NE.head (linear m) `shouldBe` m

  prop "index 0 m == Just m" $ \(AnyTerm (_, m)) ->
    index 0 m `shouldBe` Just m

  it "index i m == Just (toList m !! i)" $
    let gen = do
          AnyTerm (_, m) <- arbitrary
          let c = countTerm m
          i <- Q.choose (0, c-1)
          pure (m, i)
    in forAll gen $ \(m, i) -> index i m == Just (toList m !! i)

  prop "ix is consistent with linear" $ \(AnyTerm (_, m)) ->
    NE.toList (linear m) `shouldBe` [ n | i <- [0..(countTerm m)], n <- toListOf (ix i) m ]

  describe "conversion between the other notation" $ do
    prop "(toDeBruijn . fromDeBruijn) m == m" $ \(AnyTerm (num, m)) ->
      let free = FiniteList (map show [1..num])
      in (snd . toDeBruijn free . fromDeBruijn free) m `shouldBe` m

    prop "(fromDeBruijn . toDeBruijn) m `alphaEqv` m" $ \(Term.AnyTerm (_, m)) ->
      (uncurry fromDeBruijn . toDeBruijn mempty) m `shouldSatisfy` (`Term.alphaEqv` m)

    describe "reduceBeta" $ do
      prop "arbitrary Term.BetaReducibleTerm" $ \(Term.BetaReducibleTerm (_, m)) ->
        case m of
          Term.App (Term.Abs _ _) _ ->
            let (free, m') = toDeBruijn mempty m
            in fromDeBruijn free (reduceBeta m') `shouldSatisfy` (`Term.alphaEqv` Term.reduceBeta m)
          _ -> Q.discard

  describe "instance Genetic ClosedTerm" $ do
    let isReallyClosed = isClosed . unClosedTerm

    it "genCrossover should return closed terms" $ forAll (arbitrary >>= \m12 -> genCrossover m12 >>= \m12' -> pure (m12, m12')) $ \((m1, m2), (m1', m2')) -> do
      m1  `shouldSatisfy` isReallyClosed
      m2  `shouldSatisfy` isReallyClosed
      m1' `shouldSatisfy` isReallyClosed
      m2' `shouldSatisfy` isReallyClosed

    it "genMutant should return a closed term" $ forAll (arbitrary >>= \m -> genMutant m >>= \m' -> pure (m, m')) $ \(m, m') -> do
      m  `shouldSatisfy` isReallyClosed
      m' `shouldSatisfy` isReallyClosed
