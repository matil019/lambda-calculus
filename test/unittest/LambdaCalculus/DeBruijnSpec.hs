module LambdaCalculus.DeBruijnSpec where

import LambdaCalculus.DeBruijn
import LambdaCalculus.TermSpec (AnyTerm(AnyTerm), BetaReducibleTerm(BetaReducibleTerm))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified LambdaCalculus.Main as Term  -- TODO move reduceBeta to LambdaCalculus.Term
import qualified LambdaCalculus.Term as Term
import qualified Test.QuickCheck as Q

spec :: Spec
spec = do
  it "toDeBruijn . fromDeBruijn == id" $
    pendingWith "instance Arbitrary DeBruijn.Term not defined"

  -- an alternative to above until I define @instance Arbitrary DeBruijn.Term@
  prop "toDeBruijn . fromDeBruijn . toDeBruijn == toDeBruijn" $ \(AnyTerm (_, m)) ->
    (toDeBruijn . uncurry fromDeBruijn . toDeBruijn) m `shouldBe` toDeBruijn m

  prop "fromDeBruijn (toDeBruijn m) `alphaEqv` m" $ \(AnyTerm (_, m)) ->
    (uncurry fromDeBruijn . toDeBruijn) m `shouldSatisfy` (`Term.alphaEqv` m)

  describe "reduceBeta" $ do
    prop "arbitrary Term.BetaReducibleTerm" $ \(BetaReducibleTerm (_, m)) ->
      case m of
        Term.App (Term.Abs _ _) _ ->
          let (free, m') = toDeBruijn m
          in fromDeBruijn free (reduceBeta m') `shouldSatisfy` (`Term.alphaEqv` Term.reduceBeta m)
        _ -> Q.discard
