module LambdaCalculus.DeBruijnSpec where

import LambdaCalculus.DeBruijn
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified LambdaCalculus.Term as Term

spec :: Spec
spec = do
  it "toDeBruijn . fromDeBruijn == id" $
    pendingWith "instance Arbitrary DeBruijn.Term not defined"

  -- an alternative to above until I define @instance Arbitrary DeBruijn.Term@
  prop "toDeBruijn . fromDeBruijn . toDeBruijn == toDeBruijn" $ \m ->
    (toDeBruijn . fromDeBruijn . toDeBruijn) m `shouldBe` toDeBruijn m

  prop "fromDeBruijn (toDeBruijn m) `alphaEqv` m" $ \m ->
    Term.unClosedTerm (fromDeBruijn (toDeBruijn m)) `shouldSatisfy` (`Term.alphaEqv` Term.unClosedTerm m)
