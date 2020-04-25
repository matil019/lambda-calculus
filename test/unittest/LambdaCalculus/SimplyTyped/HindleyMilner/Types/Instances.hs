{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances where

import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import Test.QuickCheck (Arbitrary, CoArbitrary, Function, arbitrary)

import qualified LambdaCalculus.SimplyTyped.HindleyMilner.MGU as MGU
import qualified Test.QuickCheck as Q

-- | The size parameter is used as the expected size of a 'MonoType'.
instance Arbitrary MonoType where
  arbitrary = do
    size <- max 1 <$> Q.getSize
    let p = 10000 * (size + 1) `div` (2 * size)
        q = 10000 - p
    Q.frequency [(p `div` 2, genVar), (p `div` 2, genConst), (q, genMono)]
    where
    -- 1
    genVar = VarType <$> arbitrary
    -- 1
    genConst = ConstType <$> arbitrary
    -- 2X + 1
    genMono = (:->) <$> arbitrary <*> arbitrary

instance CoArbitrary MonoType

instance Function MonoType

instance Arbitrary PolyType where
  arbitrary = Q.oneof [genMono, genBound1, genBoundSome, genBoundAll]
    where
    genMono = Mono <$> arbitrary
    genBound1 = do
      t <- arbitrary
      case MGU.vars t of
        [] -> pure $ Mono t
        as -> do
          a <- Q.elements as
          pure $ ForAll a $ Mono t
    genBoundSome = do
      t <- arbitrary
      as <- Q.shuffle =<< Q.sublistOf (MGU.vars t)
      pure $ bindVars as t
    genBoundAll = do
      t <- arbitrary
      as <- Q.shuffle (MGU.vars t)
      pure $ bindVars as t
    bindVars [] t = Mono t
    bindVars (a:as) t = ForAll a $ bindVars as t
