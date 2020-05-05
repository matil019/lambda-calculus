{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.DeBruijnBench where

import Criterion.Main
import Control.Monad (replicateM)
import LambdaCalculus.SimplyTyped.DeBruijn
import Test.QuickCheck (Arbitrary, arbitrary, genericShrink, shrink)

import qualified LambdaCalculus.SimplyTyped.HindleyMilner.MGU as MGU
import qualified Test.QuickCheck as Q

-- TODO copy pasted from the unittest; extract it into another internal library
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

instance Arbitrary Term where
  arbitrary = do
    let constants = fmap Just arbitrary
    Q.NonNegative (Q.Small freeNum) <- arbitrary
    genTerm constants freeNum

  shrink = filter positiveVar . genericShrink
    where
    positiveVar (Var x) = x > 0
    positiveVar _ = True

instance Arbitrary ClosedTerm where
  arbitrary = do
    let constants = fmap Just arbitrary
    genClosedTerm constants

  shrink = map ClosedTerm . shrink . unClosedTerm

benches :: [Benchmark]
benches =
  [ env (Q.generate $ replicateM 100 $ Q.resize 100 Q.arbitrary) $ \terms ->
      bgroup "reduceBeta"
      [ bench "reduceBeta"  $ nf (map reduceBeta)  terms
      , bench "reduceBeta2" $ nf (map reduceBeta2) terms
      ]
  ]
