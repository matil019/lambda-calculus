{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.DeBruijnBench where

import Criterion.Main
import Control.Monad (replicateM)
import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()

import qualified LambdaCalculus.InfList as InfList
import qualified Test.QuickCheck as Q

benches :: [Benchmark]
benches =
  [ env (Q.generate $ replicateM 100 $ Q.resize 100 Q.arbitrary) $ \terms ->
      bgroup "reduceBeta"
      [ bench "recursive"  $ nf (map reduceBeta) terms
      , bench "substitute" $ nf (map reduceBeta_substitute) terms
      ]
  , let gen = (\(Q.NonNegative (Q.Small inc)) m -> (inc, m)) <$> Q.arbitrary <*> Q.arbitrary
    in
    env (Q.generate $ replicateM 100 $ Q.resize 100 gen) $ \incsTerms ->
      bgroup "incrementFreeVars"
      [ bench "incrementFreeVars" $ nf (map (uncurry incrementFreeVars)) incsTerms
      , bench "substitute" $ nf (map (\(inc, m) -> substitute (fmap Var $ InfList.enumFrom (1 + inc)) m)) incsTerms
      ]
  ]

-- | Performs a beta-reduction.
--
-- This is the old implementation which uses `substitute`.
reduceBeta_substitute :: Term -> Maybe Term
reduceBeta_substitute (App (Abs m) n) = Just $ substitute (InfList.cons n $ fmap Var $ InfList.enumFrom 1) m
reduceBeta_substitute _ = Nothing
