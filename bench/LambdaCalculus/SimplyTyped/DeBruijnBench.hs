{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.DeBruijnBench where

import Criterion.Main
import Control.Monad (replicateM)
import LambdaCalculus.SimplyTyped.DeBruijn
import LambdaCalculus.SimplyTyped.HindleyMilner.Term.Instances ()
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()

import qualified Test.QuickCheck as Q

benches :: [Benchmark]
benches =
  [ env (Q.generate $ replicateM 100 $ Q.resize 100 Q.arbitrary) $ \terms ->
      bgroup "reduceBeta"
      [ bench "reduceBeta"  $ nf (map reduceBeta)  terms
      , bench "reduceBeta2" $ nf (map reduceBeta2) terms
      ]
  ]
