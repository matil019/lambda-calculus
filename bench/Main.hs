module Main where

import Control.Monad (replicateM)
import Criterion.Main

import qualified Bench.Term as Term
import qualified LambdaCalculus.Term as Term
import qualified Test.QuickCheck as Q

main :: IO ()
main = defaultMain
  [ env (Q.generate $ replicateM 100 $ Term.unClosedTerm <$> Q.arbitrary) $ \terms ->
      bgroup "Term"
      [ bench "reduce"  $ nf (map Term.reduce)  terms
      , bench "reduce2" $ nf (map Term.reduce2) terms
      , bench "reduce3" $ nf (map Term.reduce3) terms
      ]
  ]
