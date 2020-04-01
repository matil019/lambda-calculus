module Main where

import Bench.Class
import Control.Monad (replicateM)
import Criterion.Main

import qualified LambdaCalculus.DeBruijn as DeBruijn
import qualified LambdaCalculus.DeBruijn2 as DeBruijn2
import qualified LambdaCalculus.Term as Term
import qualified Test.QuickCheck as Q

benches :: IsTerm a => [a] -> [Benchmark]
benches terms =
  [ bench "reduce"  $ nf (map reduce)  terms
  , bench "reduce2" $ nf (map reduce2) terms
  , bench "reduce3" $ nf (map reduce3) terms
  ]

main :: IO ()
main = defaultMain
  [ env (Q.generate $ replicateM 100 $ Term.unClosedTerm <$> Q.arbitrary) $ \terms ->
      bgroup "bench"
      [ bgroup "Term" $ benches terms
      -- TODO make sure that DeBruijn terms actually reduce the same as the other notation
      , bgroup "DeBruijn"  $ benches $ map (snd . DeBruijn.toDeBruijn mempty) terms
      , bgroup "DeBruijn2" $ benches $ map (snd . DeBruijn2.toDeBruijn mempty) terms
      ]
  ]
