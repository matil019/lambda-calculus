module Main where

import Control.Monad (replicateM)
import Criterion.Main

import qualified Bench.Class as Class
import qualified Bench.DeBruijn as DeBruijn
import qualified Bench.DeBruijn2 as DeBruijn2
import qualified Bench.Term as Term
import qualified LambdaCalculus.DeBruijn as DeBruijn
import qualified Test.QuickCheck as Q

main :: IO ()
main = defaultMain
  [ env (Q.generate $ replicateM 100 $ Term.unClosedTerm <$> Q.arbitrary) $ \terms ->
      bgroup "bench"
      [ bgroup "Term"
        [ bench "reduce"  $ nf (map Term.reduce)  terms
        , bench "reduce2" $ nf (map Term.reduce2) terms
        , bench "reduce3" $ nf (map Term.reduce3) terms
        ]
      , let terms' = map (snd . DeBruijn.toDeBruijn []) terms
        in -- TODO make sure that DeBruijn terms actually reduce the same as the other notation
        bgroup "DeBruijn"
        [ bench "reduce"  $ nf (map DeBruijn.reduce)  terms'
        , bench "reduce2" $ nf (map DeBruijn.reduce2) terms'
        , bench "reduce3" $ nf (map DeBruijn.reduce3) terms'
        ]
      , let terms' = map (snd . DeBruijn2.toDeBruijn []) terms
        in -- TODO make sure that DeBruijn terms actually reduce the same as the other notation
        bgroup "DeBruijn2"
        [ bench "reduce"  $ nf (map DeBruijn2.reduce)  terms'
        , bench "reduce2" $ nf (map DeBruijn2.reduce2) terms'
        , bench "reduce3" $ nf (map DeBruijn2.reduce3) terms'
        ]
      , bgroup "IsTerm Term"
        [ bench "reduce"  $ nf (map Class.reduce)  terms
        , bench "reduce2" $ nf (map Class.reduce2) terms
        , bench "reduce3" $ nf (map Class.reduce3) terms
        ]
      ]
  ]
