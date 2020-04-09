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
      bgroup "reduce"
      [ bgroup "Term" $ benches terms
      -- TODO make sure that DeBruijn terms actually reduce the same as the other notation
      , bgroup "DeBruijn"  $ benches $ map (snd . DeBruijn.toDeBruijn mempty) terms
      , bgroup "DeBruijn2" $ benches $ map (snd . DeBruijn2.toDeBruijn mempty) terms
      ]
  , env (fmap (map $ snd . DeBruijn.toDeBruijn mempty) $ Q.generate $ replicateM 100 $ Term.unClosedTerm <$> Q.arbitrary) $ \terms ->
      bgroup "countTerm"
        [ bench "countTerm"  $ nf (map DeBruijn.countTerm)  terms
        , bench "countTerm2" $ nf (map DeBruijn.countTerm2) terms
        ]
  , bgroup "index"
      [ let gen = do
              -- an arbitrary closed term and an arbitrary index in it
              DeBruijn.ClosedTerm m <- Q.arbitrary
              i <- Q.choose (0, DeBruijn.countTerm m - 1)
              pure (m, i)
        in
        env (Q.generate $ replicateM 1000 $ Q.resize 200 gen) $ \terms ->
          bgroup "individual"
            [ bench "baseline" $ nf (map (\(term, _) -> Just term)) terms
            , bgroup "midpoint" $
                let mid m = DeBruijn.countTerm m `div` 2
                in
                [ bench "index"    $ nf (map (\(term, _) -> DeBruijn.index  (mid term) term)) terms
                , bench "index2"   $ nf (map (\(term, _) -> DeBruijn.index2 (mid term) term)) terms
                ]
            , bgroup "head"
                [ bench "index"    $ nf (map (\(term, _) -> DeBruijn.index  0 term)) terms
                , bench "index2"   $ nf (map (\(term, _) -> DeBruijn.index2 0 term)) terms
                ]
            , bgroup "last"
                [ bench "index"    $ nf (map (\(term, _) -> DeBruijn.index  (DeBruijn.countTerm term - 1) term)) terms
                , bench "index2"   $ nf (map (\(term, _) -> DeBruijn.index2 (DeBruijn.countTerm term - 1) term)) terms
                ]
            , bgroup "random"
                [ bench "index"    $ nf (map (\(term, i) -> DeBruijn.index  i term)) terms
                , bench "index2"   $ nf (map (\(term, i) -> DeBruijn.index2 i term)) terms
                ]
            ]
      , env (fmap (map $ snd . DeBruijn.toDeBruijn mempty) $ Q.generate $ replicateM 100 $ Term.unClosedTerm <$> Q.arbitrary) $ \terms ->
          bgroup "all" $
            let allof f term = map (flip f term) $ [0..(DeBruijn.countTerm term - 1)]
            in
            [ bench "toList (baseline)" $ nf (map DeBruijn.toList) terms
            , bench "index"    $ nf (map $ allof DeBruijn.index) terms
            , bench "index2"   $ nf (map $ allof DeBruijn.index2) terms
            ]
      ]
  ]
