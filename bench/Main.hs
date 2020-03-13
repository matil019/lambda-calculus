module Main where

import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict (State, evalState)
import Criterion.Main
import Data.Conduit ((.|), runConduitPure)
import Numeric.Natural (Natural)
import LambdaCalculus.Term

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Test.QuickCheck as Q

reduceTerm :: Term -> Term
reduceTerm m = runConduitPure
   $ reduceSteps m
  .| C.take 1000
  .| C.takeWhile ((<= 1000) . countTerm)
  .| C.lastDef m

reduceTermS :: Term -> State Int Term
reduceTermS m = do
  remainingSteps <- State.get
  let (result, n) = runConduitPure
         $ reduceSteps m
        .| C.take remainingSteps
        .| C.takeWhile ((<= 1000) . countTerm)
        .| C.getZipSink ((,) <$> C.ZipSink (C.lastDef m) <*> C.ZipSink C.length)
  State.put (remainingSteps - n)
  pure result

apply :: Term -> Term
apply m = App (App m (encodeChurchNumber 2)) (encodeChurchNumber 1)

-- | A naive implementation which reduces a term after applying numerals.
reduce :: Term -> Maybe Natural
reduce m = interpretChurchNumber $ reduceTerm $ apply m

-- | A two-fold implementation which reduces a term before and after applying numerals.
reduce2 :: Term -> Maybe Natural
reduce2 m = interpretChurchNumber $ reduceTerm $ apply $ reduceTerm m

-- | Like 'reduce2' but keeps the _total_ number of steps under a certain limit.
reduce3 :: Term -> Maybe Natural
reduce3 m = flip evalState 1000 $ do
  m' <- reduceTermS m
  m'' <- reduceTermS $ apply m'
  pure $ interpretChurchNumber m''

main :: IO ()
main = defaultMain
  [ env (Q.generate $ replicateM 100 $ unClosedTerm <$> Q.arbitrary) $ \terms ->
      bgroup "Term"
      [ bench "reduce"  $ nf (map reduce)  terms
      , bench "reduce2" $ nf (map reduce2) terms
      , bench "reduce3" $ nf (map reduce3) terms
      ]
  ]
