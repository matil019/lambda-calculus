module Bench.Term where

import Control.Monad.Trans.State.Strict (State, evalState)
import Data.Conduit ((.|), runConduitPure)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for)
import Numeric.Natural (Natural)
import LambdaCalculus.Term

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C

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

apply :: (Natural, Natural) -> Term -> Term
apply (a, b) m = App (App m (encodeChurchNumber a)) (encodeChurchNumber b)

probs :: [(Natural, Natural)]
probs =
  [ (0, 0)
  , (1, 0)
  , (2, 1)
  , (5, 3)
  ]

-- | A naive implementation which reduces a term after applying numerals.
reduce :: Term -> [Natural]
reduce m = mapMaybe (interpretChurchNumber . reduceTerm . flip apply m) probs

-- | A two-fold implementation which reduces a term before and after applying numerals.
reduce2 :: Term -> [Natural]
reduce2 m = mapMaybe (interpretChurchNumber . reduceTerm . flip apply (reduceTerm m)) probs

-- | Like 'reduce2' but keeps the _total_ number of steps under a certain limit.
reduce3 :: Term -> [Natural]
reduce3 = flip evalState (length probs * 1000) . go
  where
  go m = do
    m' <- reduceTermS m
    fmap catMaybes $ for probs $ \prob -> do
      m'' <- reduceTermS $ apply prob m'
      pure $ interpretChurchNumber m''
