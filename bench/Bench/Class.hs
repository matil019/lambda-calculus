module Bench.Class where

import Control.Monad.Trans.State.Strict (State, evalState)
import Data.Conduit ((.|), ConduitT, runConduitPure)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for)
import Numeric.Natural (Natural)

import qualified Bench.Term as Term
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C

class IsTerm a where
  mkApp :: a -> a -> a

  interpretChurchNumber :: a -> Maybe Natural

  encodeChurchNumber :: Natural -> a

  reduceSteps :: Monad m => a -> ConduitT i a m ()

  countTerm :: a -> Int

instance IsTerm Term.Term where
  mkApp = Term.App
  encodeChurchNumber = Term.encodeChurchNumber
  interpretChurchNumber = Term.interpretChurchNumber
  reduceSteps = Term.reduceSteps
  countTerm = Term.countTerm

reduceTerm :: IsTerm a => a -> a
reduceTerm m = runConduitPure
   $ reduceSteps m
  .| C.take 1000
  .| C.takeWhile ((<= 1000) . countTerm)
  .| C.lastDef m

reduceTermS :: IsTerm a => a -> State Int a
reduceTermS m = do
  remainingSteps <- State.get
  let (result, n) = runConduitPure
         $ reduceSteps m
        .| C.take remainingSteps
        .| C.takeWhile ((<= 1000) . countTerm)
        .| C.getZipSink ((,) <$> C.ZipSink (C.lastDef m) <*> C.ZipSink C.length)
  State.put (remainingSteps - n)
  pure result

applyChurchNumber :: IsTerm a => (Natural, Natural) -> a -> a
applyChurchNumber (a, b) m = mkApp (mkApp m (encodeChurchNumber a)) (encodeChurchNumber b)

probs :: [(Natural, Natural)]
probs =
  [ (0, 0)
  , (1, 0)
  , (2, 1)
  , (5, 3)
  ]

-- | A naive implementation which reduces a term after applying numerals.
reduce :: IsTerm a => a -> [Natural]
reduce m = mapMaybe (interpretChurchNumber . reduceTerm . flip applyChurchNumber m) probs

{-# INLINABLE reduce #-}

-- | A two-fold implementation which reduces a term before and after applying numerals.
reduce2 :: IsTerm a => a -> [Natural]
reduce2 m = mapMaybe (interpretChurchNumber . reduceTerm . flip applyChurchNumber (reduceTerm m)) probs

{-# INLINABLE reduce2 #-}

-- | Like 'reduce2' but keeps the _total_ number of steps under a certain limit.
reduce3 :: IsTerm a => a -> [Natural]
reduce3 = flip evalState (length probs * 1000) . go
  where
  go m = do
    m' <- reduceTermS m
    fmap catMaybes $ for probs $ \prob -> do
      m'' <- reduceTermS $ applyChurchNumber prob m'
      pure $ interpretChurchNumber m''

{-# INLINABLE reduce3 #-}
