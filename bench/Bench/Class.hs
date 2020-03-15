module Bench.Class where

import Control.Monad.Trans.State.Strict (State, evalState)
import Data.Conduit ((.|), ConduitT, runConduitPure)
import Data.Traversable (for)
import Numeric.Natural (Natural)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified LambdaCalculus.DeBruijn as DeBruijn
import qualified LambdaCalculus.DeBruijn2 as DeBruijn2
import qualified LambdaCalculus.Term as Term

class IsTerm a where
  mkApp :: a -> a -> a

  interpretChurchNumber :: a -> Maybe Natural

  encodeChurchNumber :: Natural -> a

  interpretChurchPair :: a -> (a, a)

  reduceSteps :: Monad m => a -> ConduitT i a m ()

  countTerm :: a -> Int

instance IsTerm Term.Term where
  mkApp = Term.App
  encodeChurchNumber = Term.encodeChurchNumber
  interpretChurchNumber = Term.interpretChurchNumber
  interpretChurchPair = Term.interpretChurchPair
  reduceSteps = Term.reduceSteps
  countTerm = Term.countTerm

instance IsTerm DeBruijn.Term where
  mkApp = DeBruijn.App
  encodeChurchNumber = DeBruijn.encodeChurchNumber
  interpretChurchNumber = DeBruijn.interpretChurchNumber
  interpretChurchPair = DeBruijn.interpretChurchPair
  reduceSteps = DeBruijn.reduceSteps
  countTerm = DeBruijn.countTerm

instance IsTerm DeBruijn2.Term where
  mkApp = DeBruijn2.App
  encodeChurchNumber = DeBruijn2.encodeChurchNumber
  interpretChurchNumber = DeBruijn2.interpretChurchNumber
  interpretChurchPair = DeBruijn2.interpretChurchPair
  reduceSteps = DeBruijn2.reduceSteps
  countTerm = DeBruijn2.countTerm

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
  , (1, 1)
  , (2, 0)
  , (2, 1)
  , (2, 2)
  , (3, 1)
  , (3, 2)
  , (4, 3)
  , (5, 3)
  , (6, 4)
  , (7, 4)
  , (8, 5)
  , (9, 6)
  , (10, 5)
  , (11, 6)
  , (12, 7)
  , (13, 7)
  ]

-- | A naive implementation which reduces a term after applying numerals.
reduce :: IsTerm a => a -> [(Maybe Natural, Maybe Natural)]
reduce m = flip map probs $ \prob -> case interpretChurchPair $ reduceTerm m of
  (m', n') ->
    ( interpretChurchNumber $ reduceTerm $ applyChurchNumber prob m'
    , interpretChurchNumber $ reduceTerm $ applyChurchNumber prob n'
    )

{-# INLINABLE reduce #-}

-- | A two-fold implementation which reduces a term before and after applying numerals.
reduce2 :: IsTerm a => a -> [(Maybe Natural, Maybe Natural)]
reduce2 m = flip map probs $ \prob -> case interpretChurchPair $ reduceTerm m of
  (m', n') ->
    ( interpretChurchNumber $ reduceTerm $ applyChurchNumber prob $ reduceTerm m'
    , interpretChurchNumber $ reduceTerm $ applyChurchNumber prob $ reduceTerm n'
    )

{-# INLINABLE reduce2 #-}

-- | Like 'reduce2' but keeps the _total_ number of steps under a certain limit.
reduce3 :: IsTerm a => a -> [(Maybe Natural, Maybe Natural)]
reduce3 = flip evalState (length probs * 1000) . go
  where
  go m = do
    m' <- reduceTermS m
    let (mfst, msnd) = interpretChurchPair m'
    mfst' <- reduceTermS mfst
    msnd' <- reduceTermS msnd
    for probs $ \prob -> do
      a' <- fmap interpretChurchNumber $ reduceTermS $ applyChurchNumber prob mfst'
      b' <- fmap interpretChurchNumber $ reduceTermS $ applyChurchNumber prob msnd'
      pure (a', b')

{-# INLINABLE reduce3 #-}
