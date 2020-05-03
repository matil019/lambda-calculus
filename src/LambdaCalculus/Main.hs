{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- | The main function and utilities.
module LambdaCalculus.Main where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, stateTVar)
import Control.Concurrent.STM.TMQueue (closeTMQueue, newTMQueueIO, readTMQueue, writeTMQueue)
import Control.Exception (SomeException, mask, throwIO)
import Control.Lens (both, over)
import Control.Monad (join, replicateM)
import Control.Monad.Extra (whenJust, whenJustM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Conduit ((.|), ConduitT, runConduit, runConduitPure)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate, sortOn)
import Data.List.Extra (maximumOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Ord (Down(Down))
import LambdaCalculus.SimplyTyped.DeBruijn
  ( GeneticTerm
  , Term(App)
  , TypeSet
  , countTerm
  , encodeChurchNumber
  , formatTerm
  , genCandidateConst
  , interpretChurchNumber
  , interpretChurchPair
  , reduceSteps
  , runGeneticTerm
  )
import LambdaCalculus.Genetic (Individual(Individual), newGeneration)
import Numeric.Natural (Natural)
import System.Environment (getArgs, withArgs)
import System.Exit (exitSuccess)
import System.IO (BufferMode(LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Random (split)
import System.Time.Extra (Seconds, duration)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (QCGen, mkQCGen, newQCGen)
import Text.Printf (printf)
import UnliftIO (finally, pooledMapConcurrently)

import qualified Control.Monad.Reader as Reader
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified LambdaCalculus.Main.Repl as Repl
import qualified Test.QuickCheck as Q

-- | Calculates an average.
average :: NonEmpty Double -> Double
average xs = sum xs / realToFrac (length xs)

-- | Calculates a common logarithm (i.e. base 10).
log10 :: Double -> Double
log10 x = log x / log 10

-- | Runs 'Gen' deterministically by splitting @QCGen@ in the reader monad.
runGen :: (MonadIO m, MonadReader (TVar QCGen) m) => Int -> Gen a -> m a
runGen size gen = do
  qcgenVar <- Reader.ask
  qcgen <- liftIO $ atomically $ stateTVar qcgenVar split
  pure $ unGen gen qcgen size

-- | Yields 'Just's and halts at the first 'Nothing'.
unNoneTerminateC :: Monad m => ConduitT (Maybe a) a m ()
unNoneTerminateC = whenJustM (join <$> C.await) $ \a -> do
  C.yield a
  unNoneTerminateC

-- | @iterPerMC n f@ runs @f@ for its effects every @n@ elements.
--
-- @
-- 'iterPerMC' 1 == 'C.iterM'
-- @
--
-- >>> runConduit $ yieldMany [1..5] .| iterPerMC 2 print .| sinkNull
-- 1
-- 3
-- 5
iterPerMC :: Monad m => Int -> (a -> m ()) -> ConduitT a a m ()
iterPerMC period f = loop
  where
  period_1 = period - 1
  loop = whenJustM C.await $ \a -> do
    C.yield a .| C.iterM f
    C.take period_1
    loop

-- | Zips elements with indexes, starting at 0.
zipWithIndexC :: Monad m => ConduitT a (a, Int) m ()
zipWithIndexC = loop 0
  where
  loop !i = whenJustM C.await $ \a -> do
    C.yield (a, i)
    loop (i + 1)

-- | Runs conduits concurrently. 'pooledMapConcurrently' for 'ConduitT'.
pooledMapConcurrentlyC :: (MonadIO m, Traversable t) => (a -> ConduitT () o IO r) -> t a -> ConduitT i o m (t r)
pooledMapConcurrentlyC f as = do
  q <- liftIO newTMQueueIO
  asy <- liftIO $ async $
    let producer a = runConduit $ f a `C.fuseUpstream` C.mapM_ (liftIO . atomically . writeTMQueue q)
    in pooledMapConcurrently producer as `finally` (atomically $ closeTMQueue q)
  C.repeatM (liftIO $ atomically $ readTMQueue q) .| unNoneTerminateC
  liftIO $ wait asy

-- | A result of evaluating a lambda term.
newtype Result = Result [(Natural, Natural, (Maybe Natural, Maybe Natural))]
  deriving (Eq, Ord, Show)

-- | Evaluates a score of a 'Result'. The larger, the better.
--
-- This doesn't include the size penalty.
resultScore :: Result -> Int
resultScore (Result xs) = sum $ flip map xs
  $ \(a, b, (r1, r2)) -> justEq (a + b) r1 + justEq (a - b) r2
  where
  justEq r (Just m)
    | m == r = 2
    | otherwise = 1
  justEq _ Nothing = 0

-- | A null data type for use with 'TypeSet'
data LikeUntyped

instance TypeSet LikeUntyped where
  genCandidateConst _ = pure Nothing

-- | An event in a progress of running Genetic Algorithm.
data Event
  = RunEvent (Term, Result, Double, Seconds)
  | GenEvent [(GeneticTerm LikeUntyped, Double)]

-- | The main function.
main :: IO ()
main = do
  -- start repl if the first argument is "repl"
  do
    args <- getArgs
    case args of
      ("repl":more) -> withArgs more Repl.main >> exitSuccess
      _ -> pure ()

  hSetBuffering stdout LineBuffering
  seed <- do
    args <- getArgs
    case args of
      [] -> pure Nothing
      (a:_) -> Just <$> readIO a
  mask $ \restoreMask -> do
    exref <- newIORef Nothing
    qcgenVar <- newTVarIO =<< case seed of
      Just i -> pure $ mkQCGen i
      Nothing -> newQCGen
    best <- flip runReaderT qcgenVar
       $ runConduit
       $ C.catchC (geneAlgo .| C.mapM (\a -> liftIO $ restoreMask $ pure a))
           (\e -> liftIO $ writeIORef exref (Just (e :: SomeException)))
      .| iterPrintEvent
      .| C.concatMap (\evt -> case evt of
           RunEvent _ -> Nothing
           GenEvent popu -> fmap (maximumOn (\(_, score) -> score) . NE.toList) $ nonEmpty popu
           )
      .| iterPerMC 1000 (\(m, score) -> liftIO $ putStrLn $
           "current best score: " <> show score <> ", term: " <> formatTerm (runGeneticTerm m))
      .| C.last
    whenJust best $ \(m, score) -> do
      hPutStrLn stderr $ "final best score: " <> show score
      hPutStrLn stderr $ formatTerm $ runGeneticTerm m
    whenJustM (readIORef exref) throwIO
  where
  iterPrintEvent :: MonadIO m => ConduitT Event Event m ()
  iterPrintEvent = loop (0 :: Int) (0 :: Int)
    where
    loop !runIdx !genIdx = whenJustM C.await $ \evt -> case evt of
      RunEvent (term, _, score, time) -> do
        liftIO $ putStrLn $ formatLabeled
          [ ("#gen",  show genIdx)
          , ("#step", show runIdx)
          , ("score", printf "%.03f" score)
          , ("size",  show $ countTerm term)
          , ("time",  printf "%.4f" time)
          ]
        C.yield evt
        loop (runIdx + 1) genIdx
      GenEvent popu -> do
        let ne0 f = maybe 0 f . nonEmpty
        liftIO $ putStrLn $ formatLabeled
          [ ("generation", show genIdx)
          , ("score best", printf "%.03f" $ ne0 maximum $ map (\(_, score) -> score) popu)
          , ("score avg",  printf "%.03f" $ ne0 average $ map (\(_, score) -> score) popu)
          , ("size avg",   printf "%.03f" $ ne0 average $ map (\(m, _) -> realToFrac $ countTerm $ runGeneticTerm m) popu)
          ]
        C.yield evt
        loop runIdx (genIdx + 1)

  formatLabeled :: [(String, String)] -> String
  formatLabeled = intercalate ", " . map (\(k, v) -> k <> ": " <> v)

  -- Here, ConduitT is used like the Writer monad; the procedure is described with
  -- monadic binding, whereas the progress is written (by 'yield') to downstream so
  -- that it can be handled outside.
  geneAlgo :: (MonadIO m, MonadReader (TVar QCGen) m) => ConduitT i Event m r
  geneAlgo = do
    terms <- runGen 30 $ replicateM numPopulation arbitrary
    -- POPUlation
    popu <- pooledMapConcurrentlyC (\term -> (term,) <$> runMeasureYield (runGeneticTerm term)) terms
    C.yield $ GenEvent popu
    loop popu
    where
    loop prevPopu = do
      terms <- runGen 30 $ newGeneration $ map (\(m, score) -> Individual m (scoreToWeight score)) prevPopu
      nextPopu <- pooledMapConcurrentlyC (\term -> (term,) <$> runMeasureYield (runGeneticTerm term)) terms
      mergedPopu <- runGen 30 $ do
        let bothPopu = sortOn (\(_, score) -> Down score) (nextPopu <> prevPopu)
            numElite = numPopulation `div` 5
            numRandom = numPopulation - numElite
            (elitePopu, badPopu) = splitAt numElite bothPopu
        randomPopu <-
          let weighted = map (\x@(_, score) -> (scoreToWeight score, pure x)) badPopu
          in replicateM numRandom $ Q.frequency weighted
        pure $ elitePopu <> randomPopu
      C.yield $ GenEvent mergedPopu
      loop mergedPopu

    scoreToWeight score = round $ max 1 $ 1000 * score

    numPopulation = 1000

    runMeasureYield :: MonadIO m => Term -> ConduitT i Event m Double
    runMeasureYield m = do
      (time, (result, score)) <- liftIO $ duration $ do
        let !result = runTerm m
            !score = realToFrac (resultScore result) - sqrt (realToFrac $ countTerm m) / 10
        pure $! (result, score)
      C.yield $ RunEvent (m, result, score, time)
      pure score

  runTerm :: Term -> Result
  runTerm m = Result $ flip map probs $ \(a, b) ->
    let apply x = App (App x (encodeChurchNumber a)) (encodeChurchNumber b)
    in (a, b, over both (interpretChurchNumber . redu . apply) $ interpretChurchPair $ redu m)
    where
    redu :: Term -> Term
    redu x = runConduitPure
       $ reduceSteps x
      .| C.take 1000
      .| C.takeWhile ((<= 1000) . countTerm)
      .| C.lastDef x

    probs =
      [ (0, 0)
      , (1, 0)
      , (1, 1)
      , (2, 0)
      , (2, 1)
      , (2, 2)
      , (3, 1)
      , (3, 2)
      ]
