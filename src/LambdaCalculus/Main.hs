{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module LambdaCalculus.Main where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.Exception (SomeException, mask, throwIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), ConduitT, runConduit)
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate, sortOn)
import Data.List.Extra (maximumOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Ord (Down(Down))
import LambdaCalculus.Genetic (Individual(Individual), newGeneration)
import LambdaCalculus.Term
  ( ClosedTerm
  , Term
  , countTerm
  , encodeChurchNumber
  , formatTerm
  , interpretChurchNumber
  , unClosedTerm
  , pattern App
  )
import Numeric.Natural (Natural)
import System.IO (BufferMode(LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Time.Extra (Seconds, duration)
import Test.QuickCheck (arbitrary)
import Text.Printf (printf)

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified Test.QuickCheck as Q

average :: NonEmpty Double -> Double
average xs = sum xs / realToFrac (length xs)

log10 :: Double -> Double
log10 x = log x / log 10

zipWithIndexC :: Monad m => ConduitT a (a, Int) m ()
zipWithIndexC = loop 0
  where
  loop i = do
    ma <- C.await
    case ma of
      Nothing -> pure ()
      Just a -> do
        C.yield (a, i)
        loop $! i + 1

newtype Result = Result [(Natural, Natural, Maybe Natural)]
  deriving (Eq, Ord, Show)

resultScore :: Result -> Int
resultScore (Result xs) = sum $ map (\(a, b, r) -> btoi (Just (a + b) == r)) xs
  where
  btoi True  = 1
  btoi False = 0

data Event
  = RunEvent (Term, Result, Double, Seconds)
  | GenEvent [(ClosedTerm, Double)]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  mask $ \restoreMask -> do
    exref <- newIORef Nothing
    best <- runConduit
       $ C.catchC (geneAlgo .| C.mapM (\a -> restoreMask $ pure a))
           (\e -> liftIO $ writeIORef exref (Just (e :: SomeException)))
      .| iterPrintEvent
      .| C.concatMap (\evt -> case evt of
           RunEvent _ -> Nothing
           GenEvent popu -> fmap (maximumOn (\(_, score) -> score) . NE.toList) $ nonEmpty popu
           )
      .| C.last
    for_ best $ \(m, score) -> do
      hPutStrLn stderr $ "final best score: " <> show score
      hPutStrLn stderr $ formatTerm $ unClosedTerm m
    traverse_ throwIO =<< readIORef exref
  where
  iterPrintEvent :: ConduitT Event Event IO ()
  iterPrintEvent = loop (0 :: Int) (0 :: Int)
    where
    loop !runIdx !genIdx = do
      mevt <- C.await
      for_ mevt $ \evt -> case evt of
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
            , ("size avg",   printf "%.03f" $ ne0 average $ map (\(m, _) -> realToFrac $ countTerm $ unClosedTerm m) popu)
            ]
          C.yield evt
          loop runIdx (genIdx + 1)

  formatLabeled :: [(String, String)] -> String
  formatLabeled = intercalate ", " . map (\(k, v) -> k <> ": " <> v)

  -- Here, ConduitT is used like the Writer monad; the procedure is described with
  -- monadic binding, whereas the progress is written (by 'yield') to downstream so
  -- that it can be handled outside.
  geneAlgo :: ConduitT i Event IO r
  geneAlgo = do
    terms <- liftIO $ Q.generate $ replicateM numPopulation arbitrary
    -- POPUlation
    popu <- traverse runMeasureYield terms `C.fuseUpstream` C.map RunEvent
    C.yield $ GenEvent popu
    loop popu
    where
    loop prevPopu = do
      terms <- liftIO $ Q.generate $ newGeneration $ map (\(m, score) -> Individual m (scoreToWeight score)) prevPopu
      nextPopu <- traverse runMeasureYield terms `C.fuseUpstream` C.map RunEvent
      mergedPopu <- liftIO $ Q.generate $ do
        let bothPopu = sortOn (\(_, score) -> Down score) (nextPopu <> prevPopu)
            numElite = numPopulation * 2 `div` 5
            numRandom = numPopulation - numElite
            (elitePopu, badPopu) = splitAt numElite bothPopu
        randomPopu <-
          let weighted = map (\x@(_, score) -> (scoreToWeight score, pure x)) badPopu
          in replicateM numRandom $ Q.frequency weighted
        pure $ elitePopu <> randomPopu
      C.yield $ GenEvent mergedPopu
      loop mergedPopu

    scoreToWeight score = max 1 $ round $ 1000 * score

    numPopulation = 1000

    runMeasureYield :: ClosedTerm -> ConduitT i (Term, Result, Double, Seconds) IO (ClosedTerm, Double)
    runMeasureYield m = do
      (time, (result, score)) <- liftIO $ duration $ do
        let !result = runTerm $ unClosedTerm m
            !score = realToFrac (resultScore result) - log10 (realToFrac $ countTerm $ unClosedTerm m) / 10
        pure $! (result, score)
      C.yield (unClosedTerm m, result, score, time)
      pure (m, score)

  runTerm :: Term -> Result
  runTerm m = Result $ map (\(a, b) -> (a, b, f a b)) probs
    where
    f a b = interpretChurchNumber (App (App m (encodeChurchNumber a)) (encodeChurchNumber b))
    probs =
      [ (0, 0)
      , (0, 1)
      , (1, 0)
      , (1, 1)
      , (1, 2)
      , (2, 0)
      , (2, 1)
      , (2, 2)
      ]
