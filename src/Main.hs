{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Exception (SomeException, mask, throwIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), ConduitT, runConduit, runConduitPure)
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find, intercalate, sortOn)
import Data.List.Extra (maximumOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Ord (Down(Down))
import Data.Set (Set)
import Genetic (Individual(Individual), newGeneration)
import System.IO (BufferMode(LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Time.Extra (Seconds, duration)
import Term
  ( ClosedTerm
  , Term
  , Var
  , countTerm
  , freeVars
  , genTerm
  , unClosedTerm
  , pattern Abs
  , pattern App
  , pattern Var
  )
import Test.QuickCheck (arbitrary)
import Text.Printf (printf)

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Test.QuickCheck as Q

-- borrowed from "extra"
dupe :: a -> (a, a)
dupe a = (a, a)

average :: NonEmpty Double -> Double
average xs = sum xs / realToFrac (length xs)

log10 :: Double -> Double
log10 x = log x / log 10

convertAlpha :: Var -> Term -> Term
convertAlpha x (Abs y m) = Abs x $! substitute y (Var x) m
convertAlpha _ m = m

newFreeVar :: Set Var -> Var
newFreeVar except = case find (`Set.notMember` except) infinitealphabets of
  Just ok -> ok
  Nothing -> error "newFreeVar: no vars available"
  where
  -- infinite list of strings, "a" : "b" : ... : "z" : "aa" : "ab" : ...
  infinitealphabets = concat $ iterate (\ss -> [ c:s | c <- ['a'..'z'], s <- ss ]) [ [c] | c <- ['a'..'z'] ]

substitute :: Var -> Term -> Term -> Term
substitute x n (Var y)
  | x == y    = n
  | otherwise = Var y
substitute x n (App m1 m2) =
  let !m1' = substitute x n m1
      !m2' = substitute x n m2
  in n `seq` App m1' m2'
substitute x n (Abs y m)
  | x == y = Abs y m
  | x /= y && y `notElem` freeVars n = Abs y $! substitute x n m
  -- TODO not needed to recurse substitute again, but for that it needs a distinct @Abs@ type
  | otherwise = substitute x n $! convertAlpha (newFreeVar (Set.insert x (freeVars n))) (Abs y m)

-- | Performs beta-reduction.
--
-- Automatically does alpha-conversions if needed.
reduceBeta :: Term -> Term
reduceBeta (App (Abs x m) n) = substitute x n m
reduceBeta m = m

reduceStep :: Term -> Maybe Term
reduceStep (Var _) = Nothing
reduceStep (Abs _ _) = Nothing
reduceStep m@(App (Abs _ _) _) = Just $ reduceBeta m
reduceStep (App m n) = case reduceStep m of
  Just m' -> Just $ App m' n
  Nothing -> App m <$> reduceStep n

reduceSteps :: Monad m => Term -> ConduitT i Term m ()
reduceSteps = C.unfold (fmap dupe . reduceStep)

formatTerm :: Term -> String
formatTerm (Var x) = x
formatTerm (Abs x m) = "(\\" <> x <> "." <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

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

interpretChurchNumber :: Term -> Maybe Int
interpretChurchNumber = \m ->
  go $
    let m' = App (App m (Var "+")) (Var "0")
    in
    runConduitPure
        $ reduceSteps m'
       .| C.take 1000
       .| C.takeWhile ((<= 1000000) . countTerm)
       .| C.lastDef m'
  where
  go (Var "0") = Just 0
  go (App (Var "+") n) = fmap (1+) $ go n
  go _ = Nothing

genChurchNumber :: Q.Gen Term
genChurchNumber = Abs "f" . Abs "x" <$> genTerm (Set.fromList ["f", "x"])

encodeChurchNumber :: Int -> Term
encodeChurchNumber n
  | n < 0 = error "encodeNat: negative number"
  | otherwise = Abs "f" $ Abs "x" $ iterate (App (Var "f")) (Var "x") !! n

data Result = Result [(Int, Int, Maybe Int)]
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
