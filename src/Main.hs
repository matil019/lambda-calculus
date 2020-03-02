{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt), mask, throwIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), ConduitT, runConduit, runConduitPure)
import Data.Foldable (traverse_)
import Data.Ord (Down(Down))
import Data.List (find, intercalate, sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Genetic (Individual(Individual), newGeneration)
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Genetic
import qualified Test.QuickCheck as Q

-- borrowed from "extra"
dupe :: a -> (a, a)
dupe a = (a, a)

convertAlpha :: Var -> Term -> Term
convertAlpha x (Abs y m) = Abs x $! substitute y (Var x) m
convertAlpha _ m = m

newFreeVar :: Set Var -> Var
newFreeVar except = case find (`Set.notMember` except) ['a'..'z'] of
  Just ok -> ok
  Nothing -> error "newFreeVar: no vars available"

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
formatTerm (Var x) = [x]
formatTerm (Abs x m) = "(\\" <> [x] <> "." <> formatTerm m <> ")"
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
    let m' = App (App m (Var '+')) (Var '0')
    in
    runConduitPure
        $ reduceSteps m'
       .| C.take 1000
       .| C.takeWhile ((<= 1000000) . countTerm)
       .| C.lastDef m'
  where
  go (Var '0') = Just 0
  go (App (Var '+') n) = fmap (1+) $ go n
  go _ = Nothing

genChurchNumber :: Q.Gen Term
genChurchNumber = Abs 'f' . Abs 'x' <$> genTerm (Set.fromList ['f', 'x'])

encodeChurchNumber :: Int -> Term
encodeChurchNumber n
  | n < 0 = error "encodeNat: negative number"
  | otherwise = Abs 'f' $ Abs 'x' $ iterate (App (Var 'f')) (Var 'x') !! n

data Result = Result
  { r0p0 :: Maybe Int
  , r0p1 :: Maybe Int
  , r1p0 :: Maybe Int
  , r1p1 :: Maybe Int
  , r1p2 :: Maybe Int
  , r2p0 :: Maybe Int
  , r2p1 :: Maybe Int
  , r2p2 :: Maybe Int
  }
  deriving (Eq, Ord, Show)

resultScore :: Result -> Int
resultScore Result{..} = sum $ map btoi
  [ r0p0 == Just 0
  , r0p1 == Just 1
  , r1p0 == Just 1
  , r1p1 == Just 2
  , r1p2 == Just 3
  , r2p0 == Just 2
  , r2p1 == Just 3
  , r2p2 == Just 4
  ]
  where
  btoi True  = 1
  btoi False = 0

main :: IO ()
main = do
  summary <- mask $ \restoreMask -> runConduit
     $ C.catchC (geneAlgo .| C.mapM (\a -> restoreMask $ pure a)) (\e -> case e of
         UserInterrupt -> mempty
         _ -> liftIO $ throwIO e
         )
    .| zipWithIndexC
    .| C.iterM (\((term, _, score, time), idx) -> putStrLn $ formatLabeled
         [ ("#",     show idx)
         , ("score", show score)
         , ("size",  show $ countTerm term)
         , ("time",  printf "%.4f" time)
         ])
    .| C.map (\((_, result, score, _), _) -> (result, score))
    .| C.foldl (\acc (result, score) -> Map.alter (Just . (+1) . fromMaybe (0 :: Int)) (score, result) acc) Map.empty
  traverse_ print . map (\((a, b), c) -> (a, b, c)) $ Map.toList summary
  where
  zero = encodeChurchNumber 0
  one = encodeChurchNumber 1
  two = encodeChurchNumber 2

  formatLabeled :: [(String, String)] -> String
  formatLabeled = intercalate ", " . map (\(k, v) -> k <> ": " <> v)

  -- Here, ConduitT is used like the Writer monad; the procedure is described with
  -- monadic binding, whereas the progress is written (by 'yield') to downstream so
  -- that it can be handled outside.
  geneAlgo :: ConduitT i (Term, Result, Int, Seconds) IO r
  geneAlgo = do
    terms <- liftIO $ Q.generate $ replicateM 100 arbitrary
    -- POPUlation
    popu <- traverse runMeasureYield terms
    loop popu
    where
    loop prevPopu = do
      terms <- liftIO $ Q.generate $ newGeneration prevPopu
      nextPopu <- traverse runMeasureYield terms
      loop $ take 100 $ sortOn (\Individual{score} -> Down score) (prevPopu <> nextPopu)

    runMeasureYield :: ClosedTerm -> ConduitT i (Term, Result, Int, Seconds) IO (Individual ClosedTerm)
    runMeasureYield m = do
      (time, (result, score)) <- liftIO $ duration $ do
        let !result = runTerm $ unClosedTerm m
            !score = resultScore result
        pure $! (result, score)
      C.yield (unClosedTerm m, result, score, time)
      pure $ Individual m score

  runTerm :: Term -> Result
  runTerm m = Result
    { r0p0 = interpretChurchNumber (App (App m zero) zero)
    , r0p1 = interpretChurchNumber (App (App m zero) one)
    , r1p0 = interpretChurchNumber (App (App m one ) zero)
    , r1p1 = interpretChurchNumber (App (App m one ) one)
    , r1p2 = interpretChurchNumber (App (App m one ) two)
    , r2p0 = interpretChurchNumber (App (App m two ) zero)
    , r2p1 = interpretChurchNumber (App (App m two ) one)
    , r2p2 = interpretChurchNumber (App (App m two ) two)
    }
