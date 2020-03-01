{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt), mask, throwIO, try)
import Data.Conduit ((.|), ConduitT, runConduitPure)
import Data.Map.Strict (Map)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Term
  ( Term
  , Var
  , countTerm
  , freeVars
  , pattern Abs
  , pattern App
  , pattern Var
  )

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Map.Strict as Map
import qualified Test.QuickCheck as Q

-- borrowed from "extra"
dupe :: a -> (a, a)
dupe a = (a, a)

convertAlpha :: Var -> Term -> Term
convertAlpha x (Abs y m) = Abs x $! substitute y (Var x) m
convertAlpha _ m = m

newFreeVar :: [Var] -> Var
newFreeVar except = case find (`notElem` except) ['a'..'z'] of
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
  | otherwise = substitute x n $! convertAlpha (newFreeVar (x : freeVars n)) (Abs y m)

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
reduceStep (App m n) = (\m' -> App m' n) <$> reduceStep m

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

interpretChurchNumber :: Term -> (Maybe Int)
interpretChurchNumber = \m -> go $ App (App m (Var '+')) (Var '0')
  where
  go m = do
    let a = runConduitPure
          $ reduceSteps m
         .| C.take 1000
         .| C.takeWhile ((<= 1000000) . countTerm)
         .| C.lastDef m
    case a of
      Var '0' -> Just 0
      App (Var '+') n -> fmap (1+) $ go n
      _ -> Nothing

genFoo :: Q.Gen Term
genFoo =
  fmap (\m -> Abs 'f' (Abs 'x' m)) $ genBound ['f', 'x']
  where
  -- assume that the probability of picking 'genVar' is @p@
  -- and the other two are @(1 - p) / 2@, resp.
  -- then, to have the expected value of the number of terms to be @X@,
  -- > p = (X + 2) / 3X
  genBound :: [Var] -> Q.Gen Term
  genBound bound = do
    size <- max 1 <$> Q.getSize
    -- @(p / 100)%@: the probability of picking 'genVar'
    let p = 10000 * (size + 2) `div` (3 * size)
        q = (10000 - p) `div` 2
    Q.frequency [(p, genVar), (q, genAbs), (q, genApp)]
    where
    -- 1 term
    genVar = Var <$> Q.elements bound
    -- X + 1 terms
    genAbs = do
      fresh <- Q.elements ['a'..'z']
      Abs fresh <$> genBound (fresh:bound)
    -- 2X + 1 terms
    genApp = App <$> genBound bound <*> genBound bound

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  print =<< mask (\r -> loop r mempty)
  where
  loop :: (forall a. IO a -> IO a) -> Map (Maybe Int) Int -> IO (Map (Maybe Int) Int)
  loop restoreMask acc = do
    let calcNext = do
          term <- Q.generate genFoo
          let result = interpretChurchNumber term
          case result of
            Just i -> print i
            Nothing -> putChar '.'
          pure $! Map.alter (Just . (+1) . fromMaybe 0) result acc
    result <- try $ restoreMask calcNext
    case result of
      Right next -> loop restoreMask next
      Left UserInterrupt -> pure acc
      Left e -> throwIO e
