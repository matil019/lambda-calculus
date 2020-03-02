{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt), mask, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit ((.|), ConduitT, runConduit, runConduitPure)
import Data.Foldable (traverse_)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Term
  ( Term
  , Var
  , countTerm
  , freeVars
  , genModifiedTerm
  , genTerm
  , pattern Abs
  , pattern App
  , pattern Var
  )

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

-- latest and best
data State = State (Term, Result, Int) (Term, Int)

main :: IO ()
main = do
  summary <- mask $ \r -> runConduit
     $ C.catchC (go r) (\e -> case e of
         UserInterrupt -> mempty
         _ -> liftIO $ throwIO e
         )
    .| zipWithIndexC
    .| C.iterM (\((State (term, _, score) (_, bestScore)), idx) -> putStrLn $ formatLabeled
         [ ("#",     show idx)
         , ("best",  show bestScore)
         , ("score", show score)
         , ("size",  show $ countTerm term)
         ])
    .| C.map (\(State latest _, _) -> latest)
    .| C.foldl (\acc (_, result, score) -> Map.alter (Just . (+1) . fromMaybe (0 :: Int)) (score, result) acc) Map.empty
  traverse_ print . map (\((a, b), c) -> (a, b, c)) $ Map.toList summary
  where
  zero = encodeChurchNumber 0
  one = encodeChurchNumber 1
  two = encodeChurchNumber 2

  formatLabeled :: [(String, String)] -> String
  formatLabeled = intercalate ", " . map (\(k, v) -> k <> ": " <> v)

  go :: (forall a. IO a -> IO a) -> ConduitT i State IO ()
  go restoreMask = do
    (m, result, score) <- liftIO $ restoreMask $ do
      m <- Q.generate $ genTerm Set.empty
      let !result = runTerm m
          !score = resultScore result
      pure $! (m, result, score)
    C.yield $ State (m, result, score) (m, score)
    loop m score
    where
    loop :: Term -> Int -> ConduitT i State IO ()
    loop bestTerm bestScore = do
      (m, result, score) <- liftIO $ restoreMask $ do
        m <- liftIO $ Q.generate $ genModifiedTerm Set.empty bestTerm
        let !result = runTerm m
            !score = resultScore result
        pure $! (m, result, score)
      C.yield $ State (m, result, score) (bestTerm, bestScore)
      if score >= bestScore
        then loop m score
        else loop bestTerm score

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
