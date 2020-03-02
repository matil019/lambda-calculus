{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt), mask, throwIO, try)
import Data.Conduit ((.|), ConduitT, runConduitPure)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Term
  ( Term
  , Var
  , countTerm
  , freeVars
  , genTerm
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
genChurchNumber = Abs 'f' . Abs 'x' <$> genTerm ['f', 'x']

encodeChurchNumber :: Int -> Term
encodeChurchNumber n
  | n < 0 = error "encodeNat: negative number"
  | otherwise = Abs 'f' $ Abs 'x' $ iterate (App (Var 'f')) (Var 'x') !! n

data Result = Result
  { r1p1 :: Maybe Int
  , r1p2 :: Maybe Int
  , r2p1 :: Maybe Int
  }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  traverse_ print . Map.toList =<< mask (\r -> loop r mempty)
  where
  one = encodeChurchNumber 1
  two = encodeChurchNumber 2
  loop :: (forall a. IO a -> IO a) -> Map Result Int -> IO (Map Result Int)
  loop restoreMask acc = do
    result <- try $ restoreMask $ do
      m <- Q.generate $ genTerm []
      let result = Result
            { r1p1 = interpretChurchNumber (App (App m one) one)
            , r1p2 = interpretChurchNumber (App (App m one) two)
            , r2p1 = interpretChurchNumber (App (App m two) one)
            }
      print result
      pure $! Map.alter (Just . (+1) . fromMaybe 0) result acc
    case result of
      Right next -> loop restoreMask next
      Left UserInterrupt -> pure acc
      Left e -> throwIO e
