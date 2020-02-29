{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Exception (AsyncException(UserInterrupt), mask, throwIO, try)
import Data.Functor.Const (Const(Const), getConst)
import Data.Map.Strict (Map)
import Data.List (find, unfoldr)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Monoid (All(All), Any(Any), getAll, getAny)
import Lens.Micro
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Test.QuickCheck as Q

-- borrowed from "lens"
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll . getConst . l (Const . All . f)

-- borrowed from "lens"
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny . getConst . l (Const . Any . f)

-- borrowed from "extra"
dupe :: a -> (a, a)
dupe a = (a, a)

type Var = Char

data Term
  = Var Var
  | Abs Var Term
  | App Term Term
  deriving (Eq, Show)

instance Arbitrary Term where
  arbitrary = Q.oneof [Var <$> genVar, genAbs, genApp]
    where
    genVar = Q.elements ['a'..'z']
    genAbs = Abs <$> genVar <*> arbitrary
    genApp = App <$> arbitrary <*> arbitrary

freeVars :: Traversal' Term Var
freeVars f = freeVars' (fmap Var . f)

freeVars' :: Traversal Term Term Var Term
freeVars' f = freeVarsCtx' (f . fst)

freeVarsCtx' :: Traversal Term Term (Var, [Var]) Term
freeVarsCtx' f = go []
  where
  go bound (Var x)
    | x `notElem` bound = f (x, bound)
    | otherwise = pure (Var x)
  go bound (Abs x m) = Abs x <$> go (x:bound) m
  go bound (App m n) = App <$> go bound m <*> go bound n

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
  | x /= y && allOf freeVars (y /=) n = Abs y $! substitute x n m
  -- TODO not needed to recurse substitute again, but for that it needs a distinct @Abs@ type
  | otherwise = substitute x n $! convertAlpha (newFreeVar (x : toListOf freeVars n)) (Abs y m)

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

reduce :: Term -> NonEmpty Term
reduce m = m :| unfoldr (fmap dupe . reduceStep) m

formatTerm :: Term -> String
formatTerm (Var x) = [x]
formatTerm (Abs x m) = "(\\" <> [x] <> "." <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

interpretChurchNumber :: Term -> Maybe Int
interpretChurchNumber = \m -> go $ App (App m (Var '+')) (Var '0')
  where
  go m = case last $ NE.take 1000 $ reduce m of
    Var '0' -> Just 0
    App (Var '+') n -> fmap (1+) $ go n
    _ -> Nothing

genFoo :: Q.Gen Term
genFoo =
  fmap (\m -> Abs 'f' (Abs 'x' m)) $ genBound ['f', 'x']
  where
  genBound :: [Var] -> Q.Gen Term
  genBound bound = Q.oneof [genVar, genAbs, genApp]
    where
    genVar = Var <$> Q.elements bound
    genAbs = do
      fresh <- Q.elements ['a'..'z']
      Abs fresh <$> genBound (fresh:bound)
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
          pure $ Map.alter (Just . (+1) . fromMaybe 0) result acc
    result <- try $ restoreMask calcNext
    case result of
      Right next -> loop restoreMask next
      Left UserInterrupt -> pure acc
      Left e -> throwIO e
