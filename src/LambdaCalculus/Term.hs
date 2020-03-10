{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCalculus.Term(module LambdaCalculus.Term.Types, module LambdaCalculus.Term) where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal', ix)
import Data.Conduit ((.|), ConduitT, runConduitPure)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (Set)
import Data.Tuple.Extra (dupe)
import GHC.Generics (Generic)
import LambdaCalculus.DeBruijn (fromDeBruijn, toDeBruijn)
import LambdaCalculus.Genetic (Genetic, genChildren)
import LambdaCalculus.Term.Types
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified LambdaCalculus.DeBruijn as DeBruijn
import qualified LambdaCalculus.Genetic
import qualified Test.QuickCheck as Q

formatTerm :: Term -> String
formatTerm (Var x) = x
formatTerm (Abs x m) = "(\\" <> x <> "." <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

alphaEqv :: Term -> Term -> Bool
alphaEqv = go []
  where
  -- earlier elements in @dict@ is bound by inner abstractions
  go :: [(Var, Var)] -> Term -> Term -> Bool
  go dict (Var x) (Var y) =
    case List.lookup x dict of
      -- bound
      Just y' -> y == y'
      -- free
      Nothing -> y == x
  go dict (Abs x m) (Abs y n) =
    go ((x, y):dict) m n
  go dict (App m1 m2) (App n1 n2) =
    go dict m1 n1 && go dict m2 n2
  go _ _ _ = False

-- | @linear m@ is a non-empty list whose elements are the sub-terms of @m@
-- traversed in depth-first, pre-order.
--
-- The first element is always @m@.
--
-- The following law holds:
--
-- > length ('linear' m) == 'countTerm' m
linear :: Term -> NonEmpty Term
linear m = m :| case m of
  Var _ -> []
  Abs _ n -> NE.toList $ linear n
  App n1 n2 -> NE.toList $ linear n1 <> linear n2

-- | This list can never be empty. See 'linear'
toList :: Term -> [Term]
toList = NE.toList . linear

-- | @index i m@ traverses @m@ to find a sub-term.
--
-- @m@ is traversed in depth-first, pre-order. @i == 0@ denotes @m@ itself.
--
-- > index 0 m == Just m
-- > index 3 ('App' ('App' ('Var' \'x\') n) o) == Just n
--
-- Another equivalence:
-- > 'toList' m !! i == fromJust ('index' i m)
index :: Int -> Term -> Maybe Term
index i m = at i (toList m)

-- | Generates a 'Term' with a specified set of free variables.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- If the list is empty, @genTerm@ always generates a closed term in a form of an @'Abs' _ _@.
-- TODO Allow @App@ (consider the size)
genTerm :: Set Var -> Gen Term
genTerm fv =
  if Set.null fv
  then Q.scale (\sz -> max 1 (sz - 1)) genAbs
  else do
    -- assume that the probability of picking 'genVar' is @p@
    -- and the other two are @(1 - p) / 2@, resp.
    -- then, to have the expected value of the number of terms to be @X@,
    -- > p = (X + 2) / 3X
    size <- max 1 <$> Q.getSize
    -- @(p / 100)%@: the probability of picking 'genVar'
    let p = 10000 * (size + 2) `div` (3 * size)
        q = (10000 - p) `div` 2
    Q.frequency [(p, genVar), (q, genAbs), (q, genApp)]
  where
  -- 1 term
  genVar = Var <$> Q.elements (Set.toList fv)
  -- X + 1 terms
  genAbs = do
    -- TODO widen the range of fresh var?
    fresh <- Q.elements $ map pure ['a'..'z']
    Abs fresh <$> genTerm (Set.insert fresh fv)
  -- 2X + 1 terms
  genApp = App <$> genTerm fv <*> genTerm fv

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: Set Var -> Term -> Gen Term
genModifiedTerm fv m = do
  i <- Q.choose (0, countTerm m - 1)
  flip (ixBound i) m $ \BoundTerm{boundVars} -> genTerm $ fv <> Set.fromList boundVars

-- | A closed lambda term. This assumption allows more type instances to be defined.
newtype ClosedTerm = ClosedTerm { unClosedTerm :: Term }
  deriving (Eq, Generic, NFData, Show)

instance Arbitrary ClosedTerm where
  arbitrary = ClosedTerm <$> genTerm Set.empty

instance Ixed ClosedTerm where
  ix :: Int -> Traversal' ClosedTerm Term
  ix i f = fmap ClosedTerm . ix i f . unClosedTerm

instance Genetic ClosedTerm where
  -- TODO implement without converting to DeBruijn (for performance)?
  genChildren (parent1, parent2) = do
    (child1, child2) <- genChildren (to parent1, to parent2)
    pure (from child1, from child2)
    where
    to = DeBruijn.ClosedTerm . snd . toDeBruijn [] . unClosedTerm
    from = ClosedTerm . fromDeBruijn [] . DeBruijn.unClosedTerm

  genMutant = fmap ClosedTerm . genModifiedTerm Set.empty . unClosedTerm

type instance Index ClosedTerm = Int
type instance IxValue ClosedTerm = Term

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

encodeChurchNumber :: Natural -> Term
encodeChurchNumber n = Abs "f" $ Abs "x" $ iterate (App (Var "f")) (Var "x") !! fromIntegral n
