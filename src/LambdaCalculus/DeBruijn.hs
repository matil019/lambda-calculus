{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Lambda terms in De Bruijn index notation.
module LambdaCalculus.DeBruijn
  ( -- * Terms
    Term
    ( ..
    , LambdaCalculus.DeBruijn.Var
    , LambdaCalculus.DeBruijn.Abs
    , LambdaCalculus.DeBruijn.App
    )
  , -- ** Conversions with other notations
    toDeBruijn, fromDeBruijn
  , -- ** Basic operations
    formatTerm, countTerm, isClosed
  , -- ** Accessors and lists
    linear, toList, index, ixBound, BoundTerm(..)
  , -- ** Closed terms
    ClosedTerm(..), NoConstants, unClosedTerm, toClosedTermUnchecked
  , -- ** Generating terms
    genTerm, genModifiedTerm
  , -- ** Reductions
    substitute, reduceBeta, reduceStep, reduceSteps
  , -- ** Church encodings
    encodeChurchNumber, genChurchNumber, interpretChurchNumber, interpretChurchPair
  ) where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix, preview)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Coerce (coerce)
import Data.Conduit ((.|), ConduitT)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty)
import LambdaCalculus.Genetic (Genetic, genCrossover, genMutant)
import LambdaCalculus.InfList (InfList)
import LambdaCalculus.Utils (FiniteList(FiniteList), unFiniteList)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified LambdaCalculus.SimplyTyped.DeBruijn as Typed
import qualified LambdaCalculus.Term.Types as Term
import qualified Test.QuickCheck as Q

-- | A lambda term in De Bruijn index notation.
--
-- This is implemented as a newtype of 'Typed.Term' because untyped terms are
-- equivalent with typed terms in which constants never appear.
newtype Term = Untyped
  { -- | This term should not contain 'Typed.Const'!
    getTyped :: Typed.Term
  }
  deriving stock (Generic)
  deriving newtype (Eq, NFData, Show)

-- | Traverses sub-terms in depth-first, pre-order.
--
-- This is consistent with 'index':
--
-- @
-- 'preview' ('ix' i) ≡ 'index' i
-- @
--
-- See also 'ixBound'.
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f = ixBound i (f . boundTerm)

type instance Index Term = Int
type instance IxValue Term = Term

-- | A variable (must start at @1@)
pattern Var :: Int -> Term
pattern Var x = Untyped (Typed.Var x)

-- | An abstraction
pattern Abs :: Term -> Term
pattern Abs m <- Untyped ((\x -> case x of Typed.Abs m' -> Just (Untyped m'); _ -> Nothing) -> Just m)
  where Abs (Untyped m) = Untyped (Typed.Abs m)

-- | An application
pattern App :: Term -> Term -> Term
pattern App m n <- Untyped ((\x -> case x of Typed.App m' n' -> Just (Untyped m', Untyped n'); _ -> Nothing) -> Just (m, n))
  where App (Untyped m) (Untyped n) = Untyped (Typed.App m n)

{-# COMPLETE Var, Abs, App #-}

-- | A term with additional info about its enclosing term.
data BoundTerm = BoundTerm
  { boundTerm :: Term
  -- | A variable @x@ is bound if @boundTerm == Var x@ and @x <= boundNum@.
  , boundNum :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | Converts a lambda 'Term.Term' to De Bruijn index 'Term'.
--
-- The first value in the returned tuple is an ordered set of free variables
-- which the second refers.
toDeBruijn
  :: FiniteList Term.Var  -- ^ Known free variables; the return value is guaranteed to begin with this list
  -> Term.Term            -- ^ The term to convert
  -> (FiniteList Term.Var, Term)
toDeBruijn = \(FiniteList free) -> (\(a, b) -> (FiniteList b, a)) . flip runState free . go []
  where
  go :: [Term.Var] -> Term.Term -> State [Term.Var] Term
  go bound (Term.Var x)
    | Just idx <- elemIndex x bound = pure (Var (idx + 1))
    | otherwise = state $ \free ->
        case elemIndex x free of
          Just idx -> (Var (length bound + idx + 1), free)
          Nothing  -> (Var (length bound + length free + 1), free <> [x])
  go bound (Term.Abs x m) = Abs <$> go (x:bound) m
  go bound (Term.App m n) = do
    m' <- go bound m
    n' <- go bound n
    pure $ App m' n'

-- | Converts a De Bruijn index 'Term' into the ordinary notation.
--
-- The list must be long enough to have all the free variables the 'Term' refers.
--
-- The return value of 'toDeBruijn' can always be applied to 'fromDeBruijn'.
fromDeBruijn :: FiniteList Term.Var -> Term -> Term.Term
fromDeBruijn (unFiniteList -> free) = go infinitevars []
  where
  -- @go unused bound m@ recursively converts @m@ from DeBruijn notation to the ordinary one.
  --
  -- @unused@ is an infinite list of to-be-bound variables.
  -- @bound@ is a list of bound variables. Its first element is bound by the innermost abstraction.
  go _ bound (Var n) = case drop (n-1) bound of
    (x:_) -> Term.Var x
    [] -> Term.Var (free !! (n - length bound - 1))
  go [] _ (Abs _) = error "fromDeBruijn: the impossible happened!"
  go (fresh:other) bound (Abs m) =
    Term.Abs fresh (go other (fresh:bound) m)
  go unused bound (App m n) =
    Term.App (go unused bound m) (go unused bound n)

  -- infinite list of strings, "a" : "b" : ... : "z" : "aa" : "ab" : ...
  infinitevars = filter (`notElem` free) $ concat $ iterate (\ss -> [ c:s | c <- ['a'..'z'], s <- ss ]) [ [c] | c <- ['a'..'z'] ]

-- | Formats a 'Term' into a human-readable string.
formatTerm :: Term -> String
formatTerm = coerce Typed.formatTerm

-- | Counts a number of sub-terms in a 'Term'.
countTerm :: Term -> Int
countTerm = coerce Typed.countTerm

-- | Is this 'Term' closed (i.e. has no free variables)?
isClosed :: Term -> Bool
isClosed = coerce Typed.isClosed

-- | @linear m@ is a non-empty list whose elements are the sub-terms of @m@
-- traversed in depth-first, pre-order.
--
-- The first element is always @m@.
--
-- The following law holds:
--
-- @
-- length ('linear' m) == 'countTerm' m
-- @
linear :: Term -> NonEmpty Term
linear = coerce Typed.linear

-- | @'toList' == NonEmpty.'NE.toList' . 'linear'@
toList :: Term -> [Term]
toList = coerce Typed.toList

-- | @index i m@ traverses @m@ to find a sub-term.
--
-- @m@ is traversed in depth-first, pre-order. @i == 0@ denotes @m@ itself.
--
-- @
-- index 0 m == Just m
-- index 3 ('App' ('App' ('Var' x) m) n) == Just m
-- @
--
-- Another equivalence:
--
-- @
-- 'toList' m !! i == fromJust ('index' i m)
-- @
index :: Int -> Term -> Maybe Term
index i m = preview (ix i) m

-- | An 'ix' for 'Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = \i f -> fmap coerce . Typed.ixBound i (fmap coerce . f . untypedBoundTerm) . coerce
  where
  untypedBoundTerm (Typed.BoundTerm x y) = BoundTerm (coerce x) y

-- | Generates a 'Term' with a specified number of free variables.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- @genTerm 0@ always generates a closed term in a form of an @('Abs' _)@.
genTerm :: Int -> Gen Term
genTerm = coerce $ Typed.genTerm []

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: Int -> Term -> Gen Term
genModifiedTerm = coerce $ Typed.genModifiedTerm []

-- | A null data type for use with 'Typed.TypeSet'
data NoConstants

-- | @'Typed.candidateConsts' _ = []@ because untyped terms are equivalent with typed terms
-- in which constants never appear.
instance Typed.TypeSet NoConstants where
  candidateConsts _ = []

-- | A closed lambda term. This assumption allows more type instances to be defined.
newtype ClosedTerm = ClosedTerm (Typed.ClosedTerm NoConstants)
  deriving stock (Generic, Show)
  deriving newtype (Eq, NFData)

instance Arbitrary ClosedTerm where
  arbitrary = fmap ClosedTerm $ coerce $ Typed.genClosedTerm []

instance Ixed ClosedTerm where
  ix :: Int -> Traversal' ClosedTerm Term
  ix i f = fmap toClosedTermUnchecked . ix i f . unClosedTerm

type instance Index ClosedTerm = Int
type instance IxValue ClosedTerm = Term

instance Genetic ClosedTerm where
  genCrossover (ClosedTerm parent1, ClosedTerm parent2) = coerce (genCrossover (parent1, parent2))
  genMutant (ClosedTerm parent) = coerce (genMutant parent)

-- | Converts back from 'ClosedTerm' to 'Term'.
unClosedTerm :: ClosedTerm -> Term
unClosedTerm = coerce Typed.unClosedTerm

-- | @toClosedTermUnchecked m@ converts a 'Term' into a 'ClosedTerm'.
--
-- This doesn't check that @m@ is really closed.
toClosedTermUnchecked :: Term -> ClosedTerm
toClosedTermUnchecked = coerce Typed.ClosedTerm

-- | Performs a substitution.
--
-- You would like to use 'reduceBeta' instead of using this directly.
substitute :: InfList Term -> Term -> Term
substitute = coerce Typed.substitute

-- | Performs a beta-reduction.
reduceBeta :: Term -> Term
reduceBeta = coerce Typed.reduceBeta

-- | @reduceStep m@ tries to reduce a beta-redex one step.
--
-- If @m@ can't be reduced any more, returns @Nothing@.
reduceStep :: Term -> Maybe Term
reduceStep = coerce Typed.reduceStep

-- | Repeatedly reduces ('reduceStep') a term and yields each step.
reduceSteps :: Monad m => Term -> ConduitT i Term m ()
reduceSteps m = coerce Typed.reduceSteps m .| C.map coerce

-- | Interprets a lambda term as a Church numeral. The term must be fully reduced. (TODO add a newtype)
interpretChurchNumber :: Term -> Maybe Natural
interpretChurchNumber = coerce Typed.interpretChurchNumber

{-# DEPRECATED genChurchNumber "Use encodeChurchNumber" #-}
-- |
genChurchNumber :: Gen Term
genChurchNumber = Abs . Abs <$> genTerm 2

-- | Encodes a natural number into a Church numeral.
encodeChurchNumber :: Natural -> Term
encodeChurchNumber = coerce Typed.encodeChurchNumber

-- | Interprets a lambda term as a Church pair.
--
-- The argument can be a redex. Always returns redexes.
interpretChurchPair :: Term -> (Term, Term)
interpretChurchPair = coerce Typed.interpretChurchPair
