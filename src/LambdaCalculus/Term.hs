{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCalculus.Term where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (Set)
import GHC.Generics (Generic)
import LambdaCalculus.Genetic (ChooseIxed, chooseIx, genModified)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Test.QuickCheck as Q

type Var = String

data TermRaw
  = VarRaw Var        -- ^ A variable
  | AbsRaw Var Term   -- ^ An abstraction
  | AppRaw Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

-- | A lambda termRaw and its metadata.
data Term = Term
  { termRaw :: TermRaw
  , freeVars :: Set Var
  , countTerm :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- TODO make sure that @instance At Term@ does *not* form a "reasonable instance"
-- TODO make sure that this instance is consistent with 'linear'; or rather, implement it with this?
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f = ixBound i (f . boundTerm)

type instance Index Term = Int
type instance IxValue Term = Term

-- | A smart constructor which matches 'termRaw' and handles other fields transparently.
pattern Var :: Var -> Term
pattern Var x <- Term { termRaw = VarRaw x }
  where
  Var x = Term
    { termRaw = VarRaw x
    , freeVars = Set.singleton x
    , countTerm = 1
    }

-- | A smart constructor which matches 'termRaw' and handles other fields transparently.
pattern Abs :: Var -> Term -> Term
pattern Abs x m <- Term { termRaw = AbsRaw x m }
  where
  Abs x m = Term
    { termRaw = AbsRaw x m
    , freeVars =
        let Term{freeVars=fm} = m
        in Set.delete x fm
    , countTerm =
        let Term{countTerm=sm} = m
        in 1 + sm
    }

-- | A smart constructor which matches 'termRaw' and handles other fields transparently.
pattern App :: Term -> Term -> Term
pattern App m n <- Term { termRaw = AppRaw m n }
  where
  App m n = Term
    { termRaw = AppRaw m n
    , freeVars =
        let Term{freeVars=fm} = m
            Term{freeVars=fn} = n
        in Set.union fm fn
    , countTerm =
        let Term{countTerm=sm} = m
            Term{countTerm=sn} = n
        in 1 + sm + sn
    }

{-# COMPLETE Var, Abs, App #-}

-- | A term with additional info about its enclosing term.
data BoundTerm = BoundTerm
  { boundTerm :: Term
  , boundVars :: Set Var
  }
  deriving (Eq, Generic, NFData, Show)

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
  where
  at j xs
    | j < 0 = Nothing
    | (x:_) <- drop j xs = Just x
    | otherwise = Nothing

-- | 'ix @Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = loop Set.empty
  where
  loop :: Applicative f => Set Var -> Int -> (BoundTerm -> f Term) -> Term -> f Term
  loop bound i f m
    | i == 0 = f (BoundTerm{boundTerm = m, boundVars = bound})
    | i < 0 = pure m
    | i >= countTerm m = pure m -- for performance only; avoids unnecessary traversal especially on recursion
    | otherwise = case m of
        Var _ -> pure m
        Abs x n -> Abs x <$> loop (Set.insert x bound) (i-1) f n
        App n1 n2 -> App <$> loop bound (i-1) f n1 <*> loop bound (i-1-(countTerm n1)) f n2

-- | Generates a 'Term' with a specified set of free variables.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- If the list is empty, @genTerm@ always generates a closed term i.e. an 'Abs'.
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
  flip (ixBound i) m $ \BoundTerm{boundVars} -> genTerm $ fv <> boundVars

-- | A closed lambda term. This assumption allows more type instances to be defined.
newtype ClosedTerm = ClosedTerm { unClosedTerm :: Term }
  deriving (Eq, Generic, NFData, Show)

instance Arbitrary ClosedTerm where
  arbitrary = ClosedTerm <$> genTerm Set.empty

instance Ixed ClosedTerm where
  ix :: Int -> Traversal' ClosedTerm Term
  ix i f = fmap ClosedTerm . ix i f . unClosedTerm

instance ChooseIxed ClosedTerm where
  chooseIx (ClosedTerm m) = Q.choose (0, countTerm m - 1)
  genModified = fmap ClosedTerm . genModifiedTerm Set.empty . unClosedTerm

type instance Index ClosedTerm = Int
type instance IxValue ClosedTerm = Term
