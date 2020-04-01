{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | The types of lambda terms in the ordinary notation.
module LambdaCalculus.Term.Types where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Set (Set)
import LambdaCalculus.Utils (at)
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

-- | A variable representation.
type Var = String

-- | A lambda term without metadata.
data TermRaw
  = VarRaw Var        -- ^ A variable
  | AbsRaw Var Term   -- ^ An abstraction
  | AppRaw Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

-- | A lambda term and its metadata.
data Term = Term
  { termRaw :: TermRaw
  , freeVars :: Set Var
  -- | The number of sub-terms in 'termRaw'.
  , countTerm :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | Traverses sub-terms in depth-first, pre-order.
--
-- This is consistent with 'index':
--
-- @
-- 'Control.Lens.Fold.preview' ('ix' i) == 'Just' ('index' i)
-- @
--
-- See also 'ixBound'.
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
  , boundVars :: [Var]  -- ^ A variable bound by the innermost 'Abs' comes first
  }
  deriving (Eq, Generic, NFData, Show)

-- | @linear m@ is a non-empty list whose elements are the sub-terms of @m@
-- traversed in depth-first, pre-order.
--
-- The first element is always @m@. (TODO add a test)
--
-- The following law holds:
--
-- @
-- length ('linear' m) == 'countTerm' m
-- @
linear :: Term -> NonEmpty Term
linear m = m :| case m of
  Var _ -> []
  Abs _ n -> NE.toList $ linear n
  App n1 n2 -> NE.toList $ linear n1 <> linear n2

-- | @'toList' == NonEmpty.'NE.toList' . 'linear'@
toList :: Term -> [Term]
toList = NE.toList . linear

-- | @index i m@ traverses @m@ to find a sub-term.
--
-- @m@ is traversed in depth-first, pre-order. @i == 0@ denotes @m@ itself. (TODO add a test)
--
-- @
-- index 0 m == Just m
-- index 3 ('App' ('App' ( v'Var' x) m) n) == Just m
-- @
--
-- Another equivalence:
--
-- @
-- 'toList' m !! i == fromJust ('index' i m)
-- @
index :: Int -> Term -> Maybe Term
index i m = at i (toList m)

-- | An 'ix' for 'Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = loop []
  where
  loop :: Applicative f => [Var] -> Int -> (BoundTerm -> f Term) -> Term -> f Term
  loop bound i f m
    | i == 0 = f (BoundTerm{boundTerm = m, boundVars = bound})
    | i < 0 = pure m
    | i >= countTerm m = pure m -- for performance only; avoids unnecessary traversal especially on recursion
    | otherwise = case m of
        Var _ -> pure m
        Abs x n -> Abs x <$> loop (x:bound) (i-1) f n
        App n1 n2 -> App <$> loop bound (i-1) f n1 <*> loop bound (i-1-(countTerm n1)) f n2
