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
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.Set as Set

-- | A safe '(!!)'.
at :: Int -> [a] -> Maybe a
at i xs
  | i < 0 = Nothing
  | (x:_) <- drop i xs = Just x
  | otherwise = Nothing

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
-- 'preview' ('ix' i) == 'Just' ('index' i)
-- @
--
-- See also 'ixBound'.
--
-- TODO make sure that @instance At Term@ does *not* form a "reasonable instance"
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

-- TODO move LambdaCalculus.Term.index here

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
