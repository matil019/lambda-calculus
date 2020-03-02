{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Term where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal', ix)
import Data.List (delete, union)
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Generics (Generic)

import qualified Data.List.NonEmpty as NE

type Var = Char

data TermRaw
  = VarRaw Var        -- ^ A variable
  | AbsRaw Var Term   -- ^ An abstraction
  | AppRaw Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

-- | A lambda termRaw and its metadata.
data Term = Term
  { termRaw :: TermRaw
  , freeVars :: [Var]
  , countTerm :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | A smart constructor which matches 'termRaw' and handles other fields transparently.
pattern Var :: Var -> Term
pattern Var x <- Term { termRaw = VarRaw x }
  where
  Var x = Term
    { termRaw = VarRaw x
    , freeVars = [x]
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
        in delete x fm
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
        in union fm fn
    , countTerm =
        let Term{countTerm=sm} = m
            Term{countTerm=sn} = n
        in 1 + sm + sn
    }

{-# COMPLETE Var, Abs, App #-}

-- | @linear m@ is a non-empty list whose elements are the sub-terms of @m@
-- traversed in depth-first, pre-order.
--
-- The first element is always @m@.
--
-- TODO make sure that @length (linear m) == countTerm m@
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

-- TODO make sure that @instance At Term@ does *not* form a "reasonable instance"
-- TODO make sure that this instance is consistent with 'linear'; or rather, implement it with this?
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f m
    | i == 0 = f m
    | i < 0 = pure m
    | i >= countTerm m = pure m -- for performance only; avoids unnecessary traversal especially on recursion
    | otherwise = case m of
        Var _ -> pure m
        Abs x n -> Abs x <$> ix (i-1) f n
        App n1 n2 -> App <$> ix (i-1) f n1 <*> ix (i-1-(countTerm n1)) f n2

type instance Index Term = Int
type instance IxValue Term = Term
