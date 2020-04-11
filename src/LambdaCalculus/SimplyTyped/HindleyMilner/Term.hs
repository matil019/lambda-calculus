{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Term where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix, preview)
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Generics (Generic)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types (MonoType)

import qualified Data.List.NonEmpty as NE

-- | A lambda term with typed constants in De Bruijn index notation.
data Term
  = Var Int                -- ^ A variable (must start at @1@)
  | Abs Term               -- ^ An abstraction
  | App Term Term          -- ^ An application
  | Const MonoType String  -- ^ A constant
  deriving (Eq, Generic, NFData, Show)

-- TODO copy-pasted from LambdaCalculus.DeBruijn

-- | Traverses sub-terms in depth-first, pre-order.
--
-- This is consistent with 'index':
--
-- @
-- 'preview' ('ix' i) == 'Just' ('index' i)
-- @
--
-- See also 'ixBound'.
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f = ixBound i (f . boundTerm)

type instance Index Term = Int
type instance IxValue Term = Term

-- | A term with additional info about its enclosing term.
data BoundTerm = BoundTerm
  { boundTerm :: Term
  -- | A variable @x@ is bound if @boundTerm == Var x@ and @x <= boundNum@.
  , boundNum :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | An 'ix' for 'Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = loop 0
  where
  loop :: Applicative f => Int -> Int -> (BoundTerm -> f Term) -> Term -> f Term
  loop boundNum i f m
    | i == 0 = f (BoundTerm{boundTerm = m, boundNum})
    | i < 0 = pure m
    | otherwise = case m of
        Var _ -> pure m
        Const _ _ -> pure m
        Abs n -> Abs <$> loop (boundNum + 1) (i-1) f n
        App n1 n2
          | i' < cn1  -> flip App n2 <$> loop boundNum i' f n1
          | otherwise -> App n1 <$> loop boundNum (i'-cn1) f n2
          where
          i' = i - 1
          cn1 = countTerm n1

-- | Formats a 'Term' into a human-readable string.
formatTerm :: Term -> String
formatTerm (Var x) = show x
formatTerm (Const _ a) = a
formatTerm (Abs m) = "\\ " <> formatTerm m
formatTerm (App m n)
   = (case m of Abs _ -> paren; _ -> id;) (formatTerm m)
  <> " "
  <> (case n of Abs _ -> paren; App _ _ -> paren; _ -> id) (formatTerm n)
  where
  paren s = "(" <> s <> ")"

-- | Counts a number of sub-terms in a 'Term'.
countTerm :: Term -> Int
countTerm (Var _) = 1
countTerm (Const _ _) = 1
countTerm (Abs m) = 1 + countTerm m
countTerm (App m n) = 1 + countTerm m + countTerm n

-- | Is this 'Term' closed (i.e. has no free variables)?
--
-- Constants are not considered as free variables.
isClosed :: Term -> Bool
isClosed = go 0
  where
  go !bound (Var x) = x <= bound
  go !bound (Abs m) = go (bound+1) m
  go !bound (App m n) = go bound m && go bound n
  go _ (Const _ _) = True

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
linear m = m :| case m of
  Var _ -> []
  Const _ _ -> []
  Abs n -> NE.toList $ linear n
  App n1 n2 -> NE.toList $ linear n1 <> linear n2

-- | @'toList' == NonEmpty.'NE.toList' . 'linear'@
toList :: Term -> [Term]
toList = NE.toList . linear

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
