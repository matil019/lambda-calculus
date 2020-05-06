{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Simply-typed lambda terms in De Bruijn index notation.
module LambdaCalculus.SimplyTyped.HindleyMilner.Term
  ( VarIndex, TermRaw(..), Term(.., Var, Abs, App, Const)
  , _Var, _Abs, _App, _Const
  , BoundTerm(..), ixBound
  , formatTerm
  , isClosed
  , foldVars, foldMapVars
  , linear, toList, index
  )
  where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Plated, Prism', Traversal, Traversal', ix, plate, preview, prism')
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Generics (Generic)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types (MonoType, formatMonoType)
import LambdaCalculus.Utils (isSimpleIdent)

import qualified Data.List.NonEmpty as NE
import qualified Test.QuickCheck as Q

-- | A variable index.
--
-- Must be @1@ or greater.
type VarIndex = Int

-- | A lambda term without metadata.
data TermRaw
  = VarRaw VarIndex           -- ^ A variable (must start at @1@)
  | AbsRaw Term               -- ^ An abstraction
  | AppRaw Term Term          -- ^ An application
  | ConstRaw MonoType String  -- ^ A constant
  deriving (Eq, Generic, NFData, Q.CoArbitrary, Q.Function, Show)

-- | A lambda term with typed constants in De Bruijn index notation.
--
-- Type annotations in abstractions are omitted so that they are inferred by
-- Hindley-Milner.
data Term = Term
  { termRaw :: TermRaw
  , -- | @0@ indicates this term is closed.
    freeDepth :: !Int
  , -- | The number of sub-terms in a 'Term'.
    countTerm :: !Int
  }
  deriving (Eq, Generic, NFData, Q.CoArbitrary, Q.Function, Show)

-- | A variable (must start at @1@).
--
-- A smart constructor which matches 'termRaw' and handles other fields
-- transparently.
pattern Var :: VarIndex -> Term
pattern Var x <- Term { termRaw = VarRaw x }
  where
  Var x = Term
    { termRaw = VarRaw x
    , freeDepth = x
    , countTerm = 1
    }

-- | An abstraction.
--
-- A smart constructor which matches 'termRaw' and handles other fields
-- transparently.
pattern Abs :: Term -> Term
pattern Abs m <- Term { termRaw = AbsRaw m }
  where
  Abs m = Term
    { termRaw = AbsRaw m
    , freeDepth =
        let Term{freeDepth=fm} = m
        in max 0 (fm - 1)
    , countTerm =
        let Term{countTerm=sm} = m
        in 1 + sm
    }

-- | An application.
--
-- A smart constructor which matches 'termRaw' and handles other fields
-- transparently.
pattern App :: Term -> Term -> Term
pattern App m n <- Term { termRaw = AppRaw m n }
  where
  App m n = Term
    { termRaw = AppRaw m n
    , freeDepth =
        let Term{freeDepth=fm} = m
            Term{freeDepth=fn} = n
        in max fm fn
    , countTerm =
        let Term{countTerm=sm} = m
            Term{countTerm=sn} = n
        in 1 + sm + sn
    }

-- | A typed constant.
--
-- A smart constructor which matches 'termRaw' and handles other fields
-- transparently.
pattern Const :: MonoType -> String -> Term
pattern Const t a <- Term { termRaw = ConstRaw t a }
  where
  Const t a = Term
    { termRaw = ConstRaw t a
    , freeDepth = 0
    , countTerm = 1
    }

{-# COMPLETE Var, Abs, App, Const #-}

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

instance Plated Term where
  plate _ m@(Var _)     = pure m
  plate f (Abs m)       = Abs <$> f m
  plate f (App m n)     = App <$> f m <*> f n
  plate _ m@(Const _ _) = pure m

-- | A prism that targets 'Var'.
_Var :: Prism' Term Int
_Var = prism' Var $ \case
  Var x -> Just x
  _ -> Nothing

-- | A prism that targets 'Abs'.
_Abs :: Prism' Term Term
_Abs = prism' Abs $ \case
  Abs m -> Just m
  _ -> Nothing

-- | A prism that targets 'App'.
_App :: Prism' Term (Term, Term)
_App = prism' (uncurry App) $ \case
  App m n -> Just (m, n)
  _ -> Nothing

-- | A prism that targets 'Const'.
_Const :: Prism' Term (MonoType, String)
_Const = prism' (uncurry Const) $ \case
  Const t a -> Just (t, a)
  _ -> Nothing

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
formatTerm (Const t a) = "(" <> a' <> " :: " <> formatMonoType t <> ")"
  where
  a' = if isSimpleIdent a then a else show a
formatTerm (Abs m) = "\\ " <> formatTerm m
formatTerm (App m n)
   = (case m of Abs _ -> paren; _ -> id;) (formatTerm m)
  <> " "
  <> (case n of Abs _ -> paren; App _ _ -> paren; _ -> id) (formatTerm n)
  where
  paren s = "(" <> s <> ")"

-- | Is this 'Term' closed (i.e. has no free variables)?
--
-- Constants are not considered as free variables.
isClosed :: Term -> Bool
isClosed = (== 0) . freeDepth

-- | Folds over 'Var's in a term.
foldVars
  :: Monoid m
  => (Int -> VarIndex -> m)  -- ^ args: the number of 'Abs' containing a 'Var' and the variable index.
  -> Term
  -> m
foldVars = foldMapVars mempty id

-- | Folds over 'Var's in a term with a custom conversion to a monoid.
foldMapVars
  :: Semigroup m
  => m                       -- ^ an "mempty"; used for 'Const's
  -> (a -> m)                -- ^ a conversion to a monoid
  -> (Int -> VarIndex -> a)  -- ^ args: the number of 'Abs' containing a 'Var' and the variable index.
  -> Term
  -> m
foldMapVars empty am f = go 0
  where
  go bound (Var x) = am $ f bound x
  go bound (Abs m) = go (bound+1) m
  go bound (App m n) = go bound m <> go bound n
  go _ (Const _ _) = empty

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
