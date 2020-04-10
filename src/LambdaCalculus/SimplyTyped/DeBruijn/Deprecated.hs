{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCalculus.SimplyTyped.DeBruijn.Deprecated {-# WARNING "To be replaced with HM" #-} where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix)
import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Generics (Generic)
import LambdaCalculus.Utils (at)

import qualified Data.List.NonEmpty as NE

type BaseType = String

data Type
  = BaseType BaseType
  | FuncType Type Type
  deriving (Eq, Generic, NFData, Show)

-- | A synonym for 'FuncType'.
pattern (:->) :: Type -> Type -> Type
pattern t :-> u = FuncType t u
infixr 1 :->

{-# COMPLETE BaseType, (:->) #-}

formatType :: Type -> String
formatType (BaseType bt) = bt
formatType (FuncType t u) =
  let decorate = case t of
        FuncType _ _ -> \s -> "(" <> s <> ")"
        BaseType _ -> id
  in decorate (formatType t) <> " -> " <> formatType u

data Term
  = Var Int                -- ^ A variable (starts at @1@)
  | Abs (Maybe Type) Term  -- ^ An abstraction with an optional explicit type annotation
  | App Term Term          -- ^ An application
  | Const Type String      -- ^ A constant
  deriving (Eq, Generic, NFData, Show)

-- | Traverses sub-terms in depth-first, pre-order.
--
-- This is consistent with 'index':
-- > preview (ix i) == Just (index i)
--
-- See also 'ixBound'.
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f = ixBound i (f . boundTerm)

type instance Index Term = Int
type instance IxValue Term = Term

formatTerm :: Term -> String
formatTerm (Var x) = show x
formatTerm (Const _ x) = x
formatTerm (Abs t m) =
  let ts = maybe "" (\s -> "[" <> formatType s <> "]") t
  in "(\\" <> ts <> " " <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

formatTermWithType :: [Type] -> Term -> String
formatTermWithType ctx m =
  formatTerm m <> " " <> maybe "<ill-typed>" (\t -> "[" <> formatType t <> "]") (typeOf ctx m)

countTerm :: Term -> Int
countTerm (Const _ _) = 1
countTerm (Var _) = 1
countTerm (Abs _ m) = 1 + countTerm m
countTerm (App m n) = 1 + countTerm m + countTerm n

-- | Is this 'Term' closed (i.e. has no free variables)?
--
-- Constants are not considered as free variables.
isClosed :: Term -> Bool
isClosed = go 0
  where
  go !_ (Const _ _) = True
  go !bound (Var x) = x <= bound
  go !bound (Abs _ m) = go (bound+1) m
  go !bound (App m n) = go bound m && go bound n

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
  Const _ _ -> []
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
-- TODO add test
index :: Int -> Term -> Maybe Term
index i m = at i (toList m)

-- | A term with additional info about its enclosing term.
data BoundTerm = BoundTerm
  { boundTerm :: Term
  -- | @boundTerm == Var x@ is bound if @x <= boundNum@.
  , boundNum :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | 'ix @Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = loop 0
  where
  loop :: Applicative f => Int -> Int -> (BoundTerm -> f Term) -> Term -> f Term
  loop boundNum i f m
    | i == 0 = f (BoundTerm{boundTerm = m, boundNum})
    | i < 0 = pure m
    | i >= countTerm m = pure m
    | otherwise = case m of
        Const _ _ -> pure m
        Var _ -> pure m
        Abs t n -> Abs t <$> loop (boundNum + 1) (i-1) f n
        App n1 n2 -> App <$> loop boundNum (i-1) f n1 <*> loop boundNum (i-1-(countTerm n1)) f n2

-- | A simple type checker which requires that all 'Abs' have type annotations.
-- TODO write tests
typeOf
  :: [Type] -- ^ typing context: @type of (Var x) == ctx !! (x-1)@
  -> Term
  -> Maybe Type
typeOf ctx (Var x) = at (x-1) ctx
typeOf _ (Const t _) = pure t
typeOf ctx (Abs mt m) = mt >>= \t -> typeOf (t:ctx) m >>= pure . FuncType t
typeOf ctx (App m n) = do
  FuncType s t <- typeOf ctx m
  u <- typeOf ctx n
  guard $ s == u
  pure t

-- The algorithm uses this made-up rule. TODO prove this
--
-- > Γ, x:s |- e => t
-- > ------------------------
-- > Γ |- \(x:s). e => s -> t
--
-- Because we don't have "an annotated term". With our datatypes only the
-- parameter of 'Abs' can be annotated, but not the whole term.
bidirectionalTypeSynth
  :: [Type] -- ^ typing context: @type of (Var x) == ctx !! (x-1)@
  -> Term
  -> Maybe Type
bidirectionalTypeSynth ctx (Var x) = at (x-1) ctx
bidirectionalTypeSynth _ (Const t _) = pure t
bidirectionalTypeSynth ctx (Abs mt m) = mt >>= \t -> bidirectionalTypeSynth (t:ctx) m >>= pure . (t :->)
bidirectionalTypeSynth ctx (App m n) = do
  s :-> t <- bidirectionalTypeSynth ctx m
  guard $ bidirectionalTypeCheck ctx s n
  pure t

bidirectionalTypeCheck
  :: [Type] -- ^ typing context: @type of (Var x) == ctx !! (x-1)@
  -> Type   -- ^ type to check against
  -> Term
  -> Bool
bidirectionalTypeCheck ctx t m | Just t == bidirectionalTypeSynth ctx m = True
bidirectionalTypeCheck ctx (s :-> t) (Abs _ m) = bidirectionalTypeCheck (s:ctx) t m
bidirectionalTypeCheck _ _ _ = False
