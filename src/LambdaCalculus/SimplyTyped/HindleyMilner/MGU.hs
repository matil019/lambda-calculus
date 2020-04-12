-- | The most general unifier.
module LambdaCalculus.SimplyTyped.HindleyMilner.MGU where

import Control.Monad (mzero)
import Data.List (nub)
import Data.Tuple (swap)
import Data.Tuple.Extra (both, second)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types -- TODO no all-in import

-- | The most general unifier.
mgu :: MonoType -> MonoType -> Maybe [(VarType, MonoType)]
mgu = \t t' -> go $ S [(t, t')] []
  where
  go (S [] acc) = pure acc
  go (S (gh:gt) acc) = case gh of
    (t, t') | t == t' -> go $ S gt acc
    (VarType x, t')
      | x `elem` vars t' -> mzero  -- infinite type (a ~ t a), fail
      | otherwise -> go $ substS x t' $ S gt ((x,t'):acc)
    tt@(_, VarType _) -> go $ S (swap tt:gt) acc
    (t1 :-> t2, t3 :-> t4) -> go $ S ((t1,t3):(t2,t4):gt) acc
    -- comparisons of ConstType are covered above
    _ -> mzero  -- conflict, fail

-- | The internal state used in 'mgu'.
--
-- This is a pair of to-be-unified types and found substitutions.
data MGUState = S [(MonoType, MonoType)] [(VarType, MonoType)]

vars :: MonoType -> [VarType]
vars = nub . go
  where
  go (VarType a) = [a]
  go (ConstType _) = []
  go (t :-> t') = go t <> go t'

-- | @subst x m t@ substitutes all occurences of @x@ in @t@ by @m@.
subst :: VarType -> MonoType -> MonoType -> MonoType
subst x m = go
  where
  go t@(VarType y)
    | x == y = m
    | otherwise = t
  go t@(ConstType _) = t
  go (t :-> t') = go t :-> go t'

substS :: VarType -> MonoType -> MGUState -> MGUState
substS x m (S tbd acc) =
  S (map (both   (subst x m)) tbd)
    (map (second (subst x m)) acc)
