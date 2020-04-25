{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Types which represent types in Hindley-Milner type system.
module LambdaCalculus.SimplyTyped.HindleyMilner.Types where

import Control.DeepSeq (NFData)
import Control.Lens (Lens', Plated, plate)
import GHC.Generics (Generic)

-- | A type variable representation.
type VarType = String

-- | A mono type.
data MonoType
  = VarType VarType        -- ^ A type variable
  | ConstType String       -- ^ A constant type
  | MonoType :-> MonoType  -- ^ A function type
  deriving (Eq, Generic, NFData, Show)
infixr 1 :->

instance Plated MonoType where
  plate _ t@(VarType _)   = pure t
  plate _ t@(ConstType _) = pure t
  plate f (t :-> t')      = (:->) <$> f t <*> f t'

-- | Formats a 'MonoType' into a human-readable string.
formatMonoType :: MonoType -> String
formatMonoType (VarType x) = x
formatMonoType (ConstType c) = c
formatMonoType (t :-> u) =
  decorate (formatMonoType t) <> " -> " <> formatMonoType u
  where
  decorate = case t of
    VarType   _ -> id
    ConstType _ -> id
    _ :-> _     -> \s -> "(" <> s <> ")"

-- | A poly type.
data PolyType
  = Mono MonoType            -- ^ A mono type with no bound type variables.
  | ForAll VarType PolyType  -- ^ A variable binder and a type.
  deriving (Eq, Generic, NFData, Show)

-- | A lens to the outermost 'MonoType' in a 'PolyType'.
--
-- Intended to be combined with 'Control.Lens.universeOn', etc. to make use of
-- the @instance 'Plated' 'MonoType'@ with a 'PolyType'.
topMono :: Lens' PolyType MonoType
topMono f (Mono t) = Mono <$> f t
topMono f (ForAll a s) = ForAll a <$> topMono f s

-- | A lens to the bound type variables introduced by 'ForAll's.
--
-- > view boundVars (Mono t) = []
--
-- > view boundVars (ForAll a (ForAll b (Mono t))) = [a, b]
boundVars :: Lens' PolyType [VarType]
boundVars f s = rebind (Mono mono) <$> f bounds
  where
  (bounds, mono) = go [] s
  go acc (Mono t) = (reverse acc, t)
  go acc (ForAll a s') = go (a:acc) s'
  rebind = foldr ForAll
