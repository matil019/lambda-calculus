{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- | Types which represent types in Hindley-Milner type system.
module LambdaCalculus.SimplyTyped.HindleyMilner.Types where

import Control.DeepSeq (NFData)
import Control.Lens (Lens', Plated, Prism', plate, prism')
import GHC.Generics (Generic)
import LambdaCalculus.Utils (isSimpleIdent)

import qualified Test.QuickCheck as Q

-- | A type variable representation.
type VarType = String

-- | A mono type.
data MonoType
  = VarType VarType        -- ^ A type variable
  | ConstType String       -- ^ A constant type
  | MonoType :-> MonoType  -- ^ A function type
  deriving (Eq, Generic, NFData, Q.CoArbitrary, Q.Function, Show)
infixr 1 :->

instance Plated MonoType where
  plate _ t@(VarType _)   = pure t
  plate _ t@(ConstType _) = pure t
  plate f (t :-> t')      = (:->) <$> f t <*> f t'

-- | A prism that targets the value of v'VarType'.
_VarType :: Prism' MonoType VarType
_VarType = prism' VarType $ \case
  VarType x -> Just x
  _ -> Nothing

-- | A prism that targets the value of 'ConstType'.
_ConstType :: Prism' MonoType String
_ConstType = prism' ConstType $ \case
  ConstType x -> Just x
  _ -> Nothing

-- | A prism that targets the operands of '(:->)'.
_FuncType :: Prism' MonoType (MonoType, MonoType)
_FuncType = prism' (uncurry (:->)) $ \case
  t :-> t' -> Just (t, t')
  _ -> Nothing

-- | Formats a 'MonoType' into a human-readable string.
formatMonoType :: MonoType -> String
formatMonoType (VarType x)   = if isSimpleIdent x then x else show x
formatMonoType (ConstType c) = if isSimpleIdent c then c else show c
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

-- | A prism that targets the value of 'Mono'.
--
-- See also 'topMono'.
_Mono :: Prism' PolyType MonoType
_Mono = prism' Mono $ \case
  Mono t -> Just t
  _ -> Nothing

-- | A prism that targets the value of 'ForAll'.
_ForAll :: Prism' PolyType (VarType, PolyType)
_ForAll = prism' (uncurry ForAll) $ \case
  ForAll a s -> Just (a, s)
  _ -> Nothing

-- | A lens to the outermost 'MonoType' in a 'PolyType'.
--
-- Intended to be combined with 'Control.Lens.universeOn', etc. to make use of
-- the @instance 'Plated' 'MonoType'@ with a 'PolyType'.
--
-- This is different from '_Mono'. 'topMono' targets a 'Mono' possibly wrapped
-- inside 'ForAll'(s), whereas '_Mono' targets the outermost 'Mono' constructor.
-- So
--
-- > preview topMono (Mono t) ≡ Just t
-- > preview _Mono   (Mono t) ≡ Just t
--
-- > preview topMono (ForAll a (Mono t)) ≡ Just t
-- > preview _Mono   (ForAll a (Mono t)) ≡ Nothing
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
