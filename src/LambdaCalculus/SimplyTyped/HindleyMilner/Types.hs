{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Types where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

type VarType = String

data MonoType
  = VarType VarType
  | ConstType String
  | MonoType :-> MonoType
  deriving (Eq, Generic, NFData, Show)
infixr 1 :->

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

data PolyType
  = Mono MonoType
  | ForAll VarType PolyType
  deriving (Eq, Generic, NFData, Show)

newtype Subst = Subst [(VarType, MonoType)]
  deriving (Eq, Show)
  deriving newtype (Monoid, NFData)

-- | @a <> b@ merges substitutions in the same way as function composition
--
-- In other words, @subst (a <> b) == subst a . subst b@  TODO test this
--
-- For efficiency, the wrapped lists are concatenated in the /reverse order/
instance Semigroup Subst where
  Subst a <> Subst b = Subst (b <> a)
