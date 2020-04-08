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

data PolyType
  = Mono MonoType
  | ForAll VarType PolyType
  deriving (Eq, Generic, NFData, Show)

data Term
  = Var Int                -- ^ A variable (must start at @1@)
  | Abs Term               -- ^ An abstraction
  | App Term Term          -- ^ An application
  | Const MonoType String  -- ^ A constant
  deriving (Eq, Generic, NFData, Show)

newtype Subst = Subst [(VarType, MonoType)]
  deriving (Eq, Show)
  deriving newtype (Monoid, NFData, Semigroup)
