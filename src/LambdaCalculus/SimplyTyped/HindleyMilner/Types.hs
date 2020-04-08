{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Types where

type VarType = String

data MonoType
  = VarType VarType
  | ConstType String
  | MonoType :-> MonoType
  deriving (Eq, Show)
infixr 1 :->

data PolyType
  = Mono MonoType
  | ForAll VarType PolyType

data Term
  = Var Int                -- ^ A variable (must start at @1@)
  | Abs Term               -- ^ An abstraction
  | App Term Term          -- ^ An application
  | Const MonoType String  -- ^ A constant
  -- deriving (Eq, Generic, NFData, Show)

newtype Subst = Subst [(VarType, MonoType)]
  deriving (Monoid, Semigroup)
  -- deriving (Eq, Monoid, Semigroup, Show)
