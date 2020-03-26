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

data Var = VarCon () -- TODO
  deriving Eq

data Term
  = Var Var                -- ^ A variable (must start at @1@)
  | Abs Var Term  -- ^ An abstraction with an optional explicit type annotation
  | App Term Term          -- ^ An application
  | Const MonoType String  -- ^ A constant
  -- deriving (Eq, Generic, NFData, Show)

newtype Subst = Subst [(VarType, MonoType)]
  deriving (Monoid, Semigroup)
  -- deriving (Eq, Monoid, Semigroup, Show)
