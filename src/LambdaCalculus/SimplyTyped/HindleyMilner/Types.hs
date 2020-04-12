{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
