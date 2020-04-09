{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Term where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types (MonoType)

data Term
  = Var Int                -- ^ A variable (must start at @1@)
  | Abs Term               -- ^ An abstraction
  | App Term Term          -- ^ An application
  | Const MonoType String  -- ^ A constant
  deriving (Eq, Generic, NFData, Show)
