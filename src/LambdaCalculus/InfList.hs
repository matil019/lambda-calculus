{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An infinite list.
module LambdaCalculus.InfList
  ( InfList
  , toList
  , iterate
  , enumFrom
  , cons
  ) where

import Data.Semigroup (stimes, stimesIdempotent)
import Prelude hiding (enumFrom, iterate)

import qualified Prelude

-- | An infinite list.
newtype InfList a = InfList
  { toList :: [a] -- ^ Gets an ordinary list back from 'InfList'.
  }
  deriving (Eq, Functor, Show)

-- | Note that @a '<>' b ≡ a@ for this type.
instance Semigroup (InfList a) where
  a <> _ = a
  stimes = stimesIdempotent

-- | Creates an infinity list in the same fashion as 'Prelude.iterate'.
iterate :: (a -> a) -> a -> InfList a
iterate f a = InfList $ Prelude.iterate f a

-- | Creates an infinite list from an infinite range.
enumFrom :: Enum a => a -> InfList a
enumFrom a = InfList (Prelude.enumFrom a)

-- | Prepends an element to an infinite list.
cons :: a -> InfList a -> InfList a
cons a (InfList as) = InfList (a:as)
