{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An infinite list.
module LambdaCalculus.InfList
  ( InfList
  , toList
  , fromListUnchecked
  , iterate
  , enumFrom
  , cons
  , drop
  ) where

import Data.Semigroup (stimes, stimesIdempotent)
import Prelude hiding (drop, enumFrom, iterate)

import qualified Prelude

-- | An infinite list.
newtype InfList a = InfList
  { toList :: [a] -- ^ Gets an ordinary list back from an 'InfList'.
  }
  deriving (Eq, Functor, Show)

-- | Note that @a '<>' b ≡ a@ for this type.
instance Semigroup (InfList a) where
  a <> _ = a
  stimes = stimesIdempotent

-- | Wraps an ordinary list into an `InfList` without checking whether the list
-- is actually infinite.
fromListUnchecked :: [a] -> InfList a
fromListUnchecked = InfList
{-# INLINE fromListUnchecked #-}

-- | Creates an infinity list in the same fashion as 'Prelude.iterate'.
iterate :: (a -> a) -> a -> InfList a
iterate f a = InfList $ Prelude.iterate f a

-- | Creates an infinite list from an infinite range.
enumFrom :: Enum a => a -> InfList a
enumFrom a = InfList (Prelude.enumFrom a)

-- | Prepends an element to an infinite list.
cons :: a -> InfList a -> InfList a
cons a (InfList as) = InfList (a:as)

-- | Drops some elements from the beginning of an infinite list.
drop :: Int -> InfList a -> InfList a
drop n (InfList as) = InfList (Prelude.drop n as)
