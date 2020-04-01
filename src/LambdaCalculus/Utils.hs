{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Miscellaneous utilities.
module LambdaCalculus.Utils where

import Control.DeepSeq (NFData)

-- | A safe '(!!)'.
at :: Int -> [a] -> Maybe a
at i xs
  | i < 0 = Nothing
  | (x:_) <- drop i xs = Just x
  | otherwise = Nothing

-- | A finite list.
--
-- Note that it is programmer's responsibility to make sure the list is indeed finite.
newtype FiniteList a = FiniteList { unFiniteList :: [a] }
 deriving (Applicative, Eq, Foldable, Functor, Monad, Monoid, NFData, Ord, Semigroup, Show)
