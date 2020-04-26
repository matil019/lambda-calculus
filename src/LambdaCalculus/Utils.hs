{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- | Miscellaneous utilities.
module LambdaCalculus.Utils where

import Control.DeepSeq (NFData)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Text.Read.Lex as L

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

-- | Check if a String is "a simple identifier".
--
-- A simple identifier is a valid, non-symbolic Haskell identifier. This is used to
-- decide whether an identifier must be quoted in formatters such as @formatTerm@.
isSimpleIdent :: String -> Bool
isSimpleIdent = accept . readP_to_S ((\case { L.Ident _ -> pure True; _ -> pure False; }) =<< L.lex)
  where
  accept [(a, "")] = a
  accept _ = False
