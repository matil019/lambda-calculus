{-# LANGUAGE LambdaCase #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Parse (parseMonoType, parseTerm) where

-- TODO no all-in imports
import LambdaCalculus.SimplyTyped.HindleyMilner.Term
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import Text.ParserCombinators.ReadP

import Control.Monad (mzero)
import Data.Char (isSpace)

import qualified Text.Read.Lex as L

-- | Parses a 'String' into a 'Term'.
--
-- This is an inverse of 'formatTerm' i.e.
--
-- @
-- 'parseTerm' . 'formatTerm' ≡ pure
-- @
--
-- Currently no extra parentheses are allowed. TODO allow this
--
-- TODO add a test to make sure
parseTerm :: String -> Maybe Term
parseTerm = accept . readP_to_S parseTermP

-- | Parses a 'String' into a 'MonoType'.
--
-- This is an inverse of 'formatMonoType' i.e.
--
-- @
-- 'parseMonoType' . 'formatMonoType' ≡ pure
-- @
--
-- Currently no extra parentheses are allowed. TODO allow this
--
-- TODO add a test to make sure
parseMonoType :: String -> Maybe MonoType
parseMonoType = accept . readP_to_S parseMonoTypeP

accept :: [(a, String)] -> Maybe a
accept [(ok, rest)] | all isSpace rest = Just ok
accept _ = Nothing

-- The convention here is that every parser is assumed to skip leading spaces
-- but try to leave trailing spaces if possible.

parseTermP :: ReadP Term
parseTermP
    = fmap Var parseVar
  +++ fmap Abs parseAbs
  +++ fmap (uncurry App) parseApp
  +++ fmap (uncurry Const) parseConst

parseVar :: ReadP Int
parseVar = do
  x <- L.lex >>= \case
    L.Number x -> pure x
    _ -> mzero
  maybe mzero (pure . fromIntegral) $ L.numberToInteger x

parseAbs :: ReadP Term
parseAbs = do
  L.expect (L.Punc "\\")
  parseTermP

parseApp :: ReadP (Term, Term)
parseApp = do
  m <- (inParen $ fmap Abs parseAbs)
       +++ fmap (uncurry App) parseApp
       +++ fmap Var parseVar
       +++ fmap (uncurry Const) parseConst
  n <- (inParen $ fmap Abs parseAbs)
       +++ (inParen $ fmap (uncurry App) parseApp)
       +++ fmap Var parseVar
       +++ fmap (uncurry Const) parseConst
  pure (m, n)

parseConst :: ReadP (MonoType, String)
parseConst = inParen $ do
  a <- parseIdentifier
  L.expect (L.Punc "::")
  t <- parseMonoTypeP
  pure (t, a)

parseMonoTypeP :: ReadP MonoType
parseMonoTypeP
    = fmap VarType parseVarType
  +++ fmap ConstType parseConstType
  +++ fmap (uncurry (:->)) parseFuncType

parseVarType :: ReadP VarType
parseVarType = parseIdentifier

parseConstType :: ReadP String
parseConstType = parseIdentifier

parseFuncType :: ReadP (MonoType, MonoType)
parseFuncType = do
  skipSpaces
  t <- (inParen $ fmap (uncurry (:->)) parseFuncType)
       +++ fmap VarType parseVarType
       +++ fmap ConstType parseConstType
  L.expect (L.Punc "->")
  u <- parseMonoTypeP
  pure (t, u)

-- | Parses an identifier or a string literal.
parseIdentifier :: ReadP String
parseIdentifier =
  L.lex >>= \case
    L.String x -> pure x
    L.Ident  x -> pure x
    _ -> mzero

inParen :: ReadP a -> ReadP a
inParen = between (skipSpaces >> char '(') (skipSpaces >> char ')')
