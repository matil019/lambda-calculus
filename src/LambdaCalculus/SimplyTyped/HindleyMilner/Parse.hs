module LambdaCalculus.SimplyTyped.HindleyMilner.Parse (parseMonoType, parseTerm) where

-- TODO no all-in imports
import LambdaCalculus.SimplyTyped.HindleyMilner.Term
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import Text.ParserCombinators.ReadP

import Control.Monad (mzero)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Text.Read (readMaybe)

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

-- The convention here is that every parser must skip leading spaces but
-- try to leave trailing spaces if possible.

parseTermP :: ReadP Term
parseTermP = do
  skipSpaces
  fmap Var parseVar
    +++ fmap Abs parseAbs
    +++ fmap (uncurry App) parseApp
    +++ fmap (uncurry Const) parseConst

parseVar :: ReadP Int
parseVar = do
  skipSpaces
  x <- parseIdentifier
  maybe mzero pure $ readMaybe x

parseAbs :: ReadP Term
parseAbs = do
  skipSpaces
  _ <- char '\\'
  -- a space after the backslash is mandatory
  _ <- satisfy isSpace
  parseTermP

parseApp :: ReadP (Term, Term)
parseApp = do
  skipSpaces
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
  skipSpaces
  a <- parseIdentifier
  skipSpaces
  _ <- string "::"
  skipSpaces
  t <- parseMonoTypeP
  pure (t, a)

parseMonoTypeP :: ReadP MonoType
parseMonoTypeP = do
  skipSpaces
  fmap VarType parseVarType
    +++ fmap ConstType parseConstType
    +++ fmap (uncurry (:->)) parseFuncType

parseVarType :: ReadP VarType
parseVarType = do
  skipSpaces
  parseIdentifier

parseConstType :: ReadP String
parseConstType = do
  skipSpaces
  parseIdentifier

parseFuncType :: ReadP (MonoType, MonoType)
parseFuncType = do
  skipSpaces
  t <- (inParen $ fmap (uncurry (:->)) parseFuncType)
       +++ fmap VarType parseVarType
       +++ fmap ConstType parseConstType
  skipSpaces
  _ <- string "->"
  skipSpaces
  u <- parseMonoTypeP
  pure (t, u)

parseIdentifier :: ReadP String
parseIdentifier = do
  skipSpaces
  (:) <$> satisfy (\c -> isAlpha c || c == '_') <*> munch (\c -> isAlphaNum c || c == '_')

inParen :: ReadP a -> ReadP a
inParen = between (skipSpaces >> char '(') (skipSpaces >> char ')')
