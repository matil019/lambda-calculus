{-# LANGUAGE LambdaCase #-}
-- | Parses terms and types from human-friendly strings.
module LambdaCalculus.SimplyTyped.HindleyMilner.Parse (parseMonoType, parseTerm) where

import Control.Monad (guard, mzero)
import Data.Char (isLower, isSpace, isUpper)
import LambdaCalculus.SimplyTyped.HindleyMilner.Term (Term(Abs, App, Const, Var))
import LambdaCalculus.SimplyTyped.HindleyMilner.Types (MonoType((:->), ConstType, VarType), VarType)
import Text.ParserCombinators.ReadP ((+++), (<++), ReadP, between, char, readP_to_S, skipSpaces)

import qualified Text.Read.Lex as L

-- | Parses a 'String' into a 'Term'.
--
-- If a term contains any 'Const', its annotation must satisfy the restrictions of
-- 'parseMonoType'. Other than that, this is an inverse of
-- 'LambdaCalculus.SimplyTyped.HindleyMilner.Term.formatTerm'.
--
-- - Currently no extra parentheses are allowed. TODO allow this
parseTerm :: String -> Maybe Term
parseTerm = accept . readP_to_S parseTermP

-- | Parses a 'String' into a 'MonoType'.
--
-- This is mostly an inverse of 'LambdaCalculus.SimplyTyped.HindleyMilner.Types.formatMonoType',
-- with the following restrictions:
--
-- - A name must be a valid Haskell non-symbolic identifier.
-- - A name beginning with an uppercase letter is parsed as a 'ConstType', and
--   otherwise v'VarType'.
-- - Currently no extra parentheses are allowed. TODO allow this
parseMonoType :: String -> Maybe MonoType
parseMonoType = accept . readP_to_S parseMonoTypeP

accept :: [(a, String)] -> Maybe a
accept [(ok, rest)] | all isSpace rest = Just ok
accept _ = Nothing

-- The convention here is that every parser is assumed to skip leading spaces
-- but try to leave trailing spaces if possible.

parseTermP :: ReadP Term
parseTermP
    = fmap (uncurry App) parseApp
  <++ fmap Abs parseAbs
  <++ fmap Var parseVar
  <++ fmap (uncurry Const) parseConst

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
  hd <- (inParen $ fmap Abs parseAbs)
       <++ fmap Var parseVar
       <++ fmap (uncurry Const) parseConst
  arg1 <- parseArg
  args <- amap parseArg
  pure $ go hd arg1 args
  where
  parseArg
      = (inParen $ fmap Abs parseAbs)
    <++ fmap Var parseVar
    <++ fmap (uncurry Const) parseConst
    <++ (inParen $ fmap (uncurry App) parseApp)
  go hd arg1 [] = (hd, arg1)
  go hd arg1 (arg2:args) = go (App hd arg1) arg2 args

parseConst :: ReadP (MonoType, String)
parseConst = inParen $ do
  a <- parseIdentifier
  L.expect (L.Punc "::")
  t <- parseMonoTypeP
  pure (t, a)

parseMonoTypeP :: ReadP MonoType
parseMonoTypeP
    = fmap (uncurry (:->)) parseFuncType
  <++ fmap ConstType parseConstType
  <++ fmap VarType parseVarType

parseVarType :: ReadP VarType
parseVarType = do
  x <- parseIdentifier
  guard $ case x of
    (h:_) -> isLower h || h == '_'
    _ -> False
  pure x

parseConstType :: ReadP String
parseConstType = do
  x <- parseIdentifier
  guard $ case x of
    -- note that @isUpper '_' == False@
    (h:_) -> isUpper h
    _ -> False
  pure x

parseFuncType :: ReadP (MonoType, MonoType)
parseFuncType = do
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

-- | Like 'Text.ParserCombinators.ReadP.many', but biased on parsing input as many as possible.
amap :: ReadP a -> ReadP [a]
amap p = ((:) <$> p <*> amap p) <++ pure []
