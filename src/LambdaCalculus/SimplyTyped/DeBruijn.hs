{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCalculus.SimplyTyped.DeBruijn where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix, preview, set)
import Control.Monad (guard)
import Data.Conduit (ConduitT)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple.Extra (dupe)
import GHC.Generics (Generic)
import LambdaCalculus.Genetic (Genetic, genCrossover)
import LambdaCalculus.Utils (at)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified LambdaCalculus.Genetic
import qualified Test.QuickCheck as Q

-- TODO lots of functions were copy-pasted from untyped DeBruijn.

type BaseType = String

data Type
  = BaseType BaseType
  | FuncType Type Type
  deriving (Eq, Generic, NFData, Show)

-- | A synonym for 'FuncType'.
pattern (:->) :: Type -> Type -> Type
pattern t :-> u = FuncType t u
infixr 1 :->

{-# COMPLETE BaseType, (:->) #-}

formatType :: Type -> String
formatType (BaseType bt) = bt
formatType (FuncType t u) =
  let decorate = case t of
        FuncType _ _ -> \s -> "(" <> s <> ")"
        BaseType _ -> id
  in decorate (formatType t) <> " -> " <> formatType u

data Term
  = Var Int                -- ^ A variable (starts at @1@)
  | Abs (Maybe Type) Term  -- ^ An abstraction with an optional explicit type annotation
  | App Term Term          -- ^ An application
  | Const BaseType String  -- ^ A constant
  deriving (Eq, Generic, NFData, Show)

-- | Traverses sub-terms in depth-first, pre-order.
--
-- This is consistent with 'index':
-- > preview (ix i) == Just (index i)
--
-- See also 'ixBound'.
--
-- TODO make sure that @instance At Term@ does *not* form a "reasonable instance"
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f = ixBound i (f . boundTerm)

type instance Index Term = Int
type instance IxValue Term = Term

formatTerm :: Term -> String
formatTerm (Var x) = show x
formatTerm (Const _ x) = x
formatTerm (Abs t m) =
  let ts = maybe "" (\s -> "[" <> formatType s <> "]") t
  in "(\\" <> ts <> " " <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

formatTermWithType :: [Type] -> Term -> String
formatTermWithType ctx m =
  formatTerm m <> " " <> maybe "<ill-typed>" (\t -> "[" <> formatType t <> "]") (typeOf ctx m)

countTerm :: Term -> Int
countTerm (Const _ _) = 1
countTerm (Var _) = 1
countTerm (Abs _ m) = 1 + countTerm m
countTerm (App m n) = 1 + countTerm m + countTerm n

-- | Is this 'Term' closed (i.e. has no free variables)?
--
-- Constants are not considered as free variables.
isClosed :: Term -> Bool
isClosed = go 0
  where
  go !_ (Const _ _) = True
  go !bound (Var x) = x <= bound
  go !bound (Abs _ m) = go (bound+1) m
  go !bound (App m n) = go bound m && go bound n

-- | @linear m@ is a non-empty list whose elements are the sub-terms of @m@
-- traversed in depth-first, pre-order.
--
-- The first element is always @m@.
--
-- The following law holds:
--
-- > length ('linear' m) == 'countTerm' m
linear :: Term -> NonEmpty Term
linear m = m :| case m of
  Const _ _ -> []
  Var _ -> []
  Abs _ n -> NE.toList $ linear n
  App n1 n2 -> NE.toList $ linear n1 <> linear n2

-- | This list can never be empty. See 'linear'
toList :: Term -> [Term]
toList = NE.toList . linear

-- | @index i m@ traverses @m@ to find a sub-term.
--
-- @m@ is traversed in depth-first, pre-order. @i == 0@ denotes @m@ itself.
--
-- > index 0 m == Just m
-- > index 3 ('App' ('App' ('Var' \'x\') n) o) == Just n
--
-- Another equivalence:
-- > 'toList' m !! i == fromJust ('index' i m)
-- TODO add test
index :: Int -> Term -> Maybe Term
index i m = at i (toList m)

-- | A term with additional info about its enclosing term.
data BoundTerm = BoundTerm
  { boundTerm :: Term
  -- | @boundTerm == Var x@ is bound if @x <= boundNum@.
  , boundNum :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | 'ix @Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = loop 0
  where
  loop :: Applicative f => Int -> Int -> (BoundTerm -> f Term) -> Term -> f Term
  loop boundNum i f m
    | i == 0 = f (BoundTerm{boundTerm = m, boundNum})
    | i < 0 = pure m
    | i >= countTerm m = pure m
    | otherwise = case m of
        Const _ _ -> pure m
        Var _ -> pure m
        Abs t n -> Abs t <$> loop (boundNum + 1) (i-1) f n
        App n1 n2 -> App <$> loop boundNum (i-1) f n1 <*> loop boundNum (i-1-(countTerm n1)) f n2

-- | Generates a 'Term' with a specified number of free variables and a set of constants.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- If no free variables, @genTerm@ always generates a closed term in a form of an @'Abs' _ _@.
-- TODO Allow @App@ (consider the size)
-- TODO Allow @Const@
-- TODO add another implementation which always yields a well-typed Terms
genTerm :: NonEmpty Type -> [(BaseType, String)] -> Int -> Gen Term
genTerm types constants freeNum = do
  -- see LambdaCalculus.Term.genTerm for explanation of the probability
  size <- max 1 <$> Q.getSize
  case () of
    _ | freeNum < 1 -> genAbs
      | null constants -> do
          let p = 10000 * (size + 2) `div` (3 * size)
              q = (10000 - p) `div` 2
          Q.frequency [(p, genVar), (q, genAbs), (q, genApp)]
      | otherwise -> do
          let p = 10000 * (size + 2) `div` (6 * size)
              q = (10000 - 2 * p) `div` 2
          Q.frequency [(p, genConst), (p, genVar), (q, genAbs), (q, genApp)]
  where
  -- 1 term
  genConst = uncurry Const <$> Q.elements constants
  -- 1 term
  genVar = Var <$> Q.choose (1, freeNum)
  -- X + 1 terms
  genAbs = do
    -- TODO generate function-type parameter e.g. (\[o->o] \[o] 2 1)
    t <- Q.liftArbitrary $ Q.elements $ NE.toList types
    Abs t <$> genTerm types constants (freeNum+1)
  -- 2X + 1 terms
  genApp = do
    m <- genTerm types constants freeNum
    n <- genTerm types constants freeNum
    case m of
      -- "correct" the parameter type if possible
      Abs _ e
        | let ctx = []  -- TODO collect typing context over recursion
        , Just t <- bidirectionalTypeSynth ctx n
          -> pure $ App (Abs (Just t) e) n
      -- if not, pretend nothing happened
      _   -> pure $ App m n

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: NonEmpty Type -> [(BaseType, String)] -> Int -> Term -> Gen Term
genModifiedTerm types constants freeNum m = do
  i <- Q.choose (0, countTerm m - 1)
  flip (ixBound i) m $ \BoundTerm{boundNum} -> genTerm types constants $ boundNum + freeNum

-- | Generates a closed 'Term'.
genClosedTerm :: NonEmpty Type -> [(BaseType, String)] -> Gen Term
genClosedTerm types constants = genTerm types constants 0

-- | A class for phantom types to control instances of 'Arbitrary'.
class TypeSet a where
  candidateTypes :: proxy a -> NonEmpty Type
  candidateConsts :: proxy a -> [(BaseType, String)]

-- | A closed lambda term. This assumption allows more type instances to be defined.
--
-- The phantom type controls instances of 'Arbitrary'. See 'TypeSet'.
newtype ClosedTerm a = ClosedTerm { unClosedTerm :: Term }
  deriving (Eq, Generic, NFData, Show)

instance TypeSet a => Arbitrary (ClosedTerm a) where
  arbitrary = ClosedTerm <$> genClosedTerm
    (candidateTypes (Proxy :: Proxy a))
    (candidateConsts (Proxy :: Proxy a))

instance Ixed (ClosedTerm a) where
  ix :: Int -> Traversal' (ClosedTerm a) Term
  ix i f = fmap ClosedTerm . ix i f . unClosedTerm

type instance Index (ClosedTerm a) = Int
type instance IxValue (ClosedTerm a) = Term

instance TypeSet a => Genetic (ClosedTerm a) where
  -- TODO always generate well-typed terms?
  genCrossover p12@(ClosedTerm parent1, ClosedTerm parent2) = do
    i1 <- Q.choose (0, countTerm parent1 - 1)
    i2 <- Q.choose (0, countTerm parent2 - 1)
    let sub1 = preview (ixBound' i1) parent1
        sub2 = preview (ixBound' i2) parent2
        child1 = maybe id (set (ix i1) . boundTerm) sub2 $ parent1
        child2 = maybe id (set (ix i2) . boundTerm) sub1 $ parent2
    -- retry if not swappable TODO implement without retrying
    if fromMaybe False $ swappable <$> sub1 <*> sub2
      then pure (ClosedTerm child1, ClosedTerm child2)
      else genCrossover p12
    where
    ixBound' :: Int -> Traversal' Term BoundTerm
    ixBound' i f = ixBound i (fmap boundTerm . f)
    -- for terms to be closed after swapping, the number of free variables in
    -- swapped sub-terms must not increase.
    -- the certain set of inequal equations reduces to this simple one.
    swappable :: BoundTerm -> BoundTerm -> Bool
    swappable m n = boundNum m == boundNum n

  genMutant = fmap ClosedTerm
    . genModifiedTerm
        (candidateTypes (Proxy :: Proxy a))
        (candidateConsts (Proxy :: Proxy a))
        0
    . unClosedTerm

-- | A simple type checker which requires that all 'Abs' have type annotations.
-- TODO write tests
typeOf
  :: [Type] -- ^ typing context: @type of (Var x) == ctx !! (x-1)@
  -> Term
  -> Maybe Type
typeOf ctx (Var x) = at (x-1) ctx
typeOf _ (Const t _) = pure (BaseType t)
typeOf ctx (Abs mt m) = mt >>= \t -> typeOf (t:ctx) m >>= pure . FuncType t
typeOf ctx (App m n) = do
  FuncType s t <- typeOf ctx m
  u <- typeOf ctx n
  guard $ s == u
  pure t

-- The algorithm uses this made-up rule. TODO prove this
--
-- > Γ, x:s |- e => t
-- > ------------------------
-- > Γ |- \(x:s). e => s -> t
--
-- Because we don't have "an annotated term". With our datatypes only the
-- parameter of 'Abs' can be annotated, but not the whole term.
bidirectionalTypeSynth
  :: [Type] -- ^ typing context: @type of (Var x) == ctx !! (x-1)@
  -> Term
  -> Maybe Type
bidirectionalTypeSynth ctx (Var x) = at (x-1) ctx
bidirectionalTypeSynth _ (Const t _) = pure (BaseType t)
bidirectionalTypeSynth ctx (Abs mt m) = mt >>= \t -> bidirectionalTypeSynth (t:ctx) m >>= pure . (t :->)
bidirectionalTypeSynth ctx (App m n) = do
  s :-> t <- bidirectionalTypeSynth ctx m
  guard $ bidirectionalTypeCheck ctx s n
  pure t

bidirectionalTypeCheck
  :: [Type] -- ^ typing context: @type of (Var x) == ctx !! (x-1)@
  -> Type   -- ^ type to check against
  -> Term
  -> Bool
bidirectionalTypeCheck ctx t m | Just t == bidirectionalTypeSynth ctx m = True
bidirectionalTypeCheck ctx (s :-> t) (Abs _ m) = bidirectionalTypeCheck (s:ctx) t m
bidirectionalTypeCheck _ _ _ = False

-- | A well-typed 'Term'. (i.e. a Term which has been typechecked)
--
-- When you see a function taking 'WTerm' as an argument, it assumes that
-- the term is well-typed.
type WTerm = Term

-- | The list must be infinite. TODO add a newtype
--
-- This function does *not* consider types, because substitution is an
-- independent of typing.
substitute :: [Term] -> Term -> Term
substitute _ m@(Const _ _) = m
substitute s (Var x) = s !! (x-1)
substitute s (App m n) = App (substitute s m) (substitute s n)
substitute s (Abs t m) = Abs t (substitute (Var 1 : map (\i -> substitute s' (Var i)) [1..]) m)
  where
  s' = map shift s
  shift = substitute (map Var [2..])

-- | Beta reduction.
reduceBeta :: WTerm -> WTerm
reduceBeta (App (Abs _ m) n) = substitute (n:map Var [1..]) m
reduceBeta m = m

-- TODO take advantage of the strongly normalizing property and optimize
reduceStep :: WTerm -> Maybe WTerm
reduceStep (Const _ _) = Nothing
reduceStep (Var _) = Nothing
reduceStep (Abs t m) = Abs t <$> reduceStep m
reduceStep m@(App (Abs _ _) _) = Just $ reduceBeta m
reduceStep (App m n) = case reduceStep m of
  Just m' -> Just $ App m' n
  Nothing -> App m <$> reduceStep n

reduceSteps :: Monad m => Term -> ConduitT i Term m ()
reduceSteps = C.unfold (fmap dupe . reduceStep)

-- | Interprets a lambda term as a Church numeral. The term must be fully reduced.
interpretChurchNumber :: WTerm -> Maybe Natural
interpretChurchNumber = \m ->
  go $ reduceBeta $ App (reduceBeta (App m (Var 2))) (Var 1)
  where
  go (Var 1) = Just 0
  go (App (Var 2) n) = fmap (1+) $ go n
  go _ = Nothing

encodeChurchNumber :: Type -> Natural -> WTerm
encodeChurchNumber t n =
  Abs (Just (t :-> t)) $ Abs (Just t) $ iterate (App (Var 2)) (Var 1) !! fromIntegral n

-- | @interpretChurchPair m@ assumes @m@ to be a Church pair and extracts their elements.
--
-- A Church-encoded pair has a type @(a -> b -> c) -> c@ where @c@ is either @a@ or @b@.
-- @m@ is typechecked against it and if it doesn't, @Nothing@ is returned.
--
-- Note that if @Just (x, y)@ is returned, @x@ and/or @y@ may not typecheck.
--
-- As of now, for both of elements to typecheck, the types @a@, and @b@ above must
-- coincide. The reason is that 'Abs' requires an explicit type annotation. This rules
-- out terms whose types can change over context. @c@ must necessarily be fixed to
-- either @a@ or @b@ on typecheck.
-- So it may be a better idea to consider 'interpretChurchPair' as a function which
-- returns a _list_ whose length is up to 2, rather than a 2-tuple.
-- TODO add type variables and/or type inference to lift this restriction
--
-- TODO add a newtype which indicates a term is known to be well-typed and has that type
interpretChurchPair :: WTerm -> Maybe (Term, Term)
interpretChurchPair m = case typeOf [] m of
  Just ((a :-> b :-> _) :-> _) ->
    let first  = Abs (Just ((a :-> b :-> a) :-> a)) $ App (Var 1) (Abs (Just a) $ Abs (Just b) $ Var 2)
        second = Abs (Just ((a :-> b :-> b) :-> b)) $ App (Var 1) (Abs (Just a) $ Abs (Just b) $ Var 1)
    in
    Just (App first m, App second m)
  _ -> Nothing
