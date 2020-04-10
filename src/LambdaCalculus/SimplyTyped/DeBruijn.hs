{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaCalculus.SimplyTyped.DeBruijn where

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal', ix, preview, set)
import Data.Conduit (ConduitT)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple.Extra (dupe)
import GHC.Generics (Generic)
import LambdaCalculus.Genetic (Genetic, genCrossover)
import LambdaCalculus.SimplyTyped.DeBruijn.Deprecated
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified LambdaCalculus.Genetic
import qualified Test.QuickCheck as Q

-- TODO lots of functions were copy-pasted from untyped DeBruijn.

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
-- genTerm :: [(MonoType, String)] -> Int -> Gen Term
-- genTerm constants freeNum = do
genTerm :: NonEmpty Type -> [(Type, String)] -> Int -> Gen Term
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
  genApp = App <$> genTerm types constants freeNum <*> genTerm types constants freeNum

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: NonEmpty Type -> [(Type, String)] -> Int -> Term -> Gen Term
genModifiedTerm types constants freeNum m = do
  i <- Q.choose (0, countTerm m - 1)
  flip (ixBound i) m $ \BoundTerm{boundNum} -> genTerm types constants $ boundNum + freeNum

-- | Generates a closed 'Term'.
genClosedTerm :: NonEmpty Type -> [(Type, String)] -> Gen Term
genClosedTerm types constants = genTerm types constants 0

-- | A class for phantom types to control instances of 'Arbitrary'.
class TypeSet a where
  candidateTypes :: proxy a -> NonEmpty Type
  candidateConsts :: proxy a -> [(Type, String)]

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
