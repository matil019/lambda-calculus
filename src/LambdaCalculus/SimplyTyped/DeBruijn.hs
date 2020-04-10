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
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple.Extra (dupe)
import GHC.Generics (Generic)
import LambdaCalculus.Genetic (Genetic, genCrossover)
import LambdaCalculus.SimplyTyped.DeBruijn.Deprecated
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified LambdaCalculus.Genetic
import qualified LambdaCalculus.SimplyTyped.HindleyMilner.Term as HM -- TODO make unqualified
import qualified LambdaCalculus.SimplyTyped.HindleyMilner.Types as HM -- TODO make unqualified
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
genTerm :: [(HM.MonoType, String)] -> Int -> Gen HM.Term
genTerm constants freeNum = do
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
  genConst = uncurry HM.Const <$> Q.elements constants
  -- 1 term
  genVar = HM.Var <$> Q.choose (1, freeNum)
  -- X + 1 terms
  genAbs = HM.Abs <$> genTerm constants (freeNum+1)
  -- 2X + 1 terms
  genApp = HM.App <$> genTerm constants freeNum <*> genTerm constants freeNum

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: [(HM.MonoType, String)] -> Int -> HM.Term -> Gen HM.Term
genModifiedTerm constants freeNum m = do
  i <- Q.choose (0, HM.countTerm m - 1)
  flip (HM.ixBound i) m $ \HM.BoundTerm{HM.boundNum} -> genTerm constants $ boundNum + freeNum

-- | Generates a closed 'Term'.
genClosedTerm :: [(HM.MonoType, String)] -> Gen HM.Term
genClosedTerm constants = genTerm constants 0

-- | A class for phantom types to control instances of 'Arbitrary'.
class TypeSet a where
  candidateConsts :: proxy a -> [(HM.MonoType, String)]

-- | A closed lambda term. This assumption allows more type instances to be defined.
--
-- The phantom type controls instances of 'Arbitrary'. See 'TypeSet'.
newtype ClosedTerm a = ClosedTerm { unClosedTerm :: HM.Term }
  deriving (Eq, Generic, NFData, Show)

instance TypeSet a => Arbitrary (ClosedTerm a) where
  arbitrary = ClosedTerm <$> genClosedTerm (candidateConsts (Proxy :: Proxy a))

instance Ixed (ClosedTerm a) where
  ix :: Int -> Traversal' (ClosedTerm a) HM.Term
  ix i f = fmap ClosedTerm . ix i f . unClosedTerm

type instance Index (ClosedTerm a) = Int
type instance IxValue (ClosedTerm a) = HM.Term

instance TypeSet a => Genetic (ClosedTerm a) where
  -- TODO always generate well-typed terms?
  genCrossover p12@(ClosedTerm parent1, ClosedTerm parent2) = do
    i1 <- Q.choose (0, HM.countTerm parent1 - 1)
    i2 <- Q.choose (0, HM.countTerm parent2 - 1)
    let sub1 = preview (ixBound' i1) parent1
        sub2 = preview (ixBound' i2) parent2
        child1 = maybe id (set (ix i1) . HM.boundTerm) sub2 $ parent1
        child2 = maybe id (set (ix i2) . HM.boundTerm) sub1 $ parent2
    -- retry if not swappable TODO implement without retrying
    if fromMaybe False $ swappable <$> sub1 <*> sub2
      then pure (ClosedTerm child1, ClosedTerm child2)
      else genCrossover p12
    where
    ixBound' :: Int -> Traversal' HM.Term HM.BoundTerm
    ixBound' i f = HM.ixBound i (fmap HM.boundTerm . f)
    -- for terms to be closed after swapping, the number of free variables in
    -- swapped sub-terms must not increase.
    -- the certain set of inequal equations reduces to this simple one.
    swappable :: HM.BoundTerm -> HM.BoundTerm -> Bool
    swappable m n = HM.boundNum m == HM.boundNum n

  genMutant = fmap ClosedTerm
    . genModifiedTerm (candidateConsts (Proxy :: Proxy a)) 0
    . unClosedTerm

-- | The list must be infinite. TODO add a newtype
--
-- This function does *not* consider types, because substitution is
-- independent of typing.
substitute :: [HM.Term] -> HM.Term -> HM.Term
substitute _ m@(HM.Const _ _) = m
substitute s (HM.Var x) = s !! (x-1)
substitute s (HM.App m n) = HM.App (substitute s m) (substitute s n)
substitute s (HM.Abs m) = HM.Abs (substitute (HM.Var 1 : map (\i -> substitute s' (HM.Var i)) [1..]) m)
  where
  s' = map shift s
  shift = substitute (map HM.Var [2..])

-- | Beta reduction.
reduceBeta :: HM.Term -> HM.Term
reduceBeta (HM.App (HM.Abs m) n) = substitute (n:map HM.Var [1..]) m
reduceBeta m = m

-- TODO take advantage of the strongly normalizing property and optimize
reduceStep :: HM.Term -> Maybe HM.Term
reduceStep (HM.Const _ _) = Nothing
reduceStep (HM.Var _) = Nothing
reduceStep (HM.Abs m) = HM.Abs <$> reduceStep m
reduceStep m@(HM.App (HM.Abs _) _) = Just $ reduceBeta m
reduceStep (HM.App m n) = case reduceStep m of
  Just m' -> Just $ HM.App m' n
  Nothing -> HM.App m <$> reduceStep n

reduceSteps :: Monad m => HM.Term -> ConduitT i HM.Term m ()
reduceSteps = C.unfold (fmap dupe . reduceStep)

-- | Interprets a lambda term as a Church numeral. The term must be fully reduced.
interpretChurchNumber :: HM.Term -> Maybe Natural
interpretChurchNumber = \m ->
  go $ reduceBeta $ HM.App (reduceBeta (HM.App m (HM.Var 2))) (HM.Var 1)
  where
  go (HM.Var 1) = Just 0
  go (HM.App (HM.Var 2) n) = fmap (1+) $ go n
  go _ = Nothing

encodeChurchNumber :: Natural -> HM.Term
encodeChurchNumber n =
  HM.Abs $ HM.Abs $ iterate (HM.App (HM.Var 2)) (HM.Var 1) !! fromIntegral n

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
interpretChurchPair :: Term -> Maybe (Term, Term)
interpretChurchPair m = case typeOf [] m of
  Just ((a :-> b :-> _) :-> _) ->
    let first  = Abs (Just ((a :-> b :-> a) :-> a)) $ App (Var 1) (Abs (Just a) $ Abs (Just b) $ Var 2)
        second = Abs (Just ((a :-> b :-> b) :-> b)) $ App (Var 1) (Abs (Just a) $ Abs (Just b) $ Var 1)
    in
    Just (App first m, App second m)
  _ -> Nothing
