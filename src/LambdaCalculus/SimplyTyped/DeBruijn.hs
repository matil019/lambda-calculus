{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | Simply-typed terms in De Bruijn index notation and some high-level stuff.
module LambdaCalculus.SimplyTyped.DeBruijn
  ( -- * Types
    VarType, MonoType(..), _VarType, _ConstType, _FuncType
  , formatMonoType, parseMonoType
  , PolyType(..), _Mono, _ForAll
  , topMono, boundVars
  , -- * Terms
    Term(..), _Var, _Abs, _App, _Const
  , -- ** Basic operations
    formatTerm, parseTerm, countTerm, isClosed
  , -- ** Accessors and lists
    linear, toList, index, ixBound, BoundTerm(..)
  , -- ** Type inference
    infer, check, quantify
  , -- ** Closed terms
    ClosedTerm(..), TypeSet(..), closedTerm, _closedTerm
  , -- ** Generating terms
    -- | Generated terms may or may not be well-typed.
    genTerm, genModifiedTerm, genClosedTerm
  , -- ** Reductions
    -- | These functions do /not/ consider types, because substitution is independent of typing.
    substitute, reduceBeta, reduceStep, reduceSteps
  , -- ** Church encodings
    encodeChurchNumber, interpretChurchNumber, _churchNumber
  , interpretChurchPair, _churchPair
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
  ( Index
  , IxValue
  , Ixed
  , Iso'
  , Prism
  , Prism'
  , Traversal'
  , iso
  , ix
  , preview
  , prism'
  , set
  )
import Data.Conduit (ConduitT)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple.Extra (dupe)
import GHC.Generics (Generic)
import LambdaCalculus.Genetic (Genetic, genCrossover)
import LambdaCalculus.InfList (InfList)
import LambdaCalculus.SimplyTyped.HindleyMilner (check, infer, quantify)
import LambdaCalculus.SimplyTyped.HindleyMilner.Parse (parseMonoType, parseTerm)
import LambdaCalculus.SimplyTyped.HindleyMilner.Term
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
  ( MonoType((:->), ConstType, VarType)
  , PolyType(ForAll, Mono)
  , VarType
  , _ConstType
  , _ForAll
  , _FuncType
  , _Mono
  , _VarType
  , boundVars
  , formatMonoType
  , topMono
  )
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, CoArbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified LambdaCalculus.Genetic
import qualified LambdaCalculus.InfList as InfList
import qualified Test.QuickCheck as Q

-- | Generates a 'Term' with a specified number of free variables and a set of
-- constants.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- @genTerm 0@ always generates a closed term in a form of an @('Abs' _)@.
-- TODO Allow @Const@
genTerm :: [(MonoType, String)] -> Int -> Gen Term
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
  genConst = uncurry Const <$> Q.elements constants
  -- 1 term
  genVar = Var <$> Q.choose (1, freeNum)
  -- X + 1 terms
  genAbs = Abs <$> genTerm constants (freeNum+1)
  -- 2X + 1 terms
  genApp = App <$> genTerm constants freeNum <*> genTerm constants freeNum

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: [(MonoType, String)] -> Int -> Term -> Gen Term
genModifiedTerm constants freeNum m = do
  i <- Q.choose (0, countTerm m - 1)
  flip (ixBound i) m $ \BoundTerm{boundNum} -> genTerm constants $ boundNum + freeNum

-- | Generates a closed 'Term'.
genClosedTerm :: [(MonoType, String)] -> Gen (ClosedTerm a)
genClosedTerm constants = ClosedTerm <$> genTerm constants 0

-- | A class for phantom types to control instances of 'Arbitrary'.
class TypeSet a where
  -- | A set of constants which may be included in an arbitrary @'ClosedTerm' a@.
  candidateConsts :: proxy a -> [(MonoType, String)]

-- | A closed lambda term. This assumption allows more type instances to be defined.
--
-- The phantom type controls instances of 'Arbitrary'. See 'TypeSet'.
newtype ClosedTerm a = ClosedTerm { unClosedTerm :: Term }
  deriving (CoArbitrary, Eq, Generic, NFData, Q.Function, Show)

instance TypeSet a => Arbitrary (ClosedTerm a) where
  arbitrary = genClosedTerm (candidateConsts (Proxy :: Proxy a))

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
    . genModifiedTerm (candidateConsts (Proxy :: Proxy a)) 0
    . unClosedTerm

-- | A smart constructor of 'ClosedTerm' which checks whether a 'Term' is
-- closed, and converts it into a 'ClosedTerm' if it is the case.
closedTerm :: Term -> Maybe (ClosedTerm a)
closedTerm m | isClosed m = Just (ClosedTerm m)
closedTerm _ = Nothing

-- | A prism version of 'closedTerm'.
--
-- TODO test to make sure this is really a prism
_closedTerm :: Prism Term Term (ClosedTerm a) (ClosedTerm b)
_closedTerm = prism' unClosedTerm closedTerm

-- | Performs a substitution.
--
-- You would like to use 'reduceBeta' instead of using this directly.
substitute :: InfList Term -> Term -> Term
substitute _ m@(Const _ _) = m
substitute s (Var x) = InfList.toList s !! (x-1)
substitute s (App m n) = App (substitute s m) (substitute s n)
substitute s (Abs m) = Abs (substitute (InfList.cons (Var 1) $ fmap (\i -> substitute s' (Var i)) $ InfList.enumFrom 1) m)
  where
  s' = fmap shift s
  shift = substitute (fmap Var $ InfList.enumFrom 2)

-- | Performs a beta-reduction.
reduceBeta :: Term -> Term
reduceBeta (App (Abs m) n) = substitute (InfList.cons n $ fmap Var $ InfList.enumFrom 1) m
reduceBeta m = m

-- | @reduceStep m@ tries to reduce a beta-redex one step.
--
-- If @m@ can't be reduced any more, returns @Nothing@.
reduceStep :: Term -> Maybe Term
reduceStep (Const _ _) = Nothing
reduceStep (Var _) = Nothing
reduceStep (Abs m) = Abs <$> reduceStep m
reduceStep m@(App (Abs _) _) = Just $ reduceBeta m
reduceStep (App m n) = case reduceStep m of
  Just m' -> Just $ App m' n
  Nothing -> App m <$> reduceStep n

-- | Repeatedly reduces ('reduceStep') a term and yields each step.
reduceSteps :: Monad m => Term -> ConduitT i Term m ()
reduceSteps = C.unfold (fmap dupe . reduceStep)

-- | Interprets a lambda term as a Church numeral. The term must be fully reduced. (TODO add a newtype)
interpretChurchNumber :: Term -> Maybe Natural
interpretChurchNumber = \m ->
  go $ reduceBeta $ App (reduceBeta (App m (Var 2))) (Var 1)
  where
  go (Var 1) = Just 0
  go (App (Var 2) n) = fmap (1+) $ go n
  go _ = Nothing

-- | Encodes a natural number into a Church numeral.
encodeChurchNumber :: Natural -> Term
encodeChurchNumber n = Abs $ Abs $ iterate (App (Var 2)) (Var 1) !! fromIntegral n

-- | A prism which encodes a natural number to a Church numeral and decodes back.
--
-- TODO test to make sure this is really a prism
_churchNumber :: Prism' Term Natural
_churchNumber = prism' encodeChurchNumber interpretChurchNumber

-- | Interprets a lambda term as a Church pair.
--
-- The argument can be a redex. Always returns redexes.
interpretChurchPair :: Term -> (Term, Term)
interpretChurchPair m =
  ( App m (Abs (Abs (Var 2)))
  , App m (Abs (Abs (Var 1)))
  )

-- | An iso which encodes a pair of terms to a Church pair and decodes back.
--
-- TODO test to make sure this is really an iso (I believe a few 'reduceBeta'
-- are needed)
_churchPair :: Iso' Term (Term, Term)
_churchPair = iso interpretChurchPair $ \(m, n) -> (App (App (Abs $ Var 1) m) n)
