{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
    formatTerm, parseTerm, countTerm, isClosed, foldVars, foldMapVars
  , -- ** Accessors and lists
    linear, toList, index, ixBound, BoundTerm(..)
  , -- ** Type inference
    infer, check, quantify
  , -- ** Closed terms
    ClosedTerm(..), closedTerm, _closedTerm
  , -- ** Closed terms with the 'Genetic' class
    TypeSet(..), GeneticTerm(..), runGeneticTerm
  , -- ** Generating terms
    -- | Generated terms may or may not be well-typed.
    genTerm, genModifiedTerm, genClosedTerm
  , -- ** Reductions
    -- | These functions do /not/ consider types, because substitution is independent of typing.
    substitute, reduceBeta, reduceEta, reduceEtaShallow, reduceStep, reduceSteps
  , -- ** Church encodings
    encodeChurchNumber, interpretChurchNumber, interpretChurchPair
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Lens
  ( Index
  , IxValue
  , Ixed
  , Prism'
  , Traversal'
  , ix
  , preview
  , prism'
  , set
  )
import Data.Conduit (ConduitT)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(Any), getAny)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup (Max(Max), getMax)
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

-- | Generates a 'Term' with a specified number of free variables and a
-- generator of constants.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- @genTerm _ 0@ always generates a closed term in a form of an @('Abs' _)@.
--
-- TODO Allow @Const@
genTerm :: Gen (Maybe (MonoType, String)) -> Int -> Gen Term
genTerm genConst' freeNum = do
  -- see LambdaCalculus.Term.genTerm for explanation of the probability
  size <- max 1 <$> Q.getSize
  mconst <- genConst
  case mconst of
    _ | freeNum < 1 -> genAbs
    Nothing -> do
          let p = 10000 * (size + 2) `div` (3 * size)
              q = (10000 - p) `div` 2
          Q.frequency [(p, genVar), (q, genAbs), (q, genApp)]
    Just c -> do
          let p = 10000 * (size + 2) `div` (6 * size)
              q = (10000 - 2 * p) `div` 2
          Q.frequency [(p, pure c), (p, genVar), (q, genAbs), (q, genApp)]
  where
  -- 1 term
  genConst = fmap (fmap (uncurry Const)) genConst'
  -- 1 term
  genVar = Var <$> Q.choose (1, freeNum)
  -- X + 1 terms
  genAbs = Abs <$> genTerm genConst' (freeNum+1)
  -- 2X + 1 terms
  genApp = App <$> genTerm genConst' freeNum <*> genTerm genConst' freeNum

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: Gen (Maybe (MonoType, String)) -> Int -> Term -> Gen Term
genModifiedTerm genConst freeNum m = do
  i <- Q.choose (0, countTerm m - 1)
  flip (ixBound i) m $ \BoundTerm{boundNum} -> genTerm genConst $ boundNum + freeNum

-- | Generates a closed 'Term'.
genClosedTerm :: Gen (Maybe (MonoType, String)) -> Gen ClosedTerm
genClosedTerm genConst = ClosedTerm <$> genTerm genConst 0

-- | A closed lambda term. This assumption allows more type instances to be defined.
newtype ClosedTerm = ClosedTerm { unClosedTerm :: Term }
  deriving (CoArbitrary, Eq, Generic, NFData, Q.Function, Show)

instance Ixed ClosedTerm where
  ix :: Int -> Traversal' ClosedTerm Term
  ix i f = fmap ClosedTerm . ix i f . unClosedTerm

type instance Index ClosedTerm = Int
type instance IxValue ClosedTerm = Term

-- | A smart constructor of 'ClosedTerm' which checks whether a 'Term' is
-- closed, and converts it into a 'ClosedTerm' if it is the case.
closedTerm :: Term -> Maybe ClosedTerm
closedTerm m | isClosed m = Just (ClosedTerm m)
closedTerm _ = Nothing

-- | A prism version of 'closedTerm'.
_closedTerm :: Prism' Term ClosedTerm
_closedTerm = prism' unClosedTerm closedTerm

-- | A class for phantom types to control instances of 'Arbitrary'.
class TypeSet a where
  -- | A set of constants which may be included in an arbitrary @'ClosedTerm' a@.
  genCandidateConst :: proxy a -> Gen (Maybe (MonoType, String))

-- | A closed term for use with 'Genetic'.
--
-- The phantom type controls instances of 'Arbitrary'. See 'TypeSet'.
newtype GeneticTerm a = GeneticTerm { unGeneticTerm :: ClosedTerm }
  deriving (CoArbitrary, Eq, Generic, NFData, Q.Function, Show)

instance Ixed (GeneticTerm a) where
  ix :: Int -> Traversal' (GeneticTerm a) Term
  ix i f = fmap (GeneticTerm . ClosedTerm) . ix i f . runGeneticTerm

type instance Index (GeneticTerm a) = Int
type instance IxValue (GeneticTerm a) = Term

instance TypeSet a => Arbitrary (GeneticTerm a) where
  arbitrary = GeneticTerm <$> genClosedTerm (genCandidateConst (Proxy :: Proxy a))

instance TypeSet a => Genetic (GeneticTerm a) where
  -- TODO always generate well-typed terms?
  genCrossover p12@(runGeneticTerm -> parent1, runGeneticTerm -> parent2) = do
    i1 <- Q.choose (0, countTerm parent1 - 1)
    i2 <- Q.choose (0, countTerm parent2 - 1)
    let sub1 = preview (ixBound' i1) parent1
        sub2 = preview (ixBound' i2) parent2
        child1 = maybe id (set (ix i1) . boundTerm) sub2 $ parent1
        child2 = maybe id (set (ix i2) . boundTerm) sub1 $ parent2
    -- retry if not swappable TODO implement without retrying
    if fromMaybe False $ swappable <$> sub1 <*> sub2
      then pure (GeneticTerm $ ClosedTerm child1, GeneticTerm $ ClosedTerm child2)
      else genCrossover p12
    where
    ixBound' :: Int -> Traversal' Term BoundTerm
    ixBound' i f = ixBound i (fmap boundTerm . f)
    -- for terms to be closed after swapping, the number of free variables in
    -- swapped sub-terms must not increase.
    -- the certain set of inequal equations reduces to this simple one.
    swappable :: BoundTerm -> BoundTerm -> Bool
    swappable m n = boundNum m == boundNum n

  genMutant = fmap (GeneticTerm . ClosedTerm)
    . genModifiedTerm (genCandidateConst (Proxy :: Proxy a)) 0
    . runGeneticTerm

-- | Extracts a 'Term' from a 'GeneticTerm'.
runGeneticTerm :: GeneticTerm a -> Term
runGeneticTerm = unClosedTerm . unGeneticTerm

-- | Performs a substitution.
--
-- Substitutes the free variables in a term with elements in a list in the
-- order.
--
-- The indices of variables are adjusted to preserve the meaning of the term.
substitute :: InfList Term -> Term -> Term
substitute _ m@(Const _ _) = m
substitute s (Var x) = InfList.toList s !! (x-1)
substitute s (App m n) = App (substitute s m) (substitute s n)
substitute s (Abs m) = Abs (substitute (InfList.cons (Var 1) $ fmap (\i -> substitute s' (Var i)) $ InfList.enumFrom 1) m)
  where
  s' = fmap shift s
  shift = substitute (fmap Var $ InfList.enumFrom 2)

-- | Performs a beta-reduction.
reduceBeta :: Term -> Maybe Term
reduceBeta = \case
  App (Abs m) n -> Just $ go 0 m n
  _ -> Nothing
  where
  go bound (Var x) n = case compare (bound + 1) x of
    EQ -> incrementFreeVars bound n
    LT -> Var (x-1)
    GT -> Var x
  go bound (Abs m) n = Abs $ go (bound+1) m n
  go bound (App m m') n = App (go bound m n) (go bound m' n)
  go _ m@(Const _ _) _ = m

-- TODO export this? (cf. LambdaCalculus.DeBruijn.Crossover.incrementFreeVars)
incrementFreeVars :: Int -> Term -> Term
incrementFreeVars inc = go 0
  where
  go bound (Var x)
    | x > bound = Var (x + inc)
    | otherwise = Var x
  go bound (Abs m) = Abs $ go (bound+1) m
  go bound (App m n) = App (go bound m) (go bound n)
  go _ m@(Const _ _) = m

-- | Performs an eta-reduction on the outermost abstraction.
--
-- > reduceEtaShallow (Abs $ App m (Var 1)) == substitute [_, Var 1, Var 2, ..] m
-- >   if m doesn't reference the (from the point of m) free variable #1
reduceEtaShallow :: Term -> Maybe Term
reduceEtaShallow (Abs (App m (Var 1)))
  | not $ refers1 m = Just $ substitute (InfList.cons undefined $ fmap Var $ InfList.enumFrom 1) m
  where
  refers1 = getAny . foldVars (\bound x -> Any $ x - bound == 1)
reduceEtaShallow _ = Nothing

-- | Performs an eta-reduction on the innermost nested abstraction.
--
-- > reduceEta (Abs $ Abs $ App (Var 2) (Var 1)) == Abs $ Var 1
reduceEta :: Term -> Maybe Term
reduceEta (Abs m) = reduceEtaShallow (Abs m) <|> (Abs <$> reduceEta m)
reduceEta _ = Nothing

-- | @reduceStep m@ tries to reduce a redex one step.
--
-- If @m@ can't be reduced any more, returns @Nothing@.
reduceStep :: Term -> Maybe Term
reduceStep (Const _ _) = Nothing
reduceStep (Var _) = Nothing
reduceStep (reduceBeta -> Just m) = Just m
-- eta-reduction here changes the outcome of the main program, for some reason (TODO investigate why)
-- reduceStep (reduceEta -> Just m) = Just m
reduceStep (Abs m) = Abs <$> reduceStep m
reduceStep (App m n) = case reduceStep m of
  Just m' -> Just $ App m' n
  Nothing -> App m <$> reduceStep n

-- | Repeatedly reduces ('reduceStep') a term and yields each step.
reduceSteps :: Monad m => Term -> ConduitT i Term m ()
reduceSteps = C.unfold (fmap dupe . reduceStep)

-- | Interprets a lambda term as a Church numeral. The term must be fully reduced. (TODO add a newtype)
interpretChurchNumber :: Term -> Maybe Natural
interpretChurchNumber m0 =
  go $ reduceBeta' $ App (reduceBeta' $ App m0 (Var vplus)) (Var vzero)
  where
  go (Var x) | x == vzero = Just 0
  go (App (Var x) n) | x == vplus = fmap (1+) $ go n
  go _ = Nothing

  reduceBeta' m = fromMaybe m (reduceBeta m)

  -- use @maximum free variable index + 1@ to avoid conflict in case the term is open
  vzero = succ $ maxFreeVar m0
  vplus = succ vzero

  -- finds the maximum index of the free variable in a term
  maxFreeVar = getMax . foldMapVars (Max 0) Max (\bound x -> x - bound)

-- | Encodes a natural number into a Church numeral.
encodeChurchNumber :: Natural -> Term
encodeChurchNumber n = Abs $ Abs $ iterate (App (Var 2)) (Var 1) !! fromIntegral n

-- | Interprets a lambda term as a Church pair.
--
-- The argument can be a redex. Always returns redexes.
interpretChurchPair :: Term -> (Term, Term)
interpretChurchPair m =
  ( App m (Abs (Abs (Var 2)))
  , App m (Abs (Abs (Var 1)))
  )
