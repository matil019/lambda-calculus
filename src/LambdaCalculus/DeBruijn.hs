{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Lambda terms in De Bruijn index notation.
module LambdaCalculus.DeBruijn where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.DeepSeq (NFData)
import Control.Lens (Index, IxValue, Ixed, Traversal, Traversal', ix, preview, set)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Conduit (ConduitT)
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Tuple.Extra (dupe)
import LambdaCalculus.Genetic (Genetic, genChildren)
import LambdaCalculus.Utils (at)
import Numeric.Natural (Natural)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, Gen)

import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified LambdaCalculus.Genetic
import qualified LambdaCalculus.Term.Types as Term
import qualified Test.QuickCheck as Q

-- | A lambda term in De Bruijn index notation.
data Term
  = Var Int        -- ^ A variable (must start at @1@)
  | Abs Term       -- ^ An abstraction
  | App Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

-- | Traverses sub-terms in depth-first, pre-order.
--
-- This is consistent with 'index':
--
-- @
-- 'preview' ('ix' i) == 'Just' ('index' i)
-- @
--
-- See also 'ixBound'.
instance Ixed Term where
  ix :: Int -> Traversal' Term Term
  ix i f = ixBound i (f . boundTerm)

type instance Index Term = Int
type instance IxValue Term = Term

-- | A term with additional info about its enclosing term.
data BoundTerm = BoundTerm
  { boundTerm :: Term
  -- | A variable @x@ is bound if @boundTerm == Var x@ and @x <= boundNum@.
  , boundNum :: Int
  }
  deriving (Eq, Generic, NFData, Show)

-- | Converts a lambda 'Term.Term' to De Bruijn index 'Term'.
--
-- The first value in the returned tuple is an ordered set of free variables
-- which the second refers.
toDeBruijn
  :: [Term.Var]  -- ^ Known free variables; the return value is guaranteed to begin with this list
  -> Term.Term   -- ^ The term to convert
  -> ([Term.Var], Term)
toDeBruijn = \free -> swap . flip runState free . go []
  where
  go :: [Term.Var] -> Term.Term -> State [Term.Var] Term
  go bound (Term.Var x)
    | Just idx <- elemIndex x bound = pure (Var (idx + 1))
    | otherwise = state $ \free ->
        case elemIndex x free of
          Just idx -> (Var (length bound + idx + 1), free)
          Nothing  -> (Var (length bound + length free + 1), free <> [x])
  go bound (Term.Abs x m) = Abs <$> go (x:bound) m
  go bound (Term.App m n) = do
    m' <- go bound m
    n' <- go bound n
    pure $ App m' n'

-- | Converts a De Bruijn index 'Term' into the ordinary notation.
--
-- The list must be long enough to have all the free variables the 'Term' refers.
--
-- The return value of 'toDeBruijn' can always be applied to 'fromDeBruijn'.
--
-- __NOTE__ the list must be finite! (TODO add a newtype)
fromDeBruijn :: [Term.Var] -> Term -> Term.Term
fromDeBruijn free = go infinitevars []
  where
  -- @go unused bound m@ recursively converts @m@ from DeBruijn notation to the ordinary one.
  --
  -- @unused@ is an infinite list of to-be-bound variables.
  -- @bound@ is a list of bound variables. Its first element is bound by the innermost abstraction.
  go _ bound (Var n) = case drop (n-1) bound of
    (x:_) -> Term.Var x
    [] -> Term.Var (free !! (n - length bound - 1))
  go [] _ (Abs _) = error "fromDeBruijn: the impossible happened!"
  go (fresh:other) bound (Abs m) =
    Term.Abs fresh (go other (fresh:bound) m)
  go unused bound (App m n) =
    Term.App (go unused bound m) (go unused bound n)

  -- infinite list of strings, "a" : "b" : ... : "z" : "aa" : "ab" : ...
  infinitevars = filter (`notElem` free) $ concat $ iterate (\ss -> [ c:s | c <- ['a'..'z'], s <- ss ]) [ [c] | c <- ['a'..'z'] ]

-- | Formats a 'Term' into a human-readable string.
--
-- TODO remove redundant parens
formatTerm :: Term -> String
formatTerm (Var x) = show x
formatTerm (Abs m) = "(\\ " <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

-- | Counts a number of sub-terms in a 'Term'.
countTerm :: Term -> Int
countTerm (Var _) = 1
countTerm (Abs m) = 1 + countTerm m
countTerm (App m n) = 1 + countTerm m + countTerm n

-- | Is this 'Term' closed (i.e. has no free variables)?
--
-- TODO consider implementing this with the lenses (bench)
isClosed :: Term -> Bool
isClosed = go 0
  where
  go !bound (Var x) = x <= bound
  go !bound (Abs m) = go (bound+1) m
  go !bound (App m n) = go bound m && go bound n

-- | @linear m@ is a non-empty list whose elements are the sub-terms of @m@
-- traversed in depth-first, pre-order.
--
-- The first element is always @m@. (TODO add a test)
--
-- The following law holds:
--
-- @
-- length ('linear' m) == 'countTerm' m
-- @
linear :: Term -> NonEmpty Term
linear m = m :| case m of
  Var _ -> []
  Abs n -> NE.toList $ linear n
  App n1 n2 -> NE.toList $ linear n1 <> linear n2

-- | @'toList' == NonEmpty.'NE.toList' . 'linear'@
toList :: Term -> [Term]
toList = NE.toList . linear

-- | @index i m@ traverses @m@ to find a sub-term.
--
-- @m@ is traversed in depth-first, pre-order. @i == 0@ denotes @m@ itself. (TODO add a test)
--
-- @
-- index 0 m == Just m
-- index 3 ('App' ('App' ('Var' x) m) n) == Just m
-- @
--
-- Another equivalence:
--
-- @
-- 'toList' m !! i == fromJust ('index' i m)
-- @
--
-- TODO add test
index :: Int -> Term -> Maybe Term
index i m = at i (toList m)

-- | An 'ix' for 'Term' with an additional info. (See 'BoundTerm')
ixBound :: Int -> Traversal Term Term BoundTerm Term
ixBound = loop 0
  where
  loop :: Applicative f => Int -> Int -> (BoundTerm -> f Term) -> Term -> f Term
  loop boundNum i f m
    | i == 0 = f (BoundTerm{boundTerm = m, boundNum})
    | i < 0 = pure m
    | i >= countTerm m = pure m
    | otherwise = case m of
        Var _ -> pure m
        Abs n -> Abs <$> loop (boundNum + 1) (i-1) f n
        App n1 n2 -> App <$> loop boundNum (i-1) f n1 <*> loop boundNum (i-1-(countTerm n1)) f n2

-- | Generates a 'Term' with a specified number of free variables.
--
-- The size parameter of 'Gen' is used as an average of a number of sub-terms
-- in a term. Note that there is no upper limit of a size of a generated term;
-- although rare, a huge term may be generated.
--
-- @genTerm 0@ always generates a closed term in a form of an @('Abs' _)@.
genTerm :: Int -> Gen Term
genTerm freeNum = do
  -- see LambdaCalculus.Term.genTerm for explanation of the probability
  size <- max 1 <$> Q.getSize
  if freeNum >= 1
    then do
      let p = 10000 * (size + 2) `div` (3 * size)
          q = (10000 - p) `div` 2
      Q.frequency [(p, genVar), (q, genAbs), (q, genApp)]
    else
      genAbs
  where
  genVar = Var <$> Q.choose (1, freeNum)
  genAbs = Abs <$> genTerm (freeNum+1)
  genApp = App <$> genTerm freeNum <*> genTerm freeNum

-- | Generates a modified 'Term'.
--
-- Picks a random sub-term and replaces it with a fresh one.
genModifiedTerm :: Int -> Term -> Gen Term
genModifiedTerm freeNum m = do
  i <- Q.choose (0, countTerm m - 1)
  flip (ixBound i) m $ \BoundTerm{boundNum} -> genTerm $ boundNum + freeNum

-- | A closed lambda term. This assumption allows more type instances to be defined.
newtype ClosedTerm = ClosedTerm { unClosedTerm :: Term }
  deriving (Eq, Generic, NFData, Show)

instance Arbitrary ClosedTerm where
  arbitrary = ClosedTerm <$> genTerm 0

instance Ixed ClosedTerm where
  ix :: Int -> Traversal' ClosedTerm Term
  ix i f = fmap ClosedTerm . ix i f . unClosedTerm

instance Genetic ClosedTerm where
  genChildren p12@(ClosedTerm parent1, ClosedTerm parent2) = do
    i1 <- Q.choose (0, countTerm parent1 - 1)
    i2 <- Q.choose (0, countTerm parent2 - 1)
    let sub1 = preview (ixBound' i1) parent1
        sub2 = preview (ixBound' i2) parent2
        child1 = maybe id (set (ix i1) . boundTerm) sub2 $ parent1
        child2 = maybe id (set (ix i2) . boundTerm) sub1 $ parent2
    -- retry if not swappable TODO implement without retrying
    if fromMaybe False $ swappable <$> sub1 <*> sub2
      then pure (ClosedTerm child1, ClosedTerm child2)
      else genChildren p12
    where
    ixBound' :: Int -> Traversal' Term BoundTerm
    ixBound' i f = ixBound i (fmap boundTerm . f)
    -- for terms to be closed after swapping, the number of free variables in
    -- swapped sub-terms must not increase.
    -- the certain set of inequal equations reduces to this simple one.
    swappable :: BoundTerm -> BoundTerm -> Bool
    swappable m n = boundNum m == boundNum n

  genMutant = fmap ClosedTerm . genModifiedTerm 0 . unClosedTerm

type instance Index ClosedTerm = Int
type instance IxValue ClosedTerm = Term

-- | Performs a substitution.
--
-- The list must be infinite. TODO add a newtype
--
-- You would like to use 'reduceBeta' instead of using this directly.
substitute :: [Term] -> Term -> Term
substitute s (Var x) = s !! (x-1)
substitute s (App m n) = App (substitute s m) (substitute s n)
substitute s (Abs m) = Abs (substitute (Var 1 : map (\i -> substitute s' (Var i)) [1..]) m)
  where
  s' = map shift s
  shift = substitute (map Var [2..])

-- | Performs a beta-reduction.
reduceBeta :: Term -> Term
reduceBeta (App (Abs m) n) = substitute (n:map Var [1..]) m
reduceBeta m = m

-- | @reduceStep m@ tries to reduce a beta-redux one step.
--
-- If @m@ can't be reduced any more, returns @Nothing@.
reduceStep :: Term -> Maybe Term
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

{-# DEPRECATED genChurchNumber "Use encodeChurchNumber" #-}
-- |
genChurchNumber :: Gen Term
genChurchNumber = Abs . Abs <$> genTerm 2

-- | Encodes a natural number into a Church numeral.
encodeChurchNumber :: Natural -> Term
encodeChurchNumber n = Abs $ Abs $ iterate (App (Var 2)) (Var 1) !! fromIntegral n

-- | Interprets a lambda term as a Church pair.
--
-- The argument can be a redux. Always returns reduxes.
interpretChurchPair :: Term -> (Term, Term)
interpretChurchPair m =
  ( App m (Abs (Abs (Var 2)))
  , App m (Abs (Abs (Var 1)))
  )
