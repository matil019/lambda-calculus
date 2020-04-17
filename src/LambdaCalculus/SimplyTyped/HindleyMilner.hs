{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Infers the principal type of a 'Term' with Hindley-Milner type inference.
--
-- The main interface is 'infer' and 'check'.
module LambdaCalculus.SimplyTyped.HindleyMilner where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.State.Strict (State, evalState)
import Data.List (foldl', intersect)
import Data.Maybe (isJust)
import LambdaCalculus.SimplyTyped.HindleyMilner.MGU (mgu)
import LambdaCalculus.SimplyTyped.HindleyMilner.Term (Term(Var, Abs, App, Const))
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
  ( MonoType((:->), VarType)
  , PolyType(Mono, ForAll)
  , VarType
  )
import LambdaCalculus.Utils (at)

import qualified Control.Monad.Trans.State.Strict as State
import qualified LambdaCalculus.SimplyTyped.HindleyMilner.MGU as MGU

-- since this is the simply typed lambda calculus,
-- let polymorphism is not used. equivalently, polytypes never appear in the result.
-- however, indeterminate monotypes may appear. for example, @\x. x@ infers @a -> a@,
-- where @a@ is some monotype decided by "the user".
-- this is the Rank-1 type.

-- | A counter to generate fresh distinct variables
type Counter = Int

-- | @substMonoType x m t@ substitutes all occurences of @x@ in @t@ by @m@.
substMonoType :: VarType -> MonoType -> MonoType -> MonoType
substMonoType = MGU.subst

-- | @substPolyType x m s@ substitutes all occurences of @x@ in @s@ by @m@ excluding bound variables.
substPolyType :: VarType -> MonoType -> PolyType -> PolyType
substPolyType x m = go []
  where
  go bound t | x `elem` bound = t
  go _ (Mono t) = Mono $ substMonoType x m t
  go bound (ForAll a t) = ForAll a $ go (a:bound) t

-- | A list of substitutions of a type variable with a mono type.
--
-- Substitutions are applied from the first element of the list to the last.
--
-- Together with 'subst', the 'Monoid' instance obeys the following law:
--
-- @
-- subst (a <> b) ≡ subst a . subst b
-- subst mempty ≡ id
-- @
newtype Subst = Subst [(VarType, MonoType)]
  deriving (Eq, Show)
  deriving newtype (Monoid, NFData)

-- | @a <> b@ merges substitutions in the same way as function composition.
--
-- In other words, @'subst' (a <> b) == 'subst' a . 'subst' b@.
--
-- This means the wrapped lists are concatenated in the /reverse order/.
instance Semigroup Subst where
  Subst a <> Subst b = Subst (b <> a)

-- | Applies 'substPolyType' to each 'PolyType'.
substCtx :: Subst -> [PolyType] -> [PolyType]
substCtx (Subst ss) = map $ \t0 -> foldl' (flip $ uncurry substPolyType) t0 ss

-- | Instantiates a 'MonoType' from a 'PolyType'.
--
-- Strips 'ForAll's and substitutes bound variables with fresh variables.
inst :: PolyType -> State Counter MonoType
inst (Mono t) = pure t
inst (ForAll a t) = do
  x <- newvar
  inst $ substPolyType a x t

-- | Comes up with a fresh variable.
newvar :: State Counter MonoType
newvar = State.state $ \counter -> (VarType $ "a" <> show counter, counter + 1)

-- | Applies a set of substitutions to a mono type.
subst :: Subst -> MonoType -> MonoType
subst (Subst ss) t0 = foldl' (flip $ uncurry substMonoType) t0 ss

-- | Lifts a 'Maybe' to a 'MaybeT'.
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

-- | The implementation of 'infer'.
--
-- Implemented using the Algorithm W.
infer' :: [PolyType] -> Term -> MaybeT (State Counter) (MonoType, Subst)
infer' ctx (Var x) = do
  s <- liftMaybe $ at (x-1) ctx
  t <- lift $ inst s
  pure (t, mempty)
infer' _ (Const t _) = pure (t, mempty)
infer' ctx (App e0 e1) = do
  (t0, s0) <- infer' ctx e0
  (t1, s1) <- infer' (substCtx s0 ctx) e1
  t' <- lift newvar
  s2 <- fmap Subst $ liftMaybe $ mgu (subst s1 t0) (t1 :-> t')
  pure $ (subst s2 t', s2 <> s1 <> s0)
infer' ctx (Abs e) = do
  t <- lift newvar
  (t', s) <- infer' (Mono t:ctx) e
  pure $ (subst s $ t :-> t', s)
-- there is no let polymorphism

-- | Infers the principal type of a term.
infer :: Term -> Maybe MonoType
infer = fmap fst . flip evalState 0 . runMaybeT . infer' freeVarTypes
  where
  -- this is needed to allow types of free variables to be inferred;
  -- if it were @[]@, @infer (Var 1)@ would be @Nothing@, but thanks to this,
  -- @infer (Var 1)@ is @Just (VarType "a0")@ as expected.
  freeVarTypes = repeat $ ForAll "a" (Mono (VarType "a"))

-- | Checks if two types are compatible (aka unify).
--
-- You may want to apply 'quantify' before 'check' if you wish to compare
-- 'MonoType's.
-- TODO take MonoTypes instead of PolyTypes
check :: PolyType -> PolyType -> Bool
check pa pb = flip evalState 0 $ do
  ma <- inst pa
  mb <- inst pb
  pure $ isJust $ mgu ma mb

-- | Quantifies a 'MonoType' over a set of v'VarType's.
quantifySome :: [VarType] -> MonoType -> PolyType
quantifySome vt mt = foldr ForAll (Mono mt) bound
  where
  bound = MGU.vars mt `intersect` vt

-- | Quantifies a 'MonoType' over all of its v'VarType's.
quantify :: MonoType -> PolyType
quantify t = quantifySome (MGU.vars t) t
