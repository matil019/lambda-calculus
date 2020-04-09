module LambdaCalculus.SimplyTyped.HindleyMilner where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.State.Strict (State, evalState)
import Data.List (foldl')
import LambdaCalculus.SimplyTyped.HindleyMilner.MGU (mgu)
import LambdaCalculus.SimplyTyped.HindleyMilner.Term -- TODO no all-in import
import LambdaCalculus.SimplyTyped.HindleyMilner.Types -- TODO no all-in import
import LambdaCalculus.Utils (at)

import qualified Control.Monad.Trans.State.Strict as State
import qualified LambdaCalculus.SimplyTyped.HindleyMilner.MGU as MGU

-- since this is the simply typed lambda calculus,
-- let polymorphism is not used. equivalently, polytypes never appear in the result.
-- however, indeterminate monotypes may appear. for example, @\x. x@ infers @a -> a@,
-- where @a@ is some monotype decided by "the user".
-- this is the Rank-1 type.

-- | A counter to generate fresh variables
type Counter = Int

substMonoType :: VarType -> MonoType -> MonoType -> MonoType
substMonoType = MGU.subst

-- | TODO reuse ordinary @substitute@ functions for Terms?
substPolyType :: VarType -> MonoType -> PolyType -> PolyType
substPolyType x m = go []
  where
  go bound t | x `elem` bound = t
  go _ (Mono t) = Mono $ substMonoType x m t
  go bound (ForAll a t) = ForAll a $ go (a:bound) t

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
subst = \(Subst ss) t0 -> foldl' (flip subst1) t0 ss
  where
  subst1 :: (VarType, MonoType) -> MonoType -> MonoType
  subst1 (a, t) (VarType b)
    | a == b    = t
    | otherwise = VarType b
  subst1 _ (ConstType c) = ConstType c
  subst1 s (t :-> t') = subst1 s t :-> subst1 s t'

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

-- | The implementation of 'infer'.
--
-- Implemented by the Algorithm W.
infer' :: [PolyType] -> Term -> MaybeT (State Counter) (MonoType, Subst)
infer' ctx (Var x) = do
  s <- liftMaybe $ at (x-1) ctx
  t <- lift $ inst s
  pure (t, mempty)
infer' _ (Const t _) = pure (t, mempty)
infer' ctx (App e0 e1) = do
  (t0, s0) <- infer' ctx e0
  (t1, s1) <- infer' ctx e1
  t' <- lift newvar
  s2 <- liftMaybe $ mgu (subst s1 t0) (t1 :-> t')
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
