{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module LambdaCalculus.DeBruijn where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.List (elemIndex)
import Data.Tuple (swap)
import GHC.Generics (Generic)

import qualified LambdaCalculus.Term as Term

data Term
  = Var Int        -- ^ A variable (starts at @1@)
  | Abs Term       -- ^ An abstraction
  | App Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

-- | Converts a lambda 'Term.Term' to De Bruijn index 'Term'.
--
-- The first value in the returned tuple is an ordered set of free variables
-- which the second refers.
toDeBruijn :: Term.Term -> ([Term.Var], Term)
toDeBruijn = swap . flip runState [] . go []
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

-- | The list must be long enough to have all the free variables the 'Term' refers.
--
-- The return value of 'toDeBruijn' can always be applied to 'fromDeBruijn'.
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

-- | The list must be infinite. TODO add a newtype
substitute :: [Term] -> Term -> Term
substitute s (Var x) = s !! (x-1)
substitute s (App m n) = App (substitute s m) (substitute s n)
substitute s (Abs m) = Abs (substitute (Var 1 : map (\i -> substitute s' (Var i)) [1..]) m)
  where
  s' = map shift s
  shift = substitute (map Var [2..])

reduceBeta :: Term -> Term
reduceBeta (App (Abs m) n) = substitute (n:map Var [1..]) m
reduceBeta m = m
