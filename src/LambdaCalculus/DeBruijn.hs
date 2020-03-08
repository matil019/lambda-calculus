{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module LambdaCalculus.DeBruijn where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.List (elemIndex)
import GHC.Generics (Generic)

import qualified LambdaCalculus.Term as Term

data Term
  = Var Int        -- ^ A variable (starts at @1@)
  | Abs Term       -- ^ An abstraction
  | App Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

-- | Converts a lambda 'Term.Term' to De Bruijn index 'Term'.
--
-- The second value in the returned tuple is an ordered set of free variables
-- which the first refers.
toDeBruijn :: Term.Term -> (Term, [Term.Var])
toDeBruijn = flip runState [] . go []
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

-- 'Term' is assumed to be closed. TODO remove this assumption
fromDeBruijn :: Term -> Term.ClosedTerm
fromDeBruijn = Term.ClosedTerm . go infinitevars []
  where
  -- @go free bound m@ recursively converts @m@ from DeBruijn notation to the ordinary one.
  --
  -- @free@ is an infinite list of unused free variables.
  -- @bound@ is a list of bound variables. Its first element is bound by the innermost abstraction.
  -- memo: I wrote this in a hope that it works well with non-closed
  -- terms. Since it works with closed terms, I leave this redundancy.
  go free bound (Var n) = case drop (n-1) bound of
    (x:_) -> Term.Var x
    [] -> Term.Var (free !! (n - length bound - 1))
  go free bound (Abs m) =
    let fresh = head free
        other = tail free
    in Term.Abs fresh (go other (fresh:bound) m)
  go free bound (App m n) =
    Term.App (go free bound m) (go free bound n)

  -- infinite list of strings, "a" : "b" : ... : "z" : "aa" : "ab" : ...
  infinitevars = concat $ iterate (\ss -> [ c:s | c <- ['a'..'z'], s <- ss ]) [ [c] | c <- ['a'..'z'] ]

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
