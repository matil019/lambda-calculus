{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module LambdaCalculus.DeBruijn where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import qualified LambdaCalculus.Term as Term

data Term
  = Var Int        -- ^ A variable
  | Abs Term       -- ^ An abstraction
  | App Term Term  -- ^ An application
  deriving (Eq, Generic, NFData, Show)

toDeBruijn :: Term.Term -> Term
toDeBruijn = go []
  where
  go bound (Term.Var x) = Var (depth bound x)
  go bound (Term.Abs x m) = Abs (go (x:bound) m)
  go bound (Term.App m n) = App (go bound m) (go bound n)

  depth bound x = (+1) $ length $ takeWhile (/= x) bound

fromDeBruijn :: Term -> Term.Term
fromDeBruijn = go infinitevars []
  where
  -- @go free bound m@ recursively converts @m@ from DeBruijn notation to the ordinary one.
  --
  -- @free@ is an infinite list of unused free variables.
  -- @bound@ is a list of bound variables. Its first element is bound by the innermost abstraction.
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
