module Main where

import Data.Foldable (traverse_)
import Data.Functor.Const (Const(Const), getConst)
import Data.List (find)
import Data.Monoid (All(All), Any(Any), getAll, getAny)
import Lens.Micro

-- borrowed from "lens"
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll . getConst . l (Const . All . f)

-- borrowed from "lens"
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny . getConst . l (Const . Any . f)

type Var = Char

data Abs = Abs Var Term
  deriving (Eq, Show)

data App = App Term Term
  deriving (Eq, Show)

data Term
  = TermVar Var
  | TermAbs Abs
  | TermApp App
  deriving (Eq, Show)

-- | A shorthand for the constructor
var :: Char -> Term
var = TermVar

-- | A shorthand for the constructor
lam :: Var -> Term -> Term
lam x m = TermAbs (Abs x m)

-- | A shorthand for the constructor
app :: Term -> Term -> Term
app m n = TermApp (App m n)

freeVars :: Traversal' Term Var
freeVars f = freeVars' (fmap var . f)

freeVars' :: Traversal Term Term Var Term
freeVars' f = freeVarsCtx' (f . fst)

freeVarsCtx' :: Traversal Term Term (Var, [Var]) Term
freeVarsCtx' f = go []
  where
  go bound (TermVar x)
    | x `notElem` bound = f (x, bound)
    | otherwise = pure (var x)
  go bound (TermAbs (Abs x m)) = lam x <$> go (x:bound) m
  go bound (TermApp (App m n)) = app <$> go bound m <*> go bound n

convertAlpha :: Var -> Term -> Term
-- convertAlpha x (Abs y m) = Abs x $ substitute y (Var x) m ?
convertAlpha x (TermAbs (Abs y m)) = lam x $ over freeVars (\z -> if z == y then x else z) m
convertAlpha _ m = m

newFreeVar :: [Var] -> Var
newFreeVar except = case find (`notElem` except) ['a'..'z'] of
  Just ok -> ok
  Nothing -> error "newFreeVar: no vars available"

substitute :: Var -> Term -> Term -> Term
substitute x n (TermVar y)
  | x == y    = n
  | otherwise = var y
substitute x n (TermApp (App m1 m2)) = app (substitute x n m1) (substitute x n m2)
substitute x n (TermAbs (Abs y m))
  | x == y = lam y m
  | x /= y && allOf freeVars (y /=) n = lam y (substitute x n m)
  -- TODO not needed to recurse substitute again, but for that it needs a distinct @Abs@ type
  | otherwise = substitute x n $ convertAlpha (newFreeVar (x : toListOf freeVars n)) (lam y m)

-- | Performs beta-reduction.
--
-- Automatically does alpha-conversions if needed.
reduceBeta :: Term -> Term
reduceBeta (TermApp (App (TermAbs (Abs x m)) n)) = substitute x n m
reduceBeta m = m

-- | @reduceApp (App m n)@ tries to reduce @App m n@ to non-@App@ form.
reduceApp :: App -> Term
reduceApp (App m n) = reduceBeta $ app (reduceApp m) (reduceApp n)
reduceApp m = m

formatTerm :: Term -> String
formatTerm (TermVar x) = [x]
formatTerm (TermAbs (Abs x m)) = "(\\" <> [x] <> "." <> formatTerm m <> ")"
formatTerm (TermApp (App m n)) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

interpretChurchNumber :: Term -> Maybe Int
interpretChurchNumber m =
  case reduceApp m of
    TermAbs (Abs f (TermAbs (Abs x n'))) ->
      let go (TermVar y) | y == x = Just 0
          go (TermApp (App (TermVar g) n)) | g == f = fmap (1+) . go $ reduceApp n
          go _ = Nothing
      in go $ reduceApp n'
    _ -> Nothing

main :: IO ()
main = do
  let -- Church encodings of integers
      one = lam 'f' $ lam 'x' $ app (var 'f') (var 'x')
      two = lam 'f' $ lam 'x' $ app (var 'f') $ app (var 'f') (var 'x')
      -- \g. \h. \f. \x. (g f) ((h f) x)
      plus = lam 'g' $ lam 'h' $ lam 'f' $ lam 'x' $ app (app (var 'g') (var 'f')) (app (app (var 'h') (var 'f')) (var 'x'))
  putStrLn $ formatTerm $ app plus one
  putStrLn $ formatTerm $ reduceBeta $ app plus one
  putStrLn $ formatTerm $ app (app plus one) two
  putStrLn $ formatTerm $ reduceApp $ App (app plus one) two
  traverse_ print $ interpretChurchNumber $ app (app plus one) two
  traverse_ print $ interpretChurchNumber $ app (app plus one) (app (app plus two) two)
  -- TODO fix this! (should print 3)
  traverse_ print $ interpretChurchNumber $ app (app plus (app (app plus one) one)) one
