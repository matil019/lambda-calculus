module Main where

import Data.Foldable (traverse_)
import Data.Functor.Const (Const(Const), getConst)
import Data.List (find, unfoldr)
import Data.Monoid (All(All), Any(Any), getAll, getAny)
import Lens.Micro

-- borrowed from "lens"
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll . getConst . l (Const . All . f)

-- borrowed from "lens"
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny . getConst . l (Const . Any . f)

-- borrowed from "extra"
dupe :: a -> (a, a)
dupe a = (a, a)

type Var = Char

data Term
  = Var Var
  | Abs Var Term
  | App Term Term
  deriving (Eq, Show)

freeVars :: Traversal' Term Var
freeVars f = freeVars' (fmap Var . f)

freeVars' :: Traversal Term Term Var Term
freeVars' f = freeVarsCtx' (f . fst)

freeVarsCtx' :: Traversal Term Term (Var, [Var]) Term
freeVarsCtx' f = go []
  where
  go bound (Var x)
    | x `notElem` bound = f (x, bound)
    | otherwise = pure (Var x)
  go bound (Abs x m) = Abs x <$> go (x:bound) m
  go bound (App m n) = App <$> go bound m <*> go bound n

convertAlpha :: Var -> Term -> Term
-- convertAlpha x (Abs y m) = Abs x $ substitute y (Var x) m ?
convertAlpha x (Abs y m) = Abs x $ over freeVars (\z -> if z == y then x else z) m
convertAlpha _ m = m

newFreeVar :: [Var] -> Var
newFreeVar except = case find (`notElem` except) ['a'..'z'] of
  Just ok -> ok
  Nothing -> error "newFreeVar: no vars available"

substitute :: Var -> Term -> Term -> Term
substitute x n (Var y)
  | x == y    = n
  | otherwise = Var y
substitute x n (App m1 m2) = App (substitute x n m1) (substitute x n m2)
substitute x n (Abs y m)
  | x == y = Abs y m
  | x /= y && allOf freeVars (y /=) n = Abs y (substitute x n m)
  -- TODO not needed to recurse substitute again, but for that it needs a distinct @Abs@ type
  | otherwise = substitute x n $ convertAlpha (newFreeVar (x : toListOf freeVars n)) (Abs y m)

-- | Performs beta-reduction.
--
-- Automatically does alpha-conversions if needed.
reduceBeta :: Term -> Term
reduceBeta (App (Abs x m) n) = substitute x n m
reduceBeta m = m

reduceStep :: Term -> Maybe Term
reduceStep (Var _) = Nothing
reduceStep (Abs _ _) = Nothing
reduceStep m@(App (Abs _ _) _) = Just $ reduceBeta m
reduceStep (App m n) = (\m' -> App m' n) <$> reduceStep m

formatTerm :: Term -> String
formatTerm (Var x) = [x]
formatTerm (Abs x m) = "(\\" <> [x] <> "." <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

interpretChurchNumber :: Term -> Maybe Int
interpretChurchNumber = \m -> go $ App (App m (Var '+')) (Var '0')
  where
  go m = case reduce m of
    Var '0' -> Just 0
    App (Var '+') n -> fmap (1+) $ go n
    _ -> Nothing

  reduce m = case unfoldr (fmap dupe . reduceStep) m of
    [] -> m
    xs -> last xs

main :: IO ()
main = do
  let -- Church encodings of integers
      one = Abs 'f' $ Abs 'x' $ App (Var 'f') (Var 'x')
      two = Abs 'f' $ Abs 'x' $ App (Var 'f') $ App (Var 'f') (Var 'x')
      -- \g. \h. \f. \x. (g f) ((h f) x)
      plus = Abs 'g' $ Abs 'h' $ Abs 'f' $ Abs 'x' $ App (App (Var 'g') (Var 'f')) (App (App (Var 'h') (Var 'f')) (Var 'x'))
  traverse_ (putStrLn . formatTerm) $ steps $ App plus one
  putStrLn "------------------------------------------------------------------------"
  traverse_ (putStrLn . formatTerm) $ steps $ App (App plus one) two
  putStrLn "------------------------------------------------------------------------"
  traverse_ (putStrLn . formatTerm) $ steps $ App (App plus one) (App (App plus two) two)
  putStrLn "------------------------------------------------------------------------"
  traverse_ (putStrLn . formatTerm) $ steps $ App (App plus (App (App plus one) one)) one
  putStrLn "------------------------------------------------------------------------"
  traverse_ print $ interpretChurchNumber $ App (App plus one) two
  traverse_ print $ interpretChurchNumber $ App (App plus one) (App (App plus two) two)
  traverse_ print $ interpretChurchNumber $ App (App plus (App (App plus one) one)) one
  where
  steps m = m : unfoldr (fmap dupe . reduceStep) m
