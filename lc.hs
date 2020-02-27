import Data.Foldable (traverse_)
import Data.List (find)
import Lens.Micro

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
convertAlpha x (Abs y m) = Abs x $ over freeVars (\z -> if z == y then x else z) m
convertAlpha _ m = m

-- | Performs beta-reduction.
--
-- Automatically does alpha-conversions if needed.
reduceBeta :: Term -> Term
reduceBeta (App (Abs x m) n) =
  let frees = toListOf freeVars n
      foo (y, bound)
        | y /= x = Var y
        | any (`elem` (x:bound)) frees = convertAlpha (newFreeVar bound) n
        | otherwise = n
  in
  over freeVarsCtx' foo m
  where
  newFreeVar bound = case find (`notElem` bound) ['a'..'z'] of
    Just ok -> ok
    Nothing -> error "newFreeVar: no vars available"
reduceBeta m = m

-- | @reduceApp (App m n)@ tries to reduce @App m n@ to non-@App@ form.
reduceApp :: Term -> Term
reduceApp (App m n) = reduceBeta $ App (reduceApp m) (reduceApp n)
reduceApp m = m

formatTerm :: Term -> String
formatTerm (Var x) = [x]
formatTerm (Abs x m) = "(\\" <> [x] <> "." <> formatTerm m <> ")"
formatTerm (App m n) = "(" <> formatTerm m <> " " <> formatTerm n <> ")"

interpretChurchNumber :: Term -> Maybe Int
interpretChurchNumber m =
  case reduceApp m of
    Abs f (Abs x n') ->
      let go (Var y) | y == x = Just 0
          go (App (Var g) n) | g == f = fmap (1+) . go $ reduceApp n
          go _ = Nothing
      in go $ reduceApp n'
    _ -> Nothing

main :: IO ()
main = do
  let -- Church encodings of integers
      one = Abs 'f' $ Abs 'x' $ App (Var 'f') (Var 'x')
      two = Abs 'f' $ Abs 'x' $ App (Var 'f') $ App (Var 'f') (Var 'x')
      -- \g. \h. \f. \x. (g f) ((h f) x)
      plus = Abs 'g' $ Abs 'h' $ Abs 'f' $ Abs 'x' $ App (App (Var 'g') (Var 'f')) (App (App (Var 'h') (Var 'f')) (Var 'x'))
  putStrLn $ formatTerm $ App plus one
  putStrLn $ formatTerm $ reduceBeta $ App plus one
  putStrLn $ formatTerm $ App (App plus one) two
  putStrLn $ formatTerm $ reduceApp $ App (App plus one) two
  traverse_ print $ interpretChurchNumber $ App (App plus one) two
  traverse_ print $ interpretChurchNumber $ App (App plus one) (App (App plus two) two)
  -- TODO fix this! (should print 3)
  traverse_ print $ interpretChurchNumber $ App (App plus (App (App plus one) one)) one
