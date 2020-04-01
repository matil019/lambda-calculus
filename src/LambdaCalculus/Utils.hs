-- | Miscellaneous utilities.
module LambdaCalculus.Utils where

-- | A safe '(!!)'.
at :: Int -> [a] -> Maybe a
at i xs
  | i < 0 = Nothing
  | (x:_) <- drop i xs = Just x
  | otherwise = Nothing
