{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module LambdaCalculus.Genetic where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.Monad (replicateM)
import Test.QuickCheck (Gen)

import qualified Test.QuickCheck as Q

class Genetic a where
  genChildren :: (a, a) -> Gen (a, a)
  genMutant :: a -> Gen a

data Individual a = Individual
  { individual :: a
  , score :: Int
  }

newGeneration :: Genetic a => [Individual a] -> Gen [a]
newGeneration parents = do
  let population = length parents
      breedPcts  = 50
      mutantPcts = 10
  children <- fmap (concatMap (\(a, b) -> [a, b]))
    $ replicateM (population * breedPcts `div` 100 `div` 2)
    $ do parent1 <- chooseParent parents
         parent2 <- chooseParent parents
         genChildren (parent1, parent2)
  mutants <- replicateM (population * mutantPcts `div` 100)
    $ do parent <- chooseParent parents
         genMutant parent
  pure $ children <> mutants

chooseParent :: [Individual a] -> Gen a
chooseParent = Q.frequency . map (\Individual{individual, score} -> (score, pure individual))
