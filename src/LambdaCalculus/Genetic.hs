{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | A general-purpose Genetic Algorithm stuff.
module LambdaCalculus.Genetic where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.Monad (replicateM)
import Test.QuickCheck (Gen)

import qualified Test.QuickCheck as Q

-- | A set of Genetic Algorithm operation.
class Genetic a where
  -- | Performs a crossover between a pair of parents.
  -- TODO rename to genCrossover
  genChildren :: (a, a) -> Gen (a, a)

  -- | Generates a mutant from a parent.
  genMutant :: a -> Gen a

-- | An individual in a population.
data Individual a = Individual
  { individual :: a
  , score :: Int -- TODO rename to weight
  }

-- | Applies crossover and mutation to a generation to generate a new one.
newGeneration :: Genetic a => [Individual a] -> Gen [a]
newGeneration parents = do
  -- TODO take as a parameter
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

-- | Chooses an individual from a population with weights.
chooseParent :: [Individual a] -> Gen a
chooseParent = Q.frequency . map (\Individual{individual, score} -> (score, pure individual))
