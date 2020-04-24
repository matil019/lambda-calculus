{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | A general-purpose Genetic Algorithm stuff.
module LambdaCalculus.Genetic where

import Control.Monad (replicateM)
import Numeric.Natural (Natural)
import Test.QuickCheck (Gen)

import qualified Test.QuickCheck as Q

-- | A set of Genetic Algorithm operation.
class Genetic a where
  -- | Performs a crossover between a pair of parents.
  genCrossover :: (a, a) -> Gen (a, a)

  -- | Generates a mutant from a parent.
  genMutant :: a -> Gen a

-- | An individual in a population.
data Individual a = Individual
  { individual :: a
  , weight :: Natural
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
         genCrossover (parent1, parent2)
  mutants <- replicateM (population * mutantPcts `div` 100)
    $ do parent <- chooseParent parents
         genMutant parent
  pure $ children <> mutants

-- | Chooses an individual from a population with weights.
chooseParent :: [Individual a] -> Gen a
chooseParent = Q.frequency . map (\Individual{individual, weight} -> (fromIntegral weight, pure individual))
