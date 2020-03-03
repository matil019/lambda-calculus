{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Genetic where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Control.Lens (Index, IxValue, Ixed, ix, preview, set)
import Control.Monad (replicateM)
import Test.QuickCheck (Arbitrary, Gen, arbitrary)

import qualified Test.QuickCheck as Q

class Ixed a => ChooseIxed a where
  chooseIx :: a -> Gen (Index a)

  genModified :: a -> Gen a

  default genModified :: Arbitrary (IxValue a) => a -> Gen a
  genModified original = do
    i <- chooseIx original
    sub <- arbitrary
    pure $ set (ix i) sub original

data Individual a = Individual
  { individual :: a
  , score :: Int
  }

newGeneration :: ChooseIxed a => [Individual a] -> Gen [a]
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

genChildren :: ChooseIxed a => (a, a) -> Gen (a, a)
genChildren (parent1, parent2) = do
  i1 <- chooseIx parent1
  i2 <- chooseIx parent2
  let sub1 = preview (ix i1) parent1
      sub2 = preview (ix i2) parent2
      child1 = maybe id (set (ix i1)) sub2 $ parent1
      child2 = maybe id (set (ix i2)) sub1 $ parent2
  pure (child1, child2)

genMutant :: ChooseIxed a => a -> Gen a
genMutant = genModified
