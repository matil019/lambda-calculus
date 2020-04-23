{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances where

import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Test.QuickCheck as Q

-- | The size parameter is used as the expected size of a 'MonoType'.
instance Arbitrary MonoType where
  arbitrary = do
    size <- max 1 <$> Q.getSize
    let p = 10000 * (size + 1) `div` (2 * size)
        q = 10000 - p
    Q.frequency [(p `div` 2, genVar), (p `div` 2, genConst), (q, genMono)]
    where
    -- 1
    genVar = VarType <$> arbitrary
    -- 1
    genConst = ConstType <$> arbitrary
    -- 2X + 1
    genMono = (:->) <$> arbitrary <*> arbitrary
