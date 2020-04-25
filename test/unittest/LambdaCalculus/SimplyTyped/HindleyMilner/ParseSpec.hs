module LambdaCalculus.SimplyTyped.HindleyMilner.ParseSpec where

import Control.Lens (transformM)
import Data.Char (isUpper, toLower, toUpper)
import LambdaCalculus.SimplyTyped.HindleyMilner.Parse
-- import LambdaCalculus.SimplyTyped.HindleyMilner.Term (formatTerm)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types (MonoType(VarType, ConstType), formatMonoType)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import LambdaCalculus.Utils (isSimpleIdent)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary)

import qualified Test.QuickCheck as Q

newtype ParsableMonoType = PMT MonoType
  deriving (Eq, Show)

instance Arbitrary ParsableMonoType where
  arbitrary = do
    t <- arbitrary
    fmap PMT $ transformM go t
    where
    go (VarType _) = do
      -- isSimpleIdent guarantees that @x@ is non-empty
      x <- genApproxIdent `Q.suchThat` isSimpleIdent
      pure $ VarType (toLower (head x) : tail x)
    go (ConstType _) = do
      x <- genApproxIdent `Q.suchThat` (\x -> isSimpleIdent x && isUpper (head x)) -- exclude a leading underscore
      pure $ ConstType (toUpper (head x) : tail x)
    go t = pure t
    -- generates a mostly valid identifier
    -- the intent is to generate a non-empty string consisting of alphabets/numbers/underscores
    -- but I'm too lazy to implement an exact generator for it
    genApproxIdent = Q.listOf1 $ Q.chooseEnum ('0', 'z')

spec :: Spec
spec = do
  -- prop "parseTerm . formatTerm == pure" $ \t ->
  --   (parseTerm . formatTerm) t `shouldBe` Just t

  prop "parseMonoType . formatMonoType == pure" $ \(PMT t) ->
    (parseMonoType . formatMonoType) t `shouldBe` Just t
