{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaCalculus.SimplyTyped.HindleyMilner.TypesSpec where

import Control.Lens (Lens', Traversal', set, view)
import Data.Functor.Compose (Compose(Compose), getCompose)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck (Arbitrary, Blind(Blind), CoArbitrary)

traversalLawSpec :: (Eq s, Show s, Arbitrary s, Arbitrary a, CoArbitrary a) => Traversal' s a -> Spec
traversalLawSpec l = do
  -- we use Maybe as an example of Applicative
  prop "l pure == pure" $ \a ->
    l Just a `shouldBe` Just a

  -- we use [] as an example of Functor (because it's easiest to just write @map@)
  modifyMaxSize (`div` 5) $
    prop "fmap (l f) . l g == getCompose . l (Compose . fmap f . g)" $ \a (Blind (f :: a -> [a])) (Blind g) ->
      (map (l f) . l g) a `shouldBe` (getCompose . l (Compose . map f . g)) a

lensLawSpec :: (Eq s, Show s, Arbitrary s, Eq a, Show a, Arbitrary a, CoArbitrary a) => Lens' s a -> Spec
lensLawSpec l = do
  prop "view l (set l v s) == v" $ \v s ->
    view l (set l v s) `shouldBe` v

  prop "set l (view l s) == s" $ \s ->
    set l (view l s) s `shouldBe` s

  prop "set l v' (set l v s) == set l v' s" $ \v v' s ->
    set l v' (set l v s) `shouldBe` set l v' s

  traversalLawSpec l

spec :: Spec
spec = do
  describe "boundVars" $ do
    prop "set boundVars [] should remove all bindings" $ \s ->
      set boundVars [] s `shouldBe` Mono (view topMono s)

    describe "should follow the lens law" $
      lensLawSpec boundVars
