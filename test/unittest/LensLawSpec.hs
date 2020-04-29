{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LensLawSpec where

import Control.Lens (Iso', Lens', Prism', Traversal', matching, preview, review, set, view)
import Data.Functor.Compose (Compose(Compose), getCompose)
import LambdaCalculus.SimplyTyped.HindleyMilner.Types.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck (Arbitrary, CoArbitrary, Function, pattern Fn)

traversalLawSpec
  :: (Eq s, Show s, Arbitrary s, Show a, Arbitrary a, CoArbitrary a, Function a)
  => Traversal' s a
  -> Spec
traversalLawSpec l = do
  -- we use Maybe as an example of Applicative
  prop "l pure == pure" $ \a ->
    l Just a `shouldBe` Just a

  -- we use [] as an example of Functor (because it's easiest to just write @map@)
  modifyMaxSize (`div` 10) $
    prop "fmap (l f) . l g == getCompose . l (Compose . fmap f . g)" $ \a (Fn (f :: a -> [a])) (Fn g) ->
      (map (l f) . l g) a `shouldBe` (getCompose . l (Compose . map f . g)) a

lensLawSpec
  :: (Eq s, Show s, Arbitrary s, Eq a, Show a, Arbitrary a, CoArbitrary a, Function a)
  => Lens' s a
  -> Spec
lensLawSpec l = do
  prop "view l (set l v s) == v" $ \v s ->
    view l (set l v s) `shouldBe` v

  prop "set l (view l s) == s" $ \s ->
    set l (view l s) s `shouldBe` s

  prop "set l v' (set l v s) == set l v' s" $ \v v' s ->
    set l v' (set l v s) `shouldBe` set l v' s

  traversalLawSpec l

prismLawSpec
  :: (Arbitrary s, Eq s, Show s, Arbitrary a, CoArbitrary a, Eq a, Function a, Show a)
  => Prism' s a
  -> Spec
prismLawSpec l = do
  prismLawOnlySpec l

  traversalLawSpec l

-- | Like 'prismLawSpec', but doesn't call 'traversalLawSpec'.
prismLawOnlySpec
  :: (Arbitrary s, Eq s, Show s, Arbitrary a, Eq a, Show a)
  => Prism' s a
  -> Spec
prismLawOnlySpec l = do
  prop "preview l (review l b) == Just b" $ \b ->
    preview l (review l b) `shouldBe` Just b

  prop "preview l s == Just a ==> review l a == s" $ \s ->
    case preview l s of
      Just a -> review l a `shouldBe` s
      Nothing -> pure ()

  prop "matching l s == Left t ==> matching l t == Left s" $ \s ->
    case matching l s of
      Left t -> matching l t `shouldBe` Left s
      Right _ -> pure () -- using @discard@ causes the test to fail if an @Iso@ was given

isoLawSpec
  :: (Arbitrary s, Eq s, Show s, Arbitrary a, CoArbitrary a, Eq a, Function a, Show a)
  => Iso' s a
  -> Spec
isoLawSpec f = do
  lensLawSpec f
  prismLawOnlySpec f
