module Control.FlexStateT where

import Prelude

import Data.Tuple (Tuple(..), fst, snd)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Utils (mapSnd)


data FlexStateT s s' m a = FlexStateT (s -> m (Tuple a s'))

runFlexStateT :: forall s s' m a. FlexStateT s s' m a → s → m (Tuple a s')
runFlexStateT (FlexStateT x) = x

evalFlexStateT :: forall s s' m a. Functor m ⇒ FlexStateT s s' m a → s → m a
evalFlexStateT (FlexStateT f) s = fst <$> f s

execFlexStateT :: forall s s' m a. Functor m ⇒ FlexStateT s s' m a → s → m s'
execFlexStateT (FlexStateT f) s = snd <$> f s

instance functorFlexStateT :: Functor m => Functor (FlexStateT s s' m) where
    map f (FlexStateT statef) = 
        FlexStateT (\s -> map (\(Tuple b s') -> Tuple (f b) s') (statef s))

instance applyFlexStateT :: (Monoid s', Monad m) => Apply (FlexStateT s s' m) where
    apply = ap

instance applicativeFlexStateT :: (Monoid s', Monad m) => Applicative (FlexStateT s s' m) where
    pure x = FlexStateT (\_ -> pure $ Tuple x mempty)

instance bindFlexStateT :: (Monoid s', Monad m) => Bind (FlexStateT s s' m) where
    bind (FlexStateT fstate) f = FlexStateT \s ->
        fstate s >>= \(Tuple v s') -> case f v of
            FlexStateT st -> map (mapSnd $ append s') (st s)

instance monadFlexStateT :: (Monoid s', Monad m) => Monad (FlexStateT s s' m)

instance arbitraryFlexStateT :: (Arbitrary a, Monad m, Monoid s') => Arbitrary (FlexStateT s s' m a) where
    arbitrary = pure <$> arbitrary
