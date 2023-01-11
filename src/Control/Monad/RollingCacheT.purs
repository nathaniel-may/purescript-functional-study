module Control.Monad.RollingCacheT where

import Prelude

import Control.Monad.RollingCache.Class (class MonadRollingCache)
import Control.Monad.State (class MonadTrans, StateT(..), evalStateT, execStateT, lift, runStateT)
import Data.Hashable (class Hashable)
import Data.RollingCache (RollingCache)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)


newtype RollingCacheT k v m a = RollingCacheT (StateT (RollingCache k v) m a)

instance functorRollingCacheT :: (Applicative m, Hashable k) => Functor (RollingCacheT k v m) where
    map f (RollingCacheT x) = RollingCacheT (map f x)

instance applyRollingCacheT :: (Monad m, Hashable k) => Apply (RollingCacheT k v m) where
    apply = ap

instance applicativeRollingCacheT :: (Monad m, Hashable k) => Applicative (RollingCacheT k v m) where
    pure x = RollingCacheT (StateT \s -> pure $ Tuple x s)

instance bindRollingCacheT :: (Monad m, Hashable k) => Bind (RollingCacheT k v m) where
    bind :: forall a b. RollingCacheT k v m a -> (a -> RollingCacheT k v m b) -> RollingCacheT k v m b
    bind (RollingCacheT (StateT x)) f = RollingCacheT <<< StateT $ \s ->
        x s >>= \(Tuple v s') -> case f v of RollingCacheT (StateT st) -> st s'

instance monadRollingCacheT :: (Monad m, Hashable k) => Monad (RollingCacheT k v m)

derive newtype instance monadTransRollingCacheT :: MonadTrans (RollingCacheT k v)

instance monadRollingCacheRollingCacheT :: (Hashable k, Monad m) => MonadRollingCache k v (RollingCacheT k v m) where
    cache f = RollingCacheT (StateT $ pure <<< f)

instance monadEffectRollingCacheT :: (Hashable k, MonadEffect m) => MonadEffect (RollingCacheT k v m) where
    liftEffect = lift <<< liftEffect

instance monadAffRollingCacheT :: (Hashable k, MonadAff m) => MonadAff (RollingCacheT k v m) where
    liftAff = lift <<< liftAff

runRollingCacheT :: forall m k v a. Functor m => RollingCacheT k v m a -> RollingCache k v -> m (Tuple a (RollingCache k v))
runRollingCacheT (RollingCacheT x) = runStateT x

evalRollingCacheT :: forall m k v a. Functor m => RollingCacheT k v m a -> RollingCache k v -> m a
evalRollingCacheT (RollingCacheT x) = evalStateT x

execRollingCacheT :: forall m k v a. Functor m => RollingCacheT k v m a -> RollingCache k v -> m (RollingCache k v)
execRollingCacheT (RollingCacheT x) = execStateT x
