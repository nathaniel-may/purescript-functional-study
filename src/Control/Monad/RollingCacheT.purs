module Control.Monad.RollingCacheT where

import Prelude

import Control.Monad.RollingCache.Class (class MonadRollingCache)
import Control.Monad.State (StateT(..), evalStateT, execStateT, runStateT)
import Data.Hashable (class Hashable)
import Data.RollingCache (RollingCache)
import Data.RollingCache as Cache
import Data.Tuple (Tuple(..))


newtype RollingCacheT m k v a = RollingCacheT (StateT (RollingCache k v) m a)

instance functorRollingCacheT :: (Applicative m, Hashable k) => Functor (RollingCacheT m k v) where
    map f (RollingCacheT x) = RollingCacheT (map f x)

instance applyRollingCacheT :: (Monad m, Hashable k) => Apply (RollingCacheT m k v) where
    apply = ap

instance applicativeRollingCacheT :: (Monad m, Hashable k) => Applicative (RollingCacheT m k v) where
    pure x = RollingCacheT (StateT \s -> pure $ Tuple x s)

instance bindRollingCacheT :: (Monad m, Hashable k) => Bind (RollingCacheT m k v) where
    bind :: forall a b. RollingCacheT m k v a -> (a -> RollingCacheT m k v b) -> RollingCacheT m k v b
    bind (RollingCacheT (StateT fs)) f = RollingCacheT (StateT \s -> do
        Tuple x c <- fs s
        let (RollingCacheT (StateT fs')) = (f x)
        Tuple y c' <- fs' s
        pure $ Tuple y (Cache.merge c' c))

instance monadRollingCacheT :: (Monad m, Hashable k) => Monad (RollingCacheT m k v)

instance monadRollingCacheRollingCacheT :: (Hashable k, Monad m) => MonadRollingCache (RollingCacheT m k v) k v where
    cache f = RollingCacheT (StateT $ pure <<< f)

runRollingCacheT :: forall m k v a. Functor m => RollingCacheT m k v a -> RollingCache k v -> m (Tuple a (RollingCache k v))
runRollingCacheT (RollingCacheT x) = runStateT x

evalRollingCacheT :: forall m k v a. Functor m => RollingCacheT m k v a -> RollingCache k v -> m a
evalRollingCacheT (RollingCacheT x) = evalStateT x

execRollingCacheT :: forall m k v a. Functor m => RollingCacheT m k v a -> RollingCache k v -> m (RollingCache k v)
execRollingCacheT (RollingCacheT x) = execStateT x
