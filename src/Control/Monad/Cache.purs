module Control.Monad.Cache where

import Prelude

import Control.Monad.Cache.Class (class MonadCache)
import Control.Monad.State (StateT(..), evalStateT, execStateT, runStateT)
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Tuple (Tuple(..))


-- TODO Add cache size so its not always infinite
-- TODO make Cache type with Identity
newtype CacheT m k v a = CacheT (StateT (HashMap k v) m a)

instance semigroupCacheT :: (Hashable k, Monad m, Semigroup a) => Semigroup (CacheT m k v a) where
    append (CacheT (StateT fx)) (CacheT (StateT fy)) = CacheT (StateT \s -> do
        Tuple x s' <- fx s
        Tuple x' s'' <- fy s'
        -- union keeps the value from the left in the event of duplicates
        pure $ Tuple (x <> x') (M.union s'' s'))

instance functorCacheT :: (Applicative m, Hashable k) => Functor (CacheT m k v) where
    map f (CacheT x) = CacheT (map f x)

instance applyCacheT :: (Monad m, Hashable k) => Apply (CacheT m k v) where
    apply = ap

instance applicativeCacheT :: (Monad m, Hashable k) => Applicative (CacheT m k v) where
    pure x = CacheT (StateT \s -> pure $ Tuple x s)

instance bindCacheT :: (Monad m, Hashable k) => Bind (CacheT m k v) where
    bind :: forall a b. CacheT m k v a -> (a -> CacheT m k v b) -> CacheT m k v b
    bind (CacheT (StateT fs)) f = CacheT (StateT \s -> do
        Tuple x m <- fs s
        let (CacheT (StateT fs')) = (f x)
        Tuple y m' <- fs' s
        pure $ Tuple y (M.union m' m))

instance monadCacheT :: (Monad m, Hashable k) => Monad (CacheT m k v)

instance monadCacheCacheT :: (Hashable k, Monad m) => MonadCache (CacheT m k v) k v where
    cache f = CacheT (StateT $ pure <<< f)

empty :: forall m k v. Applicative m => CacheT m k v Unit
empty = CacheT (StateT \_ -> pure $ Tuple unit M.empty)

runCacheT :: forall m k v a. CacheT m k v a → (HashMap k v) → m (Tuple a (HashMap k v))
runCacheT (CacheT x) = runStateT x

evalCacheT :: forall m k v a. Functor m ⇒ CacheT m k v a → (HashMap k v) → m a
evalCacheT (CacheT x) = evalStateT x

execCacheT :: forall m k v a. Functor m ⇒ CacheT m k v a → (HashMap k v) → m (HashMap k v)
execCacheT (CacheT x) = execStateT x
