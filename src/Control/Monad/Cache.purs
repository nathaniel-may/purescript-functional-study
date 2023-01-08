module Control.Monad.Cache where

import Prelude

import Control.Monad.Cache.Class (class MonadCache)
import Control.Monad.State (StateT(..), evalStateT, execStateT, runStateT)
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Tuple (Tuple(..))


-- TODO Add cache size so its not always infinite
-- TODO rename CacheT and make Cache type with Identity
newtype Cache m k v a = Cache (StateT (HashMap k v) m a)

instance semigroupCache :: (Hashable k, Monad m, Semigroup a) => Semigroup (Cache m k v a) where
    append (Cache (StateT fx)) (Cache (StateT fy)) = Cache (StateT \s -> do
        Tuple x s' <- fx s
        Tuple x' s'' <- fy s'
        -- union keeps the value from the left in the event of duplicates
        pure $ Tuple (x <> x') (M.union s'' s'))

instance functorCache :: (Applicative m, Hashable k) => Functor (Cache m k v) where
    map f (Cache x) = Cache (map f x)

instance applyCache :: (Monad m, Hashable k) => Apply (Cache m k v) where
    apply = ap

instance applicativeCache :: (Monad m, Hashable k) => Applicative (Cache m k v) where
    pure x = Cache (StateT \s -> pure $ Tuple x s)

instance bindCache :: (Monad m, Hashable k) => Bind (Cache m k v) where
    bind :: forall a b. Cache m k v a -> (a -> Cache m k v b) -> Cache m k v b
    bind (Cache (StateT fs)) f = Cache (StateT \s -> do
        Tuple x m <- fs s
        let (Cache (StateT fs')) = (f x)
        Tuple y m' <- fs' s
        pure $ Tuple y (M.union m' m))

instance monadCache :: (Monad m, Hashable k) => Monad (Cache m k v)

instance monadCacheCache :: (Hashable k, Monad m) => MonadCache (Cache m k v) k v where
    cache f = Cache (StateT $ pure <<< f)

empty :: forall m k v. Applicative m => Cache m k v Unit
empty = Cache (StateT \_ -> pure $ Tuple unit M.empty)

runCache :: forall m k v a. Cache m k v a → (HashMap k v) → m (Tuple a (HashMap k v))
runCache (Cache x) = runStateT x

evalCache :: forall m k v a. Functor m ⇒ Cache m k v a → (HashMap k v) → m a
evalCache (Cache x) = evalStateT x

execCache :: forall m k v a. Functor m ⇒ Cache m k v a → (HashMap k v) → m (HashMap k v)
execCache (Cache x) = execStateT x
