module Control.Cache where

import Prelude

import Control.Monad.State (StateT(..), evalStateT, execStateT, get, modify, put, runStateT)
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Tuple (Tuple(..), fst, snd)


newtype Cache m k v a = Cache ((HashMap k v) -> m (Tuple a (HashMap k a)))

runCache :: forall m k v a. Cache m k v a → (HashMap k v) → m (Tuple a (HashMap k a))
runCache (Cache x) = x

evalCache :: forall m k v a. Functor m ⇒ Cache m k v a → (HashMap k v) → m a
evalCache (Cache f) s = fst <$> f s

execCache :: forall m k v a. Functor m ⇒ Cache m k v a → (HashMap k v) → m (HashMap k a)
execCache (Cache f) s = snd <$> f s

instance semigroupoidCache :: Monad m => Semigroupoid (Cache m k) where
    compose :: forall b c d. Cache m k c d -> Cache m k b c -> Cache m k b d
    compose (Cache fcd) (Cache fbc) = Cache \b -> fbc b >>= case _ of Tuple _ mc -> fcd mc

instance functorCache :: (Applicative m, Hashable k) => Functor (Cache m k v) where
    map f (Cache statef) = 
        Cache (\s -> map (\(Tuple b s') -> Tuple (f b) (map f s')) (statef s))

-- instance applyCache :: (Applicative m, Hashable k) => Apply (Cache m k v) where
--     apply = ap

-- instance applicativeCache :: (Applicative m, Hashable k) => Applicative (Cache m k v) where
--     pure x = Cache \s -> pure $ Tuple x s

-- instance bindCache :: (Applicative m, Hashable k, Eq v) => Bind (Cache m k v) where
--     bind :: forall a b. Cache m k v a -> (a -> Cache m k v b) -> Cache m k v b
--     bind (Cache fstate) f = Cache \s ->
--         fstate s >>= \(Tuple v s') -> case f v of Cache st -> st (map f s')

-- instance monadCache :: (Applicative m, Hashable k, Eq v) => Monad (Cache m k v)
