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

instance functorCache :: (Applicative m, Hashable k) => Functor (Cache m k v) where
    map f (Cache statef) = 
        Cache (\s -> map (\(Tuple b s') -> Tuple (f b) (map f s')) (statef s))

-- See FlexStateT for why this cannot be an Applicative nor a Monad