module Data.Evictor where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)

-- TODO name this better
-- | a data type for keeping track of a fixed number of elements, 
-- | evicting the oldest touched when necessary and its processesing effects on eviction
data Evictor m a = Evictor Int (Array a) (a -> m Unit)

-- | remove the last touched element from the Evictor and process its effects
pop :: forall m a. Applicative m => Evictor m a -> m (Evictor m a)
pop (Evictor max xs f) = fromMaybe (pure $ Evictor max [] f) do
    x <- Array.last xs
    let xs' = Array.dropEnd 1 xs
    pure $ f x $> Evictor max xs' f

-- | remove the last touched elements and process their effects until the Evictor is
-- | within its maximum size
shrink :: forall m a. Monad m => Evictor m a -> m (Evictor m a)
shrink input@(Evictor max xs _)
    | Array.length xs <= max = pure input
    | otherwise = pop input >>= shrink

-- | add an element to the Evictor if it does not exist or update its position to
-- | the most recently touched. Process the effects of the last element if the maximum size is exceeded
touch :: forall m a. Eq a => Monad m => a -> Evictor m a -> m (Evictor m a)
touch x (Evictor max xs f) =
    let xs' = Array.cons x <<< Array.delete x $ xs
    in shrink (Evictor max xs' f)
