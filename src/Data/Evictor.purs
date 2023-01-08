module Data.Evictor where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)


data Evictor m a = Evictor Int (Array a) (a -> m Unit)

pop :: forall m a. Applicative m => Evictor m a -> m (Evictor m a)
pop (Evictor max xs f) = fromMaybe (pure $ Evictor max [] f) do
    x <- Array.last xs
    let xs' = Array.dropEnd 1 xs
    pure $ f x $> Evictor max xs' f

shrink :: forall m a. Monad m => Evictor m a -> m (Evictor m a)
shrink input@(Evictor max xs _)
    | Array.length xs <= max = pure input
    | otherwise = pop input >>= shrink

touch :: forall m a. Eq a => Monad m => a -> Evictor m a -> m (Evictor m a)
touch x (Evictor max xs f) =
    let xs' = Array.cons x <<< Array.delete x $ xs
    in shrink (Evictor max xs' f)
