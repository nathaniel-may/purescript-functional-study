module ZipperM.Utils where

import Prelude

import Data.Identity (Identity(..))
import Data.List.Lazy (List, nil)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))


tail' :: forall a. List a -> List a
tail' = fromMaybe nil <<< List.tail

init' :: forall a. List a -> List a
init' = fromMaybe nil <<< List.init

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x

-- TODO should this be part of an UnfoldableM1 class?
unfoldM1 :: forall m a b. Monad m => (b -> m (Tuple a (Maybe b))) -> b -> m (List a)
unfoldM1 f z = do
    Tuple x my <- f z
    case my of
        Nothing -> pure $ List.cons x nil
        Just y -> List.cons x <$> unfoldM1 f y
