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
