module ZipperM.Utils where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)


tail' :: forall a. Array a -> Array a
tail' = fromMaybe [] <<< Array.tail

init' :: forall a. Array a -> Array a
init' = fromMaybe [] <<< Array.init