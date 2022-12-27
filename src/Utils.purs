module ZipperM.Utils where

import Prelude

import Data.List.Lazy (List, nil)
import Data.List.Lazy as List
import Data.Maybe (fromMaybe)


tail' :: forall a. List a -> List a
tail' = fromMaybe nil <<< List.tail

init' :: forall a. List a -> List a
init' = fromMaybe nil <<< List.init