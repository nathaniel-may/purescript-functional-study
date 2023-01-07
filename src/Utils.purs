module Utils where

import Prelude

import Data.Array as Array
import Data.List.Lazy (List, nil)
import Data.List.Lazy as List
import Data.Maybe (fromMaybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))


tail' :: forall a. List a -> List a
tail' = fromMaybe nil <<< List.tail

arrayTail' :: forall a. Array a -> Array a
arrayTail' = fromMaybe [] <<< Array.tail

init' :: forall a. List a -> List a
init' = fromMaybe nil <<< List.init

nonEmptyListPrepend :: forall a. List a -> NonEmpty List a -> NonEmpty List a
nonEmptyListPrepend xs ys =
    maybe ys (\{ head, tail } -> (head :| tail) <> ys) (List.uncons xs)
