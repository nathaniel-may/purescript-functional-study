module Test.Main where

import Prelude

import Data.Traversable (traverse)
import Effect (Effect)
import Test.TestM (runTestM)
import Test.Unit as Unit

main :: Effect Unit
main = void $ traverse runTestM Unit.allTests
