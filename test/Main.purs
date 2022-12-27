module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import ZipperM.Test.Unit (unitSuite)

main :: Effect Unit
main = runTest unitSuite
