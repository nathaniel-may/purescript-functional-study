module Test.Main where

import Prelude

import Effect (Effect)
import Test.Laws.MCache as MCache
import Test.Unit.Main (runTest)
import ZipperM.Test.Unit (unitSuite)

main :: Effect Unit
main = do
    runTest unitSuite
    runTest MCache.laws
