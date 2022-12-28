module Test.Main where

import Prelude

import Effect (Effect)
import Test.MCache.Laws as MCacheLaws
import Test.Unit.Main (runTest)
import Test.ZipperM.Unit as ZipperMUnit

main :: Effect Unit
main = do
    runTest ZipperMUnit.tests
    runTest MCacheLaws.tests
