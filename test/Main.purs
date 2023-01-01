module Test.Main where

import Prelude

import Effect (Effect)
import Test.MCache.Laws as MCacheLaws
import Test.Unit.Main (runTest)
import Test.Zipper.Unit as ZipperUnit
import Test.ZipperM.Laws as ZipperMLaws
import Test.ZipperM.Unit as ZipperMUnit
import Test.BufferedZipper.Unit as BufferedZipperUnit

main :: Effect Unit
main = do
    runTest ZipperMUnit.tests
    runTest ZipperUnit.tests
    runTest ZipperMLaws.tests
    runTest MCacheLaws.tests
    runTest BufferedZipperUnit.tests
