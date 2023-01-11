module Test.Main where

import Prelude

import Effect (Effect)
import Test.BufferedZipper.Unit as BufferedZipperUnit
import Test.FlexStateT.Laws as FlexStateTLaws
import Test.MCache.Laws as MCacheLaws
import Test.Necklace.Laws as NecklaceLaws
import Test.Necklace.Unit as NecklaceUnit
import Test.Unit.Main (runTest)
import Test.Zipper.Laws as ZipperLaws
import Test.Zipper.Unit as ZipperUnit
import Test.ZipperM.Laws as ZipperMLaws
import Test.ZipperM.Unit as ZipperMUnit
import Test.RollingCache.Unit as RollingCacheUnit
import Test.RollingCacheT.Unit as RollingCacheTUnit

main :: Effect Unit
main = do
    runTest NecklaceUnit.tests
    runTest NecklaceLaws.tests
    runTest ZipperUnit.tests
    runTest ZipperLaws.tests
    runTest ZipperMUnit.tests
    runTest ZipperMLaws.tests
    runTest MCacheLaws.tests
    runTest BufferedZipperUnit.tests
    runTest FlexStateTLaws.tests
    runTest RollingCacheUnit.tests
    runTest RollingCacheTUnit.tests
