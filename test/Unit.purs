-- | module for all unit tests
module ZipperM.Test.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (unitSuite) 
    where

import Prelude

import Control.Pure (Pure, run)
import Data.ZipperM (mkZipperM)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import ZipperM.Test.Utils (PN(..), walk, walk')

-- import Test.Assert (assert, assert', assertEqual, assertEqual')

-- | array of all tests to run
unitSuite :: TestSuite
unitSuite = suite "unit tests" do

    test "next and prev foward and back" do
        let input = (pure <$> [1, 2] :: Array (Pure Int))
        let zipper = mkZipperM 0 input
        let values = run $ walk [N, N, P, P, P] zipper
        Assert.equal [0, 1, 2, 1, 0] values

    test "next' and prev' forward and back" do
        let input = (pure <$> [1, 2] :: Array (Pure Int))
        let zipper = mkZipperM 0 input
        let values = run $ walk' [P, N, N, N, P] zipper
        Assert.equal [0, 0, 1, 2, 2, 1] values
