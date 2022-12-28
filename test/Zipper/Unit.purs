-- | module for all unit tests
module Test.Zipper.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.Identity (Identity)
import Data.List.Lazy (List, fromFoldable)
import Data.Zipper (mkZipper)
import Data.Zipper as Zipper
import Data.ZipperM (mkZipperM)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import ZipperM.Test.Utils (PN(..), walk, walk')
import ZipperM.Utils (runIdentity)


tests :: TestSuite
tests = suite "Zipper unit tests" do

    test "toUnfoldable" do
        let input = (fromFoldable $ pure <$> [1, 2, 3, 4] :: List (Identity Int))
        let zipper = mkZipperM 0 input
        Assert.equal [0, 1, 2, 3, 4] (Zipper.toUnfoldable zipper)

    test "next and prev foward and back" do
        let zipper = mkZipper 0 (fromFoldable [1, 2])
        let values = runIdentity $ walk [N, N, P, P, P] zipper
        Assert.equal [0, 1, 2, 1, 0] values

    test "next' and prev' forward and back" do
        let zipper = mkZipper 0 (fromFoldable [1, 2])
        let values = runIdentity $ walk' [P, N, N, N, P] zipper
        Assert.equal [0, 0, 1, 2, 2, 1] values
