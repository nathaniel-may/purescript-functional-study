-- | module for all unit tests
module Test.BufferedZipper.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.BufferedZipper (mkBufferedZipper)
import Data.BufferedZipper as BufferedZipper
import Data.Identity (Identity)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import ZipperM.Test.Utils (PN(..), bWalk)
import ZipperM.Utils (runIdentity)


tests :: TestSuite
tests = suite "ZipperM unit tests" do

    test "next and prev foward and back" do
        let input = (pure <$> [1, 2, 3, 4] :: Array (Identity Int))
        let zipper = mkBufferedZipper 3 0 input
        let values = runIdentity $ bWalk [N, N, N, N, P, P, P, P] zipper
        Assert.equal [0, 1, 2, 3, 4, 3, 2, 1, 0] values

    test "toArray" do
        let input = (pure <$> [1, 2, 3, 4] :: Array (Identity Int))
        let zipper = mkBufferedZipper 3 0 input
        let expected = pure <$> [0, 1, 2, 3, 4]
        Assert.equal expected (BufferedZipper.toArray zipper)
