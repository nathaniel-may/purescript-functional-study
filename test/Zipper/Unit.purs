-- | module for all unit tests
module Test.Zipper.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.Lazy (defer)
import Data.List.Lazy (fromFoldable)
import Data.List.Lazy as List
import Data.List.Lazy.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Zipper as Zipper
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import ZipperM.Test.Utils (PN(..), walkZipperM)
import ZipperM.Utils (runIdentity)


mkNonEmpty :: forall a. a -> Array a -> NonEmptyList a
mkNonEmpty h t = NonEmptyList $ defer (\_ -> (h :| List.fromFoldable t))

tests :: TestSuite
tests = suite "Zipper unit tests" do

    test "constructors are all equivelant" do
        let zFromList = Zipper.fromList (fromFoldable [0, 1, 2, 3, 4])
        let list = (Zipper.toUnfoldable <$> zFromList) :: Maybe (Array Int)
        let zFromNonEmpty = Zipper.fromNonEmpty (mkNonEmpty 0 [1, 2, 3, 4])
        let list1 = (Zipper.toUnfoldable <$> Just zFromNonEmpty) :: Maybe (Array Int)
        Assert.assert
            (show list <> " != " <> show list1)
            (zFromList == Just zFromNonEmpty)

    test "toUnfoldable" do
        let zipper = Zipper.fromNonEmpty (mkNonEmpty 0 [1, 2, 3, 4])
        Assert.equal [0, 1, 2, 3, 4] (Zipper.toUnfoldable zipper)

    test "next and prev foward and back" do
        let zipper = Zipper.fromNonEmpty (mkNonEmpty 0 [1, 2, 3, 4])
        let values = runIdentity $ walkZipperM [N, N, P, P, P] zipper
        Assert.equal [0, 1, 2, 1, 0] values
