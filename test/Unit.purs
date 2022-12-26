-- | module for all unit tests
module Test.Unit
    -- exporting only the full array to get dead code warnings if written tests aren't in the array
    (allTests) 
    where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..), isNothing)
import Data.ZipperM (focus, mkZipperM, next, next', prev, prev')
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Assert (assert, assert', assertEqual)

-- | array of all tests to run
allTests :: forall m. MonadEffect m => MonadState Int m => Array (m Unit)
allTests = [test0, test1]

fail :: String -> Effect Unit
fail msg = assert' msg false

-- | test that next and prev progress in the expected order
test0 :: forall m. MonadEffect m => m Unit
test0 = do
    let input = pure <$> [1]
    let zipper = mkZipperM 0 input
    liftEffect $ assertEqual { actual: focus zipper, expected: 0 }
    -- calling prev at the beginning of the zipper should return Nothing
    liftEffect <<< assert <<< isNothing $ prev zipper
    case next zipper of
        Nothing -> liftEffect $ fail "next failed from 0 -> 1"
        Just zipperm -> do
            zipper <- zipperm
            liftEffect $ assertEqual { actual: focus zipper, expected: 1 }
            -- calling next at the end of the zipper should return Nothing
            liftEffect <<< assert <<< isNothing $ next zipper
            case prev zipper of
                Nothing -> liftEffect $ fail "prev failed from 1 -> 0"
                Just zipperm -> do
                    zipper <- zipperm
                    liftEffect $ assertEqual { actual: focus zipper, expected: 0 }

-- | test that next' and prev' progress in the expected order
test1 :: forall m. MonadEffect m => m Unit
test1 = do
    let input = pure <$> [1, 2]
    let zipper = mkZipperM 0 input
    liftEffect $ assertEqual { actual: focus zipper, expected: 0 }
    -- calling prev' at the beginning of the zipper should return the same zipper
    zipper <- prev' zipper
    liftEffect $ assertEqual { actual: focus zipper, expected: 0 }
    zipper <- next' zipper
    liftEffect $ assertEqual { actual: focus zipper, expected: 1 }
    zipper <- next' zipper
    liftEffect $ assertEqual { actual: focus zipper, expected: 2 }
    -- calling next' at the end of the zipper should return the same zipper
    zipper <- next' zipper
    liftEffect $ assertEqual { actual: focus zipper, expected: 2 }
    zipper <- prev' zipper
    liftEffect $ assertEqual { actual: focus zipper, expected: 1 }
