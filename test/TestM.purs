module Test.TestM where

import Prelude

import Control.Monad.State.Trans (class MonadState, StateT, evalStateT)
import Effect (Effect)
import Effect.Class (class MonadEffect)


newtype TestM a = TestM (StateT Int Effect a)

derive newtype instance testMBFunctor :: Functor TestM
derive newtype instance testMBApply :: Apply TestM
derive newtype instance testMBind :: Bind TestM
derive newtype instance testMApplicative :: Applicative TestM
derive newtype instance testMMonad :: Monad TestM
derive newtype instance testMState :: MonadState Int TestM
derive newtype instance testEffect :: MonadEffect TestM

runTestM :: forall a. TestM a -> Effect a
runTestM (TestM m) = evalStateT m 0
