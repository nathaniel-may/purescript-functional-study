module Control.Pure where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- TODO replace with library for Identity Monad
newtype Pure a = Pure a

run :: forall a. Pure a -> a
run (Pure x) = x

derive instance pureGeneric :: Generic (Pure a) _
derive instance pureFunctor :: Functor Pure

instance pureApply :: Apply Pure where
    apply (Pure f) (Pure x) = Pure (f x)

instance pureApplicative :: Applicative Pure where
    pure = Pure

instance pureBind :: Bind Pure where
    bind (Pure x) f = f x 

instance pureMonad :: Monad Pure

instance pureShow :: Show a => Show (Pure a) where
    show = genericShow

instance pureEq :: Eq a => Eq (Pure a) where
    eq (Pure x) (Pure y) = x == y
