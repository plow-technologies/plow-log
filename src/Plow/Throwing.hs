{-# LANGUAGE RankNTypes #-}

module Plow.Throwing where

import Data.Functor.Contravariant (Contravariant (..))
import Plow.Logging

newtype Thrower m a = Thrower (forall b. a -> m b)

instance Contravariant (Thrower m) where
  contramap f (Thrower t) = Thrower (t . f)

throwWith :: Thrower m a -> a -> m b
throwWith (Thrower t) a = t a

withTracer :: Monad m => Tracer m a -> Thrower m a -> Thrower m a
withTracer (Tracer tr) (Thrower th) = Thrower $ \m -> tr m >> th m
