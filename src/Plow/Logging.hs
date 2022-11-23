{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Plow.Logging
  ( Tracer (..),
    traceWith,
    HasEnumerableConstructors,
    invalidSilencedConstructors,
    warnInvalidSilencedConstructorsWith,
    withSilencedTracer,
    withAllowedTracer,
    withMaybeTracer,
    withEitherTracer,
    filterTracer,
    simpleStdOutTracer,
    simpleStdErrTracer,
    voidTracer,
    IOTracer (..),
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.List (intercalate, intersect)
import Data.Proxy (Proxy)
import Data.String (IsString (..))
import Plow.Logging.EnumerableConstructors
import System.IO (hPutStrLn, stderr)

newtype Tracer m a = Tracer (a -> m ())

instance Monad m => Semigroup (Tracer m a) where
  (Tracer f) <> (Tracer g) = Tracer $ \a -> f a >> g a

instance Monad m => Monoid (Tracer m a) where
  mempty = Tracer $ \_ -> pure ()

class TraceWith x m where
  traceWith :: x a -> a -> m ()

instance TraceWith (Tracer m) m where
  traceWith (Tracer t) = t

instance Contravariant (Tracer m) where
  contramap f (Tracer t) = Tracer (t . f)

invalidSilencedConstructors :: HasEnumerableConstructors a => Proxy a -> [String] -> [String]
invalidSilencedConstructors p silencedConstructors =
  let cs = allConstructors p
   in filter (\c -> not $ c `elem` cs) silencedConstructors

-- | Given a "string-like" tracer, outputs a warning message if the supplied 'silencedConstructors' do not
-- match the output of 'allConstructors' for the given tracer type 'a'
warnInvalidSilencedConstructorsWith :: (Applicative m, HasEnumerableConstructors a, IsString s) => Proxy a -> [String] -> Tracer m s -> m ()
warnInvalidSilencedConstructorsWith p silencedConstructors t = case invalidSilencedConstructors p silencedConstructors of
  [] -> pure ()
  invalid -> traceWith t $ fromString $ "Detected invalid silenced logging options: " <> intercalate ", " invalid

filterTracer :: Applicative m => (a -> Bool) -> Tracer m a -> Tracer m a
filterTracer test (Tracer f) = Tracer $ \m -> when (test m) (f m)

-- | Modifies a given tracer so that any message with a constructor name appearing in 'silencedConstructors'
-- will be discarded. In order to work as expected, the type 'a' is required to have an automatically
-- derived instance of 'HasEnumerableConstructors'.
-- @
--    data Foo = Florb Int | Fleeb String | Bar Bool deriving (Generic, HasEnumerableConstructors)
-- @
-- then calling
-- >  traceWith (withSilencedTracer ["Bar"] t) $ Bar False
-- is equivalent to
-- > pure ()
-- and
-- >  traceWith (withSilencedTracer ["Bar"] t) $ Florb 3
-- will be definitionally equal to
-- >  traceWith t $ Florb 3
withSilencedTracer :: (Applicative m, HasEnumerableConstructors a) => [String] -> Tracer m a -> Tracer m a
withSilencedTracer silencedConstructors = filterTracer (\m -> null $ listConstructors m `intersect` silencedConstructors)

-- | The opposite of 'withSilencedTracer'. This tracer only loggs messages which match a constructor in
-- 'allowedConstructors'.
withAllowedTracer :: (Applicative m, HasEnumerableConstructors a) => [String] -> Tracer m a -> Tracer m a
withAllowedTracer allowedConstructors = filterTracer (\m -> not $ null $ listConstructors m `intersect` allowedConstructors)

-- | Turns a tracer for some 'a' into a tracer for 'Maybe a', which traces 'Just x' using the original tracer
-- | and ignores 'Nothing' (i.e. 'pure ()')
withMaybeTracer :: Applicative m => Tracer m a -> Tracer m (Maybe a)
withMaybeTracer (Tracer t) = Tracer (maybe (pure ()) t)

-- | Takes two tracers for values 'a'  and 'b' to a tracer for 'Either a b', which selects the appropriate tracer for each value
withEitherTracer :: Applicative m => Tracer m a -> Tracer m b -> Tracer m (Either a b)
withEitherTracer (Tracer ta) (Tracer tb) = Tracer $ \case
  Left a -> ta a
  Right b -> tb b

simpleStdOutTracer :: MonadIO m => Tracer m String
simpleStdOutTracer = Tracer $ liftIO . putStrLn

simpleStdErrTracer :: MonadIO m => Tracer m String
simpleStdErrTracer = Tracer $ liftIO . hPutStrLn stderr

-- | Tracer that discards/ignores all messages. Useful in test suites, if we don't care about logging output
voidTracer :: Applicative m => Tracer m t
voidTracer = Tracer $ const $ pure ()

-- | To avoid having to write those pesky 'liftIO's when changing monads, e.g. when
-- passing the logger into a servant server, we can hide the monad entirely behind an
-- existential, requiring a 'MonadIO' instance.
-- We wrap in newtype instead of just a type synonym, to avoid having to have
-- RankNTypes turned on everywhere.
newtype IOTracer a = IOTracer (forall m. MonadIO m => Tracer m a)

instance Contravariant IOTracer where
  contramap f (IOTracer t) = IOTracer $ contramap f t

instance MonadIO m => TraceWith IOTracer m where
  traceWith (IOTracer (Tracer t)) = t
