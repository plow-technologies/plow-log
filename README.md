# plow-log

This is a fairly bare bones contravariant logging library, with some bells and whistles, inspired by this talk: [MuniHac 2020: Duncan Coutts - Contravariant Logging: How to add logging without getting grumpy](https://www.youtube.com/watch?v=qzOQOmmkKEM&list=PLxxF72uPfQVTfDksvV4KPV5CxKnf0d_X3&index=17)

## What is contravariant logging?

The basic idea is really simple. Namely, we want to abstract over two things. The first is the message that gets printed and the second is the logging mechanism itself.

As an example, take:

```haskell
myFun :: IO ()
myFun = do
  ...
  Log.warn $ Text.pack $ "Polling threw an error. Exiting rtu-client: " <> show ex
  ...
```

The logging code can be distracting if the message is long. It is also a bit annoying to refactor if the logging library/message style needs to change. Instead, we can create a new data type of log messages/traces and pass an abstract tracer/logger function to `myFun` instead:

```haskell
myFun :: (MyFunTrace -> IO ()) -> IO ()
myFun tracer = do
  ...
  tracer $ PollingError ex
  ...
```

Now we can define a `tracer` as:

```haskell
tracer = \case
  PollingError ex -> warn $ Text.pack $ 
    "Polling threw an error. Exiting rtu-client: " <> show ex
```

To make everything more official/abstract/type-safe like, we wrap our callback function in a newtype called `Tracer`

```haskell
newtype Tracer m t = Tracer { unTracer :: t -> m () }
```

and define a helper function `traceWith`, which simply unwraps the function inside `Tracer` and applies it:

```haskell
traceWith :: Tracer m t -> t -> m ()
traceWith (Tracer f) t = f t
```

This on its own is a nice design pattern for logging, but there is no "contravariant" yet.

### Compositional logging

In the `rtu-client` codebase, we have several client protocols, which all implement logging of similar messages, e.g.:

```haskell
pollAllBristols mgr host port = do
  let env = buildClientEnv mgr host port
  eitherLocIds <- runClientM bristolGetLocationIds env
  case eitherLocIds of
    Left err -> do
      Log.warn $ Text.pack $ "Error grabbing bristol locations: " <> show err
      ...
```

```haskell
pollAllTotalFlows mgr host port = do
  let env = buildClientEnv mgr host port
  eitherLocIds <- runClientM tflowLocationIdsQuery env
  case eitherLocIds of
    Left err -> do
      Log.warn $ Text.pack $ "Error grabbing totalflow locations: " <> show err
      ...
```

What we want to do is refactor the two snippets above to log the same trace, namely, some sort of a "location query error":

```haskell
pollAllBristols tracer mgr host port = do
  let env = buildClientEnv mgr host port
  eitherLocIds <- runClientM bristolGetLocationIds env
  case eitherLocIds of
    Left err -> do
      traceWith tracer $ LocationQueryError err
      ...
```

This is much more concise and saves us the hassle of writing the protocol name in every log message. However, when it comes to logging the trace, we want to recreate the original message `Error grabbing <protocol name> locations: ...`. To do this, we can wrap our `LocationQueryError err` in a datatype keeping track of which protocol it originated from:

```haskell
data ProtocolTrace = ProtocolTrace {
  protocol :: String,
  trace :: MyFunTrace
}
```

Then we can define our new tracer as follows:

```haskell
protocolTracer :: Tracer IO ProtocolTrace
protocolTracer = Tracer $ \(ProtocolTrace protocol trace) -> case trace of
  LocationQueryError err -> warn $ Text.pack $ 
    "Error grabbing " <> protocol <> " locations: " <> show err
```

However, there is now obviously a mismatch between the tracer we want to pass into `pollAllBristols` or `pollAllTotalFlows`, which must be of type `Tracer IO MyFunTrace` (from the context), vs `protocolTracer` defined above. The obvious way to modify `protocolTracer` when passing to `pollAllTotalFlows` is the following one:

```haskell
let myFunTracer = Tracer $ \t -> unTracer protocolTracer $ ProtocolTrace "totalflow" t in
  pollAllTotalFlows myFunTracer ...
```

This is where the "contravariant" bit comes in. If we look at the types

```
tracer :: Tracer IO ProtocolTrace
myFunTracer :: Tracer IO MyFunTrace
ProtocolTrace "totalflow" :: MyFunTrace -> ProtocolTrace
```

we can see something functor-y going on. If we factor out the composition of `tracer` with the partially applied constructor `ProtocolTrace "totalflow"` into a separate function, we almost get `fmap`:

```haskell
notQuiteFmap :: (a -> b) -> Tracer m b -> Tracer m a
notQuiteFmap g (Tracer f) = Tracer $ f . g


myFunTracer = notQuiteFmap (ProtocolTrace "totalflow") protocolTracer
```

In fact, `notQuiteFmap` is an instance of a contravariant map or `contramap`, defined in `Data.Functor.Contravariant`:

```haskell
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
```

And there you have it. Defining a `Contravariant` instance for our `Tracer` type allows us to make tracers nicely compositional. It allows us to wrap additional information, such as the protocol name, into a tracer as it is passed deeper into some context without exposing this information to the inner context, i.e. `pollAllTotalFlows`/`pollAllBristols` no longer need to explicitly add the protocol name to their traces.

## IOTracer

Since all logging using `plow-log` (so far) is based in (some) IO monad, there is an additional convenience type called `IOTracer` provided in the `Plow.Logging` module. `IOTracer` is defined as

```haskell
newtype IOTracer a = IOTracer (forall m. MonadIO m => Tracer m a)
```

which allows us to hide the concrete monad `m` behind an existential. This can save us some typing in the form of `liftIO`s when passing the tracer to a different monad stack, e.g. when running a servant client/server monad. However, as always, the tradeoff is more confusing error messages from GHC in the case of a type error.

## Whats a Thrower?

In its original iteration by Scott, `Tracer` was even more parametric

```haskell
newtype Tracer m b a = Tracer (a -> m b)
```

The extra parameter `b` was used to define a special kind of tracer for Servant

```haskell
type ServantTracer m a = forall b. Tracer m b a
```

which could be used to better organise servant errors in a style similar to contravariant logging. Namely, instead of throwing a servant error as a string

```haskell
throwError err500 { errBody = "Simple cell store not found" }
```

we pull out the concrete implementation of how the error is thrown and formatted into an abstract `tracer`:

```haskell
traceWith servantTracer SimpleCellStoreNotFound
...
servantTracer = Tracer $ \t ->
  let msg = 
    case t of
      SimpleCellStoreNotFound -> "Simple cell store not found"
  in throwError err500 { errBody = msg }
```

However, upon further discussion, we decided to rename the `ServantTracer` and move it into a separate `Thrower` type. The reason for this is the fact that whilst both the `Tracer` and `Thrower` could in theory be seen as an instance of the same thing, in practice, they represent two very different things. 

For example, the module `Plow.Throwing` provides a function called `withTracer`, which combines a tracer and a thrower into a new thrower that logs the error before throwing. However, there seems to be no reason nor a natural/universal way of doing the opposite, i.e. turning a thrower into a tracer.

## Dem bells'n'whistles™

The `plow-log` library aims to be minimal, containing only the core definitions of a `Tracer` and `Thrower`, with the specifics of a concrete logging library/mechanism left to the user of this library. This allows the library to use anything from a simple `putStrLn` to using a `TMChan` for async traces. It also gives the user the flexibility to decide on the style/output/verbosity/etc.

 However, for ease of use, we included a nifty mechanism for fine grained control over shown traces. Using haskell generics, `Plow.Logging.EnumerableConstructors` defines the `HasEnumerableConstructors` class, which can be used to list all constructor names of a given value. For example, given

```haskell
data Foo = Foo Bar | Boo Far | Moo Foo deriving (Generic, HasEnumerableConstructors)
data Bar = Baz String deriving (Generic, HasEnumerableConstructors)
data Far = Far Bool
```

we can call `listConstructors` on any value to get all its constructors

```haskell
listConstructors $ Foo $ Baz "hello"
  = ["Foo", "Baz"]

listConstructors $ Foo $ Moo $ Foo $ Baz "hello"
  = ["Foo", "Moo", "Foo", "Baz"]

listConstructors $ Boo $ Far True
  = ["Boo"]
```

Notice that neither `Far` nor `True` are printed in the last example, since they do not have a specifically derived instance of `HasEnumerableConstructors`. 

Using this class together with the `withSilencedTracer` helper function, we can easily build a `Tracer` which filters out traces based on the constructor names.   
First, we define our custom trace type, adding `deriving (Generic, HasEnumerableConstructors)`, and define a `Tracer` for our type:

```haskell
data MyTrace = DebugTrace String | WarningTrace String | InfoTrace String deriving (Generic, HasEnumerableConstructors)

instance Show MyTrace where
  show (DebugTrace t)   = "[DEBUG] " <> t
  show (WarningTrace t) = "[WARN] " <> t
  show (InfoTrace t)    = "[INFO] " <> t


myTracer :: Tracer IO MyTrace
myTracer = Tracer print
```

Now, we can easily define a "production" tracer which will only log `INFO` and `WARN` messages, ignoring `DEBUG`:

```haskell
noDebugTracer = withSilencedTracer ["DebugTrace"] myTracer
```

The list of silenced constructors can be added to a config Dhall/YAML file and loaded on service startup, giving flexibility between testing and production logging.