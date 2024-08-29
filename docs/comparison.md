# Comparison with other approaches

Some of the goals that you might have when using `cleff` can be
achieved in other ways, and the library may remind you of those other
techniques. Here I try to discuss what those approaches offer and how
they differ from `cleff`.

## Unifying with one monad

Instead of using any kind of fancy Haskell magic to structure our
application, we could simply build the entire thing using one common
everyday monad. At first, this might be `IO`; over time it might
become something like `MyAppM`, which might be a newtype over `ReaderT
MyEnvironment IO`. We might still use individual monad transformers
such as `ExceptT` in certain circumstances, but most code is written
against one monad.

This approach has the advantage that it's simple. Using `IO` is a
typical milestone for Haskell programmers, and everything in your
application is concrete, so you don't have to consider
polymorphism. It's easy to jump from call site to definition. Type
errors are easy to understand.

In this model, different subsystems are not really separated. Because
everything uses the same monad, functions can plug in anywhere. This
can make the codebase feel like a "big ball of mud", but the flip side
is that it's easy to move abstraction boundaries around, as they are
largely conventional rather than enforced in code.

Testing in this model is tricky, because everything is "baked in". If
your code under test makes a database call, there absolutely has to be
a database on the other end. Pure code is easy to write tests for, but
anything with side effects is in your application's "big monad", and
that means it can do essentially anything your whole application
does. Every test becomes an end-to-end test, and while end-to-end
tests are helpful, they are expensive to write and maintain.

Code written in this style might look something like:

```haskell
log :: String -> IO ()
log x = putStrLn x

display :: String -> IO ()
display x = putStrLn x

promptText :: String -> IO String
promptText s = do
  putStrLn s
  getLine

readUntilYesOrNo :: IO Bool
readUntilYesOrNo = error "leaving blank for succinctness"

chat :: IO ()
chat = do
  name <- promptText "What's your name?"
  log $ "User entered name: " ++ name
  -- ...
```

Testing any program that uses `putStrLn` is challenging. The easiest
way I can think of is to fork it and capture its standard output from
outside the process.

### Testing with stubs

At my day job, we mainly use "large" monads like the above -- one for
database access, and one for most other effectful code.

A common responsibility of our code is to make requests to third-party
APIs. We have some functions that do this and they return actions in
`MyAppM`. As with the calls to `putStrLn`, code that makes HTTP
requests via a real network stack is tricky. One approach might be to
stand up a server process to connect to, and for some common APIs you
might be able to do this (using something like `moto` for AWS, for
example), but in general this is a lot of work.

Another approach we use at work is to define "stubs". Stubs are sort
of like a catalog of effectful functions that you might invoke which
have a default implementation but can be overridden for testing
purposes. That might look something like this:

```haskell
data Stubs = Stubs { stubLog :: Maybe (String -> IO ()) }

newtype MyAppM a = MyAppM { runMyAppM :: ReaderT Stubs IO a }
  deriving newtype (MonadIO, MonadReader)

log :: String -> MyAppM ()
log s = do
  stubs <- ask
  case stubs.stubLog of
    Nothing -> liftIO $ putStrLn s
    Just stubLog -> liftIO $ stubLog s

-- Then, in tests..
logsMVar <- newMVar
let collectLogs s = modifyMVar_ (pure . (s:)) logsMVar
withStubs (stubs { stubLog = collectLogs }) $ chat
logsOutput <- takeMVar logsMVar
-- Assert properties of logsOutput...
```

One unfortunate property of stubs implemented this way is stubs are
available even in your real production code, rather than isolated to
testing code. Someone would probably catch it in code review if you
tried to provide a stub in production, but it's still kind of unclean
architecturally.

Another problem is that a lot of code will use the `MyAppM` type, and
the `MyAppM` type depends on `Stubs`, which depends on a variety of
different types that are used from different stubbable functions. This
introduces choke points in your module compilation graph, which can be
a problem in larger projects -- changing an innocuous-looking type can
force recompilation of lots of things that just happen to mention
`MyAppM` somewhere, and if you want to load a module in the REPL, it
may take some time to load all the dependencies.

To get around this problem, someone at work introduced "dynamic" type
stubs, which use some pretty fancy type machinery and dynamic
dispatch, which doesn't have this problem. Instead, Stubs is a
type-based map where code that wants a specific kind of stub can look
it up, and a test which wants to provide that specific kind of stub
can provide it, without any other code incurring dependencies. Because
stub type signatures aren't necessarily unique, they tend to get
grouped into record types, with one record type per subsystem (one per
third-party API, for instance).

With stubs, it's possible to provide a sort of substitute
implementation of something effectful, with two caveats:

- It's impossible to tell what substitutions you can or need to make
  in a test. You have to read the code, and if the code changes,
  nothing can let you know that your test might need to change too. By
  contrast, with `cleff`, the types communicate what effects are in
  use.
- It's only available in your "big monad". You can't test logic in
  isolation outside of your monad, even if you have pure
  implementations of some stubs. With `cleff`, once you have provided
  implementations of all effects, you get a pure value, which you can
  use in pure tests.

Stubs also don't really help with the other problem of "one big
monad", which is that it's hard to conceptualize subsystems and that
there's no boundary between them.

## MTL-style typeclasses

Another approach to system composition, common in Haskell, is the use
of typeclasses. Matt Parsons's blog post
https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
shows what this looks like (he calls it "layer 2"). Some people call
these "MTL-style" typeclasses, because they are familiar from examples
like `MonadReader` and `MonadState` from the `mtl` library. However,
instead of corresponding exactly to specific monad transformers, they
correspond to subsystems or domain concepts. Rewriting the above
example in this style might look something like this:

```haskell
class MonadLog m where
  log :: String -> m ()

instance MonadLog IO where
  log x = putStrLn x

class Interact m where
  display :: String -> m ()
  promptText :: String -> m String

instance Interact IO where
  display x = putStrLn x
  promptText s = do
    putStrLn s
    getLine

readUntilYesOrNo :: Teletype m => m Bool
readUntilYesOrNo = error "leaving blank for succinctness"

main :: IO ()
main = chat

chat :: (MonadLog m, Interact m) => ()
chat = do
  name <- promptText "What's your name?"
  log $ "User entered name: " ++ name
  -- ...
```

Our typeclasses here help divide subsystems and express dependencies
right in the type signatures. We can provide alternate implementations
for any specific dependency too by providing types that implement the
relevant typeclasses.

Another term used for this kind of approach is "tagless final
encoding". The idea is that instead of constructing programs which are
interpreted, as in `cleff`, we call typeclass methods and those
typeclass methods act directly, with no intermediate interpretation
step.

The classic way to implement these typeclasses is using monad
transformers (which Matt also writes about in
https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html). One
unfortunate problem here is that each monad transformer ends up
needing passthrough instances of each typeclass. This is sometimes
called the "n-squared problem" and it means adding a lot of
boilerplate, which scales badly as your project grows. `cleff` avoids
this by having one specific concrete monad.

Still, conceptually, this approach isn't incompatible with something
like `cleff`. Typeclasses admit multiple different implementations of
their contracts, and `Eff` can be one implementation for those teams
that prefer it. Typeclasses can serve admirably as type-level
encodings of the subsystems and dependencies in your project. I
haven't tried it but I suspect it's possible to go the other way too
and convert typeclasses into `cleff` effects.

One nice thing about using typeclasses is that they are additive, so
they compose well. `cleff` effect constraints are typeclasses too, and
they have the same property.

Typeclasses also have the advantage over the "one big monad"/"stubs"
approach that it's possible to write pure tests -- simply write an
implementation of the typeclass that acts purely and can be
unwrapped. A newtype around `Identity` might work!

### Provide simple functions

It's a trope in Haskell that you can "scrap your typeclasses" and
replace them with (in the general case) records of functions, where
each function corresponds to a typeclass method. Matt Parsons has
written something about this approach too, which he calls "[inverting
your
mocks](https://www.parsonsmatt.org/2017/07/27/inverted_mocking.html)".

Writing the above example in this style would look something like
this:

```haskell
logIO :: String -> IO ()
logIO x = putStrLn x

displayIO :: String -> IO ()
displayIO x = putStrLn x

promptTextIO :: String -> IO String
promptTextIO s = do
  putStrLn s
  getLine

data Interact m = Interact
  { display :: String -> m ()
  , promptText :: String -> m String
  }

interactIO :: Interact IO
interactIO = Interact
  { display = displayIO
  , promptText = promptTextIO
  }

readUntilYesOrNo :: IO Bool
readUntilYesOrNo = error "leaving blank for succinctness"

main :: IO ()
main = chat logIO interactIO

chat :: Monad m => (String -> m ()) -> Interact m -> m ()
chat log interact = do
  name <- interact.promptText "What's your name?"
  log $ "User entered name: " ++ name
  -- ...
```

This is a nice trick for working at smaller granularities -- rather
than defining a new effect or adding an operation, you can extract the
effectful parts of your code and turn them into inputs. You can even
make the monadic type be polymorphic, which means you can write tests
that work with any monad. If you can provide an implementation in a
pure monad, you can write pure tests. For example, you can provide
implementations that produce `Identity` values, and then `runIdentity`
them in your tests.

I wouldn't try to use this approach to define all the effects in my
application, but I think you could if you wanted.

# Conclusion

The need to decouple systems from their dependencies in some way,
especially for the purposes of testing, is fundamental to writing
production-quality software. The approaches discussed here can "feel"
very different but are actually quite similar if you squint: you
formalize a concept of a subsystem, you define alternative
implementations, and you set up your tests with test
implementations. `cleff` has some nice properties:

- No n-squared problem -- one concrete monad.
- Type signatures show the effects being used.

But at the end of the day, many approaches are possible and all seem
workable.
