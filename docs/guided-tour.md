# Logging

Let's start by looking at a single effect. A simple one might be the
`Logging` effect in `Effects.Logging`. You'll see the definition of the effect:

```haskell
data Logging :: Effect where
  Log :: String -> Logging m ()
```

This says that a program that is using `Logging`, which is an
effect. If you are using `Logging`, then you have an operation
available to you called `Log`. This operation takes a string and
doesn't produce anything. The operations in this effect define a
"contract" that any implementation of this "system" has to conform to.

To set our new effect up to be used with `cleff`, we run:

```haskell
makeEffect ''Logging
```

Once we have this, you can call `log "some string"` as long
as the `Logging` effect is available. So, we might have this:

```
sayHello :: (Logging :> es) => Eff es ()
sayHello = log "hi there"
```

Type signatures when using `cleff` are usually expressed in terms of
`es`, which is to say "more than one `e`", where `e` is an effect. The
`:>` operator here is an assertion that something is contained in a
set -- "`Logging` is a member of `es`".

Just to keep our terminology straight, we'll call `Logging` an
"effect" and `Log` an "operation". We'll use the term "program" for
`sayHello` or any other monadic action that uses effects. These
"programs", like `IO` actions, are sort of "inert": they are pure
representations of things we could do, but they don't themselves
act. In order to do something with them, we "interpret" them. This
means providing implementations of each effect. Ideally, effects are
relatively orthogonal, and each one has its own "interpreter".

Alongside our `Logging` effect, we provide an implementation. The
obvious way to respond to `Log` actions is to print them on standard
out. We can define this "interpretation" like this:

```haskell
runLoggingStdout = interpretIO \case
  Log s -> putStrLn s
```

Focus first on the `case` expression here. This is where we are
providing an implementation of what to do when we get one of the
operations from our effect. Each operation has to be present in the
`case`. Here we are explaining that we can handle a `Log` operation by
using the IO effect. By doing this, we can take an existing program
that has some effects including `Logging` and replace all the
`Logging` operations with IO-based ones. After this, our result will
be a program that no longer uses `Logging`. That's what the type
signature is telling us too:

```
runLoggingStdout :: (IOE :> es) => Eff (Logging : es) a -> Eff es a
```

It says: given that our program has `IOE` (the `cleff` way to talk
about the IO effect) somewhere in the effects it uses, and given that
the "first" effect it has is `Logging`, we can rewrite it so that it
no longer uses the `Logging` effect.

We can also provide another way to "interpret" `Logging`
operations. `cleff` has a built-in `Output` effect. A program can use
the `output` operation to return additional information.

```haskell
loggingToOutput :: Eff (Logging : es) w -> Eff (Output String : es) w
loggingToOutput =
  reinterpret \case
    Log s -> output s
```

Here the type signature is a little different -- we are directly
converting the `Logging` effect into the `Output` effect. From there
we can use other implementations of `Output` from `cleff` to, for
example, accumulate all the output strings. This gives us a program
that doesn't use IO at all, which is to say, a pure program. We can
use this when writing tests that ensure that our program is logging
appropriately.

# Teletype

Let's look at a more complicated effect, in `Effects.Teletype`.

```haskell
data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()
```

This effect has two operations in it. The second one, `WriteTTY`,
looks pretty similar to `Log`, so we won't cover it. However,
`ReadTTY` has a different structure. This one doesn't take any
parameters, but the result of performing the `ReadTTY` operation is a
string (presumably, something that the user typed). As with `Logging`,
once we use `makeEffect`, we can invoke the operation using
`readTTY`. Here's a program that might use both `Teletype` and
`Logging` effects:

```haskell
rev :: (Teletype :> es, Logging :> es) => Eff es ()
rev = do
  writeTTY "Type a string:"
  s <- readTTY
  log $ "User typed " ++ s
  writeTTY $ reverse s
```

We can mix operations of different effects because they are all
available. We see from the type signature that `es` must contain
`Teletype` and `Logging`. `cleff` also provides a shorter way to write
this: `[Teletype, Logging] :>> es`.

In `Effects.Teletype` we provide two "interpretations" of the
effect. One uses IO and works in the way you would expect. The other
is pure, and it works by taking a list of strings that the "user"
would type and producing a list of strings that were written to the
TTY. This is suitable for use in testing.

I've modified the definitions from the examples from the `cleff`
`README` just to try to make them clearer. In particular, the pure one
uses effects built-in to `cleff`, namely `Input` and `Output`, to make
it easy to define alternative implementations, but that can be hard to
read if you aren't familiar with those effects.

# Interact

We want a higher-level concept for interacting with a user using the
terminal, which we will call `Interact`. From our architecture diagram
in the intro, we know that this will build on the simpler interface
from `Teletype`. How does this look?

```haskell
data Interact :: Effect where
  PromptText :: String -> Interact m String
  PromptYesOrNo :: String -> Interact m Bool
  Display :: String -> Interact m ()
```

Interestingly, the fact that `Interact` "depends" on `Teletype` is not
visible at the effect level. This is because operations are abstract
-- the dependency is only in how the operations are implemented. But
that means that we can also offer a pure implementation of the same
effect, and use it in tests without doing IO at all. You can probably
think of different ways to represent this, but I chose this one:

```haskell
data Talker = Talker
  { respondText :: String -> String
  , respondYesNo :: String -> Bool
  }

runInteractTalker :: Talker -> Eff (Interact : es) a -> Eff (Output String : es) a
runInteractTalker talker = reinterpret \case
  PromptText s -> output s >> pure (talker.respondText s)
  PromptYesOrNo s -> output s >> pure (talker.respondYesNo s)
  Display s -> output s
```

If you have an action that has an `Interact` effect in it, plus one of
these silly "talker" values, we can turn that `Interact` into output
strings (the things displayed). The answers to the prompts come from
running the "talker". This isn't necessarily the easiest possible
representation of a "user" for testing -- there's no state tracked
across prompts -- but it will get the job done.

The "IO" implementation doesn't really use IO -- instead it just
invokes the `Teletype` effect from the last section. That looks like
this:

```haskell
runInteractTeletype :: Eff (Interact : es) a -> Eff (Teletype : es) a
runInteractTeletype = reinterpret \case
  PromptText s -> writeTTY s >> readTTY
  PromptYesOrNo s -> writeTTY s >> readUntilYesOrNo    -- see below
  Display s -> writeTTY s
```

Here we convert the `Interact` effect into a `Teletype` effect. Each
`Interact` operation corresponds to some combination of `Teletype`
operations. Many of them are straightforward, but `PromptYesOrNo` is a
little tricky because we want to be user-friendly, which means accept
"yes" and "no" answers, handle cases that aren't either of them,
etc. Having that logic buried in the interpreter for `Interact` feels
a little heavy. Because `Eff` is monadic, we can just define another
"program", which we call `readUntilYesOrNo`, and call it in the
interpreter. Here's that helper function:

```haskell
-- Using the Teletype effect, ask the user for either a "yes" or "no"
-- input and convert it to a Bool.
readUntilYesOrNo :: (Teletype :> es) => Eff es Bool
readUntilYesOrNo = do
  writeTTY "Please enter a response (yes or no):"
  res <- readTTY
  case res of
    "yes" -> pure True
    "no" -> pure False
    _ -> do
      writeTTY "I'm sorry, I didn't understand your response."
      readUntilYesOrNo
```

We could probably quibble with the UX of this function, but because
it's standalone, and because it, too, is defined in terms of an effect
(in this case, just `Teletype`), then it's straightforward to write
tests for it. This serves as a helpful segue into our first example of
testing, which is in `test/Effects/InteractSpec.hs`.

# Testing: InteractSpec

We've been promising that using `cleff` will make our code easier to
test, but so far we haven't seen that in action. How do we test things
written with `cleff`?

So far, we've written a couple of effects, which by themselves seem
relatively inert -- just a collection of, not even operations, but
just the shape the operations have. Testing these doesn't feel
valuable; they're just data and we would find ourselves with
assertions that the data we constructed was the data we
constructed.

If we don't test effects, maybe we can test interpreters. This feels
more valuable because interpreters actually do something. However the
interpreters we have seen up to this point have largely been almost
trivial. We can test an interpreter by writing an action that uses
some effect, and interpret into an action in another effect, and then
running that new action, but this is almost like an end-to-end test
and involves a lot of layers where things could go wrong.

Another thing we could test is our programs. Unlike effects, programs
contain real logic, and unlike interpreters, they are insulated from
their interpretations, so it makes sense to test them. Although
`readUntilYesOrNo` is used in our `Interact` interpreter, it's
actually a program that uses the `Teletype` effect, and because of
this, it's the first action where it feels like it might be valuable
to test.

Our first test looks like this:

```haskell
it "reads until it gets something it understands" do
  let inputs =
        [ "nop"
        , "yes"
        ]
      (val, outputs) = runPure $ runTeletypePure inputs readUntilYesOrNo
  outputs
    `shouldBe` [ "Please enter a response (yes or no):"
               , "I'm sorry, I didn't understand your response."
               , "Please enter a response (yes or no):"
               ]
  val
    `shouldBe` True
```

We start with our `readUntilYesOrNo` function. It has the type
signature `(Teletype :> es) => Eff es Bool`. This says that it has an
effect type that must have at least the `Teletype` effect, but it's
polymorphic, so when we use it, we can choose what effects will be
available. In our test, we will choose `Eff [Teletype] Bool`, which
is the simplest action type that includes `Teletype`. Then, we use our
`runTeletypePure` function from the `Teletype` module to convert our
`readUntilYesOrNo` program into `Eff [] (Bool, [String])` -- we
eliminate the `Teletype` effect using the hardcoded inputs of our test
and by returning the outputs of our action. Then, we convert an `Eff`
with no effects using `runPure`, converting `Eff [] (Bool, [String])`
into `(Bool, [String])`. Finally, we verify those outputs.

This gives us a good sense of what it "feels like" to use `cleff`. We
have some monadic "program" that uses some effects. We "eliminate"
each effect in turn, always working on the effect at the front of the
list, by providing implementations that use simpler
effects. Eventually we get down to `Eff [] a` or `Eff [IOE] a` and we
can eliminate the `Eff` type altogether using `runPure` or `runIOE`.

This test doesn't give us perfect confidence about all of the code
we've written. We now have confidence that `readUntilYesOrNo` works,
but we have only tried running it using `runTeletypePure`. In
production, we will almost certainly run it using `runTeletypeIO`, and
there could be surprises there. For example, when reading a line from
the terminal, what happens to the newline at the end? What happens
when EOF is reached? Testing these behaviors would require testing
`runTeletypeIO`, which would require writing tests against `IO`, which
is back where we started. However, we have already "freed" a lot of
code, including all of `readUntilYesOrNo`, from this trap. And if
`runTeletypeIO` is simple enough, we may be satisfied that its
implementation is correct, with the edge cases understood by looking
through the documentation for the underlying primitives that are
called from there. After all, we don't write unit tests for
third-party libraries that we depend on either.

# SampleProgram

We'll skip the `UserStore` module, which defines a `UserRecord` type
and a `UserStore` effect which should be reasonable now that you've
seen three other effects. Please note that there's no "real"
implementation of the `UserStore` effect, so when you run the program
we always have an empty user store and don't record what you write.

Let's look now at a "real life" application of these effects in the
`SampleProgram` module.

```haskell
readUserFromInteract :: (Interact :> es) => String -> Eff es UserRecord
readUserFromInteract name = do
  display $ "Nice to meet you, " ++ name ++ "!"
  plane <- promptYesOrNo "Do you like airplanes?"
  if not plane
    then do
      display "Me either! Personally, I don't mind takeoffs -- it's the landings that scare me!"
      -- FIXME: find another way to identify the affluence of the user
      pure UserRecord {name = name, flies = plane, rich = False}
    else do
      rich <- promptYesOrNo "Do you fly first class?"
      if rich
        then display "Wow! Those tech salaries are really something!"
        else display "Well, maybe one day after the IPO!"
      pure UserRecord {name = name, flies = plane, rich = rich}
```

This defines a silly little "conversation" program which, when run,
gets some information about the user and constructs a record with that
information. I was stuck on a flight when I wrote this, sorry, so this
is where my head was at. It's pretty basic except for the fact that it
uses our effect. As before, that's manifest in the type signature with
the constraint that `Interact :> es`.

We can use this program, as normal, to build bigger programs. For
example, if we have access to `UserStore`, we can retrieve an existing
user, and potentially handle it differently from a new user:

```haskell
chat :: ([Interact, UserStore] :>> es) => Eff es ()
chat = do
  name <- promptText "What's your name?"
  userM <- lookupUserByName name
  case userM of
    Nothing -> do
      inputUser <- readUserFromInteract name
      storeUser inputUser
    Just user -> do
      inputUser <- refreshUserFromInteract user
      storeUser inputUser
  display $ "It was nice talking to you, " ++ name ++ ". Hope you enjoy your trip!"
```

Here we introduce the `:>>` operator, which lets us add multiple
effects. We can seamlessly use `readUserFromInteract` here, since
`Interact` is definitely present, and `readUserFromInteract` does not
care about the other effects available. In other words, effects are
cumulative.

The tests for `chat` are in `SampleProgramSpec`. They are probably
more or less what you would expect. We provide pure implementations of
`UserStore` and `Interact` and then use `runPure` once there are no
more effects left.

# Main

As in tests, when we want to use our little dialog program in `main`,
we have to provide implementations of the effects we want. `chat`
needs `Interact` and `UserStore`. We want to convert `Interact` into
`Teletype`, and then we want to convert `Teletype` to `IOE`, which is
the way `cleff` refers to effects using built-in Haskell IO. We have
already seen `runTeletypeIO`, which has this type signature:

```haskell
runTeletypeIO :: (IOE :> es) => Eff (Teletype : es) a -> Eff es a
```

Compare this with the other type signatures we've seen for effect
implementations:

```haskell
-- Completely eliminates a Teletype effect using pure structures
runTeletypePure :: [String] -> Eff (Teletype : es) a -> Eff es (a, [String])

--- Replaces an Interact effect with a Teletype effect
runInteractTeletype :: Eff (Interact : es) a -> Eff (Teletype : es) a
```

In general, our interpreters "eliminate" an effect, but we see three different approaches here.

- `runTeletypePure` completely eliminates it (it's completely gone
  from the return type).
- `runInteractTeletype` just replaces the first effect with a simpler
  one. Directly after using `runInteractTeletype`, we'll have to use
  an interpreter for `runInteractTeletype`.
- `runTeletypeIO` conceptually replaces `Teletype` with `IOE`, but
  instead of sticking it back on the front of the effects list, it
  just asserts `IOE` is somewhere in the remaining effects.

Handling `IOE` is a little different from handling other effects
because there's no way to provide an interpreter for it. (As always in
Haskell, IO actions are opaque.) For this reason, we can't just stick
it on the front of the effects list, because there is no way to "reach
past" it to interpret the remaining effects. We always eliminate
effects from the front, and our goal is to end with `Eff [IOE] a`, so
when we want to use the `IOE` effect, we want to ensure that it's at
the end of the effects list rather than the beginning.

In this case, we use a function with a type signature that indicates
that `IOE` is somewhere in the remaining effects, which addresses
these concerns.

[Aside: It's not clear to me yet under what circumstances it's better
to use a function with this kind of type signature vs. one which uses
`:` to add the new effect to the front of the list, but it seems like
an important aspect is whether we expect the new effect to be present
in multiple other implementations. If it is, we probably want to use
`:>`, as with `IOE`.]

We saw previously that `chat` uses _at least_ `Interact` and
`UserStore`. But we know we need `IOE` to be the last effect in the
stack. Fortunately, `chat` is polymorphic in the set of what effects,
exactly, are present in the stack, so as the caller, we get to choose
the stack. We can choose `Eff [Interact, UserStore, IOE] ()` without
loss of generality, because `chat` only needs _at least_ `Interact`
and `UserStore` and it can ignore `IOE`. Then, elimination functions
like `runTeletypeIO` can rely on `IOE` still being available.

In `main`, we eliminate `Interact` and `UserStore`, as in our tests,
and get to `Eff [IOE] ()`, which we can turn into a simple IO action
using `runIOE`.

# Conclusion

This concludes the tour. Hopefully you now have enough context to poke
around the project a little and get a feel for it. From here you might
do [some exercises](exercises.md) or [analysis on developer
experience](developer-experience.md).
