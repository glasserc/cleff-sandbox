One concern about using a library like `cleff` is how much it impacts
the developer experience. `cleff` uses some advanced GHC features,
such as GADTs and type-level lists, and therefore can feel very
complicated and advanced. I don't want to advocate for anything that
deepens the learning curve or raises a barrier to entry for people who
aren't as confident in Haskell. In this document we will examine some
of the impacts when using `cleff` as opposed to using other
approaches.

# GADTs

Using `cleff` necessitates using the `GADTs` extension. However, the
main thing this introduces is a new syntax. The use of this extension
is pretty limited in a program that uses `cleff` -- not many people
are going to be creating effects. (See [architecture](architecture.md)
for more on this point.) The developer experience impact of this is
minor, especially if your program is already using GADTs somewhere
else.

# Type-level lists

Type-level lists are a language feature that's a little fancier. Here
again, while the use of type-level lists may be confusing, the blast
radius is pretty small. Most people on your project are not going to
need to work with type-level lists -- the main use is in
"interpretation" functions, when you are popping the effect being
interpreted off the stack.

Both GADTs and type-level lists should be considered in comparison
with the amount and kind of code that would be necessary to build this
kind of abstraction boundary without something like `cleff`. The
obvious alternative is something like MTL. To work effectively with
MTL, the programmer needs to be comfortable with monad transformers
(itself a complicated concept) and also be willing to write lots of
instances to "pass through" behaviors. It's not clear to me that
type-level lists are that much more fancy.

# Indirection

There is something fundamental about what we are doing which is
separating what we want to do from how we want to do it. This is
essential to all the benefits we are looking for -- increased
testability and better separation of subsystems require this
separation. This isn't even something unique to `cleff` -- we see this
with all of the tools and approaches we might use (see [a
comparison](comparison.md)).

This separation necessarily introduces a layer of indirection. If you
want to understand the system as a whole, you have to look at both
parts, and this could involve jumping back and forth, as control flow
goes from "what we want to do" to "how we want to do it" and back.

Those of us who have had the misfortune of getting caught in
Enterprise Java™ back in the day may be experiencing a crawling
sensation around now. In those days, it was common to have every
single class associated with an interface, except those that didn't,
and reading the code was uncomfortably like floating through a
dreamscape where nothing was connected to anything else.

When I speak to practitioners who have used libraries like `cleff`,
they don't seem especially concerned about this possibility. One theme
I've heard multiple times is that indirection by itself is bad, but
abstraction is good, and indirection is necessary but not sufficient
for abstraction. We accept indirection because it lets us build good
abstractions, and when we have good abstractions, we end up not having
to jump back and forth a lot; instead, we are able to focus on one
area, relying on the abstraction to insulate us from what's happening
on the other side.

It's not really clear to me why something like `cleff`, which seems
powerful and cool, would feel so differently from the dependency
injection that I saw back in the Enterprise Java™ mines. I have some
theories:

1. Perhaps Haskell is simply a better language than Java. In
   particular, the whole interface-implementation pattern in Java
   requires a lot more ceremony. In Haskell it's just a few lines. So
   maybe even if the problem is equally as bad in Haskell, the
   ergonomics are better.
2. Perhaps we can use the tools better in Haskell, in part because we
   understand them better.  For example, when writing Haskell it's
   very clear what code is pure and what code is effectful. It doesn't
   make sense to use `cleff` for pure code; we can use property tests
   for pure code, and once we are confident in it, we can use it
   directly without having to inject false results into it. Or:
   perhaps we understand that we should have effects at system
   boundaries rather than module or (Java) class boundaries.
3. Perhaps even in Haskell we run the risk of ending up in the same
   place, and that especially if things feel disconnected in this way,
   it's because we haven't put our abstraction boundaries in the right
   place, and we should revisit them and rearrange them.
4. Perhaps the tools are even more dangerous in Haskell because they
   are more ergonomic. Perhaps we run the risk of Enterprise Java™ all
   the more in Haskell and we need to be extra careful.

So far, I'm taking it on faith that the Enterprise Java™ nightmare is
avoidable. In the end, all I know is that we need something different
from what we have.

# Compiler errors

Hopefully your impression from looking at code that uses `cleff` is
fairly readable, but that doesn't tell you anything about how hard it
was to arrive there. To try to explore this, I tried to make some
intentional errors to see what `cleff` would say (plus of course some
unintentional errors that sprung up organically).

## A program without the right constraints

I removed the `Interact :> es` constraint from
`refreshUserFromInteract`, leaving the type signature
`refreshUserFromInteract :: UserRecord -> Eff es UserRecord`. GHC gave
me this error:

```
.../cleff-sandbox/src/SampleProgram.hs:39:3: error: [GHC-39999]
    • No instance for ‘Interact :> es’ arising from a use of ‘display’
    • In the first argument of ‘($)’, namely ‘display’
      In a stmt of a 'do' block:
        display $ "Good to see you again, " ++ userRecord.name ++ "!"
      In the expression:
        do display $ "Good to see you again, " ++ userRecord.name ++ "!"
           let airplanePrompt = ...
           plane <- promptYesOrNo airplanePrompt
           respondToRefreshedData userRecord.flies plane
           ....
   |
39 |   display $ "Good to see you again, " ++ userRecord.name ++ "!"
   |   ^^^^^^^
```

This seems pretty straightforward.

I also tried adding an extraneous constraint to the program. When I tried
`refreshUserFromInteract :: (UserStore :> es, Interact :> es) => UserRecord -> Eff es UserRecord`, I got the warning:

```
.../cleff-sandbox/src/SampleProgram.hs:37:28: warning: [GHC-30606] [-Wredundant-constraints]
    Redundant constraint: UserStore :> es
    In the type signature for:
         refreshUserFromInteract :: forall (es :: [Effect]).
                                    (UserStore :> es, Interact :> es) =>
                                    UserRecord -> Eff es UserRecord
   |
37 | refreshUserFromInteract :: (UserStore :> es, Interact :> es) => UserRecord -> Eff es UserRecord
   |                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

This also makes sense and seems reasonable to me.

On the other hand, when I used `:>>`, as in `refreshUserFromInteract
:: ('[UserStore, Interact] :>> es) => UserRecord -> Eff es
UserRecord`, GHC didn't complain about this at all. This seems
suboptimal. In this case, GHC identifies that the constraint is being
"used", but it's too wide, and GHC is not able to detect that. We are
avoiding `:>>` anyhow (see the sidebar in the [guided
tour](guided-tour.md), but this is another reason that you might want to avoid it.

## Interpreter with wrong operations

What happens if you accidentally try to use the wrong effects when
defining an interpreter? I tried making reference to a garbage `Foo` constructor in `runInteractTeletype`:

```haskell
runInteractTeletype :: Eff (Interact : es) a -> Eff (Teletype : es) a
runInteractTeletype = reinterpret \case
  PromptText s -> writeTTY s >> readTTY
  PromptYesOrNo s -> writeTTY s >> readUntilYesOrNo
  Display s -> writeTTY s
  Foo s -> writeTTY s
```

GHC says:

```
.../cleff-sandbox/src/Effects/Interact.hs:36:3: error: [GHC-76037]
    Not in scope: data constructor ‘Foo’
   |
36 |   Foo s -> writeTTY s
   |   ^^^
```

OK. What about something weirder? I tried adding an interpretation of
`WriteTTY`, which is already in scope. Something like this:

```haskell
runInteractTeletype :: Eff (Interact : es) a -> Eff (Teletype : es) a
runInteractTeletype = reinterpret \case
  PromptText s -> writeTTY s >> readTTY
  PromptYesOrNo s -> writeTTY s >> readUntilYesOrNo
  Display s -> writeTTY s
  WriteTTY s -> writeTTY s
```

This gets me the error:

```
.../cleff-sandbox/src/Effects/Interact.hs:36:3: error: [GHC-83865]
    • Couldn't match type ‘Teletype’ with ‘Interact’
      Expected: Interact (Eff esSend) a1
        Actual: Teletype (Eff esSend) a1
    • In the pattern: WriteTTY s
      In a \case alternative: WriteTTY s -> writeTTY s
      In the first argument of ‘reinterpret’, namely
        ‘\case
           PromptText s -> writeTTY s >> readTTY
           PromptYesOrNo s -> writeTTY s >> readUntilYesOrNo
           Display s -> writeTTY s
           WriteTTY s -> writeTTY s’
   |
36 |   WriteTTY s -> writeTTY s
   |   ^^^^^^^^^^
```

This isn't the most clear (since there are these extra type arguments
`Eff` and `a1`) but the difference between `Interact` and `Teletype`
is pretty clear.

## Interpreters without correct effects

Most of the helpers `interpret`, `reinterpret`, `interpretIO`
etc. make it pretty obvious what constraints you need. I tried
stripping the `Teletype` effect out of the result type in this:

```haskell
runInteractTeletype :: Eff (Interact : es) a -> Eff es a
runInteractTeletype = reinterpret \case
  PromptText s -> writeTTY s >> readTTY
  PromptYesOrNo s -> writeTTY s >> readUntilYesOrNo
  Display s -> writeTTY s
```

I got:

```
.../cleff-sandbox/src/Effects/Interact.hs:32:23: error: [GHC-25897]
    • Couldn't match type ‘es’ with ‘e'0 : es’
      Expected: Eff (Interact : es) a -> Eff es a
        Actual: Eff (Interact : es) a -> Eff (e'0 : es) a
      ‘es’ is a rigid type variable bound by
        the type signature for:
          runInteractTeletype :: forall (es :: [(* -> *) -> * -> *]) a.
                                 Eff (Interact : es) a -> Eff es a
        at src/Effects/Interact.hs:31:1-56
    • In the expression:
        reinterpret ...
```

This isn't ideal -- it doesn't tell me what effect I need, but I
definitely need some new effect.

The most complicated interpreter in the project so far is
`runTeletypePure`. I tried messing with that function to see how bad I
could make it. First I had to delete the type signatures (often, you
can get worse error messages if you don't have type signatures to
constrain types), then I tried using `reinterpret` instead of
`reinterpret2`, deleting the `dischargeInput` stanza:

```haskell
runTeletypePure :: [String] -> Eff (Teletype : es) a -> Eff es (a, [String])
runTeletypePure tty =
  dischargeOutput
    . convertTeletypeToInputAndOutput
  where
    convertTeletypeToInputAndOutput =
      reinterpret \case
        ReadTTY -> fromMaybe "" <$> input
        WriteTTY msg -> output msg
    dischargeOutput =
      fmap (\(a, outputs) -> (a, reverse outputs))
        . runState []
        . outputToListState
```

This got some interesting ones:

```
.../cleff-sandbox/src/Effects/Teletype.hs:36:37: error: [GHC-43085]
    • Overlapping instances for Input (Maybe [Char]) :> (e' : es1)
        arising from a use of ‘input’
      Matching instance:
        instance (e :> es) => e :> (e' : es)
          -- Defined in ‘Cleff.Internal.Rec’
      Potentially matching instance:
        instance [overlapping] e :> (e : es)
          -- Defined in ‘Cleff.Internal.Rec’
      (The choice depends on the instantiation of ‘e', es1’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
    • In the second argument of ‘(<$>)’, namely ‘input’
      In the expression: fromMaybe "" <$> input
      In a \case alternative: ReadTTY -> fromMaybe "" <$> input
   |
36 |         ReadTTY -> fromMaybe "" <$> input
   |                                     ^^^^^

.../cleff-sandbox/src/Effects/Teletype.hs:37:25: error: [GHC-43085]
    • Overlapping instances for Output String :> (e' : es1)
        arising from a use of ‘output’
      Matching instance:
        instance (e :> es) => e :> (e' : es)
          -- Defined in ‘Cleff.Internal.Rec’
      Potentially matching instance:
        instance [overlapping] e :> (e : es)
          -- Defined in ‘Cleff.Internal.Rec’
      (The choice depends on the instantiation of ‘e', es1’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
    • In the expression: output msg
      In a \case alternative: WriteTTY msg -> output msg
      In the first argument of ‘reinterpret’, namely
        ‘\case
           ReadTTY -> fromMaybe "" <$> input
           WriteTTY msg -> output msg’
   |
37 |         WriteTTY msg -> output msg
   |                         ^^^^^^
```

Here, `reinterpret` is supposed to add only one new effect (`e'`) to
my stack (`es1`), and GHC can't figure out what effect it's supposed
to be. Instead of successfully unifying with a single effect type, it
seems to be keeping it polymorphic, which means it can't find an
instance. It unhelpfully suggests `IncoherentInstances`. This is
pretty bad.

Giving it a little more structure by putting a type signature on
`convertTeletypeToInputAndOutput` helps a lot:

```haskell
convertTeletypeToInputAndOutput :: Eff (Teletype : es) a -> Eff (Output String : es) a
convertTeletypeToInputAndOutput =
  reinterpret \case
    ReadTTY -> fromMaybe "" <$> input
    WriteTTY msg -> output msg
```

```
.../cleff-sandbox/src/Effects/Teletype.hs:37:37: error: [GHC-39999]
    • Could not deduce ‘Input (Maybe [Char]) :> es1’
        arising from a use of ‘input’
      from the context: Handling esSend Teletype (Output String : es1)
        bound by a type expected by the context:
                   Handler Teletype (Output String : es1)
        at src/Effects/Teletype.hs:(36,19)-(38,34)
      or from: a2 ~ [Char]
        bound by a pattern with constructor:
                   ReadTTY :: forall (m :: * -> *). Teletype m String,
                 in a \case alternative
        at src/Effects/Teletype.hs:37:9-15
    • In the second argument of ‘(<$>)’, namely ‘input’
      In the expression: fromMaybe "" <$> input
      In a \case alternative: ReadTTY -> fromMaybe "" <$> input
   |
37 |         ReadTTY -> fromMaybe "" <$> input
   |                                     ^^^^^
```

## Not using enough interpreters

I tried dropping the `Interact` interpreter from `main`:

```haskell
main :: IO ()
main =
  runIOE
    . runUserStorePureDiscardResult mempty
    $ chat --    chat has type Eff '[Interact, UserStore, IOE] ()
```

```
.../cleff-sandbox/app/Main.hs:11:7: error: [GHC-64725]
    • The element 'Interact' is not present in the constraint
    • In the second argument of ‘($)’, namely ‘chat’
      In the expression:
        runIOE . runUserStorePureDiscardResult mempty $ chat
      In an equation for ‘main’:
          main = runIOE . runUserStorePureDiscardResult mempty $ chat
   |
11 |     $ chat --    chat has type Eff '[Interact, UserStore, IOE] ()
   |       ^^^^
```

This might be a little confusing, since actually the constraint (as
shown on `chat`) actually does include `Interact`. What's happening
here is that GHC is inferring types "from the ground up", using
`runIOE` and `runUserStorePureDiscardResult` to end up with the type
`Eff '[UserStore, IOE] ()`, and that isn't compatible with `chat`. This
type error is actually something `cleff` [explicitly
provides](https://github.com/re-xyr/cleff/blob/c71c09c8c77c804c9fe206a4704546d4140c8f90/src/Cleff/Internal/Stack.hs#L133-L137),
presumably because the vanilla type error from GHC is even worse.

## Using interpreters in the wrong order

This is one I discovered for myself.

```haskell
main :: IO ()
main =
  runIOE
    . runTeletypeIO
    . runUserStorePureDiscardResult mempty
    . runInteractTeletype
    $ chat --    chat has type Eff '[Interact, UserStore, IOE] ()
```

```
.../cleff-sandbox/app/Main.hs:13:7: error: [GHC-64725]
    • The element 'UserStore' is not present in the constraint
    • In the second argument of ‘($)’, namely ‘chat’
      In the expression:
        runIOE
          . runTeletypeIO
              . runUserStorePureDiscardResult mempty . runInteractTeletype
          $ chat
      In an equation for ‘main’:
          main
            = runIOE
                . runTeletypeIO
                    . runUserStorePureDiscardResult mempty . runInteractTeletype
                $ chat
   |
13 |     $ chat --    chat has type Eff '[Interact, UserStore, IOE] ()
   |       ^^^^
```

This was confusing to me, especially the location being indicated
here. Placing the `UserStore` interpreter between the translation from
`Interact -> Teletype` and the `Teletype` interpreter is clearly wrong
-- `runInteractTeletype` says that the result should have a `Teletype`
as its first effect, and `runUserStorePureDiscardResult` says it
should have `UserStore` as its first effect, and those are
inconsistent -- but why does that manifest as a type error on `chat`?
I think what's happening is that GHC infers a type of `Eff (Interact :
es) ()`, and since it can't deduce `UserStore :> es`, it gives this
error. This is a little misleading, even for an experienced engineer.

## Ambiguous effects

Cleff comes with some "generic" effects (`Input`, `Output`, etc.)
which work with any type (so you can have `Input String` if your
program reads strings, `Input Int` if your program reads integers
etc.). I've written programs where it's not possible to completely
infer the effect stack I'm trying to write code against. For example,
let's say I use `interpret` instead of `reinterpret` in defining
`runInteractTalker`:

```haskell
runInteractTalker :: (Output String :> es) => Talker -> Eff (Interact : es) a -> Eff es a
runInteractTalker talker = interpret \case
  PromptText s -> output s >> pure (talker.respondText s)
  PromptYesOrNo s -> output s >> pure (talker.respondYesNo s)
  Display s -> output s
```

At the call site (`SampleProgramSpec`, for example), I have this:

```haskell
let (_, users) =
      runPure
        . runUserStorePure mempty
        $ ignoreOutput
        $ runInteractTalker newTalker chat
```

Here, `ignoreOutput :: Eff (Output o : es) a -> Eff es a`. But what
type is `o`? Here I meant for it to be `String`, but GHC doesn't know;
it knows that there is an `Output String` somewhere in `es`, but it
could be somewhere else in `es` towards the bottom; maybe there is
some other `Output` that we want to ignore first. GHC gives this error:

```
.../cleff-sandbox/test/SampleProgramSpec.hs:97:17: error: [GHC-43085]
    • Overlapping instances for Output String
                                Cleff.Internal.Rec.:> [Output o1, UserStore]
        arising from a use of ‘runInteractTalker’
      Matching instance:
        instance (e Cleff.Internal.Rec.:> es) =>
                 e Cleff.Internal.Rec.:> (e' : es)
          -- Defined in ‘Cleff.Internal.Rec’
      Potentially matching instance:
        instance [overlapping] e Cleff.Internal.Rec.:> (e : es)
          -- Defined in ‘Cleff.Internal.Rec’
      (The choice depends on the instantiation of ‘o1’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
    • In the second argument of ‘($)’, namely
        ‘runInteractTalker newTalker chat’
      In the second argument of ‘($)’, namely
        ‘ignoreOutput $ runInteractTalker newTalker chat’
      In the expression:
        runPure . runUserStorePure mempty
          $ ignoreOutput $ runInteractTalker newTalker chat
   |
97 |               $ runInteractTalker newTalker chat
   |                 ^^^^^^^^^^^^^^^^^
```

This is a little confusing. `Output o1` seems like what I want, but it
isn't able to figure out that it's the same as the `Output String` I
was talking about earlier. Like some ambiguous type situations in
Haskell, it's totally possible to work around this using
`TypeApplications` (if you do `ignoreOutput @String`, then that
indicates that the next effect must be `Output String`, which
satisfies the `Output String :> es` constraint from
`runInteractTalker`). In this case I use `Output String` as the
topmost effect in the result type, which works fine since there's no
expectation that `Output String` is going to be present elsewhere in
the stack. I'm still figuring out when to use `interpret` and
`reinterpret`, and this feels like a data point in that decision.
