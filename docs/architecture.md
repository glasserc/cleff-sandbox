In this document, I try to collect some advice about architecture and
healthy use of `cleff`, based in part on practitioners' experience
with other similar systems.

# When to create an effect?

Evan Relf (@evanrelf) flags an antipattern, which is effects that only
have one implementation. This is analogous to interfaces that only
have one implementation in languages which have those. Having the
effect doesn't really add anything here. Instead, Evan's rule of thumb
is that you should only create an effect when there are at least two
possible interpretations of an effect. This doesn't mean that multiple
interpretations need to necessarily be implemented or even ready to
implement, but it should be clear what the interpretations are and why
it's helpful to have them.

# When to re-use an existing effect?

`cleff` comes with a bunch of built-in effects for things like `Input`
and `Output`. When defining an effect, you may be struck by the
resemblance to one of these built-in effects -- for example, our
example `Log` effect is isomorphic to `Output String`. It can be fine
to use an existing effect if it fits, but if you have some specific
domain meaning associated with your effect, it can be good to create a
new one. An analogy might be something like `Either` or `Maybe` versus
a custom type specific to the domain you are working in. This is
Haskell, types are cheap.

# Who should create effects?

Creating an effect can be an architectural decision. Michael Mroz
(@mjvmroz) describes creating an effect as like drawing an
architectural boundary in marker. Once that line is drawn, it becomes
harder to move, and it becomes harder to switch something from one
side of the line to the other. Similarly, moving operations from one
effect to another means rearranging the dependencies of every piece of
code that uses either effect, which can be very tedious.

If an effect is just used locally to one module or within one
subsystem, then it's probably fine, but it's probably better to be
more careful when defining effects at a system level.

# How big should effects be?

Evan Relf advises effects that have relatively coarse granularity. He
argues that there is an ergonomic impact of having too _many_ effects,
which is that people get tired of meticulously typing the names of
every effect they use. Instead they end up defining aliases for
collections of effects and use them everywhere. This ends up defeating
the whole purpose of using something like `cleff`, which is to help
represent in the type system the effects that are actually being used,
so you know which implementations are necessary to provide.

Put another way, Evan points out that ergonomics are an important part
of using `cleff` and should be considered carefully.

# Should effects "stack"?

Let's say you have a third-party API that you use to implement some
contract for your application. As an example, let's take Auth0, which
you make requests to over HTTP, and which you hope to use as your
authentication provider, which in the context of your application is
reified as some subsystem that manages users. What kinds of effects
should you have and how should they relate?

It seems reasonable to have an effect for HTTP, representing the
ability to make requests and receive responses from any third party
API. You may want to define an Auth0 effect, which represents a
higher-level concept, the ability to make Auth0 requests and receive
Auth0 responses. Finally, you almost certainly want some kind of
`UserStore` effect that represents the contract that your application
actually needs.

The implementation of `UserStore` may depend on your Auth0 effect, and
the implementation of the `Auth0` effect may depend on your HTTP
effect, but from the perspective of some feature that involves users,
these are just implementation details.

Having an Auth0 effect may be helpful if you want to test your
`UserStore` implementation and don't want to break it down all the way
to HTTP requests/responses. But since your application only cares
about access to the `UserStore`, this is an implementation choice that
can be decided by your `UserStore` implementation.

# What makes a good effect?

Michael Mroz suggests that a good effect has an internally consistent
contract. Sometimes people in Haskell talk about typeclasses being
"lawful", and I think there's something similar to be argued for
effects.
