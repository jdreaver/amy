# Differences from Haskell

Amy is heavily inspired by Haskell and Haskell-like languages (like
Purescript), as well as languages from the ML family (including OCaml).

> **NOTE**: Amy is under heavy development, and many of the features
> highlighted here might not be complete yet. See the [roadmap](roadmap.md) for
> the current status of any in-progress features.

## Criticisms of Haskell

Haskell is a lazy functional programming language with a best-in-class type
system. It holds a unique position in the programming language design space,
and despite its age it has evolved remarkably well, producing numerous original
contributions to programming languages in general.

Haskell has been known as a hotbed of language and compiler research, and has
accrued lots of new extensions and features during its lifetime. For better or
for worse, Haskell also has to live with decisions that were made decades ago
in the interest of backwards compatibility, decisions that might not live up to
the lens of 20/20 hindsight today.

We have the privilege of standing on the shoulders of giants, taking the best
features of Haskell, combining them with unique new features, and culling any
language design decisions that we don't like.

### "Why does Haskell, in your opinion, suck?"

There was a series of threads online in 2016 with variations on the theme of
"What sucks about Haskell?".

* Reddit: https://www.reddit.com/r/haskell/comments/4f47ou/why_does_haskell_in_your_opinion_suck/
* Also Reddit: https://www.reddit.com/r/haskell/comments/4sihcv/haskell_the_bad_parts/
* Hacker News: https://news.ycombinator.com/item?id=11513883

While some comments there are a bit overzealous, there are nuggets of honest,
constructive criticisms that inspired some of the directions taken in Amy. Feel
free to read through those threads.

In the rest of this document we will cover the specific differences between Amy
and Haskell.

## Strict Evaluation

One key feature of Haskell is lazy evaluation by default. Values in Haskell are
computed when they are needed, not when they are defined. Certainly, laziness
is mind-blowing when you first learn about it. Laziness allows you to more
easily compose algorithms as well. Laziness has greatly influenced the
development of Haskell, and pretty much forced Haskell to stick to purity.

Criticisms of lazy evaluation are all over the internet, but we highlight a few
here:

* Reasoning about the space/time complexity of code is more difficult.
* Writing a lazy compiler that produces performant code is harder. (GHC is a
  trailblazer in this space, and the optimizations it produces are astounding).
* Interop with other languages that are not lazy is tougher.
* This is a rather minor criticism, but laziness is something that is foreign
  to most programmers, and the pitfalls of laziness are yet another thing to
  learn.

## Extensible Records

Haskell's record system is rather rudimentary, and is a very commonly
criticized component of the language. Amy uses a real extensible record system.

TODO: Flesh this out with an example

## Lack of advanced type system features

Amy is focused on being **easy to learn** without sacrificing **power**. The
language is kept as small as possible. This prevents proliferation of too many
different idioms for doing the same thing. It also keeps the compiler smaller,
easier to maintain, fast, and easier to optimize.

It's really cool that Haskell's type system allows library authors to build
libraries for things like extensible records, lenses for record updates, etc.
However, in the interest of ease-of-use, learning, and quality error messages,
we prefer to bake these into the language itself.

## Module system

> **NOTE**: This module system in particular is a work in progress.

## Explicit `forall`

TODO: Flesh this out
