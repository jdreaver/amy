# Runtime System

Amy compiles to LLVM. One of the main goals is to take advantage of LLVM
optimizations, and we can do that by using idiomatic LLVM. That means we
shouldn't go crazy writing a ton of runtime system code to handle stacks, the
heap, registers, etc. Code not written is code not maintained!

Of course, we inevitably need _some_ runtime code. This document describes it.

## Garbage collection

We currently punt this to the [Boehm Garbage
Collector](http://www.hboehm.info/gc/). Using this garbage collector is just
too easy:

* Link it into compiled programs with Clang via `-lgc`
* Replace any uses of `malloc` with `GC_malloc`

In the future we will need much more sophisticated accurate garbage collection,
but this should do for now.

Luckily Amy is strict. We will certainly do a lot less heap allocation than
e.g. GHC/Haskell, which needs to do tons of allocations for thunks. Some
allocation is totally unavoidable, as long as the language supports partial
application and closures. Ideally the compiler can minimize the amount of heap
allocation it has to do.

## Closure conversion, eval/apply

The closure conversion algorithm is inspired by the great paper [How to make a
fast curry: push/enter vs eval/apply (Marlow/SPJ
2004)](https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/).

The key takeaways from this paper and how they apply to our closure conversion
system are:

* You can't look at a function type signature and know its arity. A function of
  type `Int -> Int -> Int` could very well take two `Int`s and return another.
  Or, it could take one `Int` and return a function that takes an `Int` and
  returns an `Int`. It could even take zero arguments and just return a
  function of `Int -> Int -> Int`!
* You can't look at a function's type and know if it is partially applied or
  not.
* With the two previous points in mind, if we are applying arguments to an
  unknown function (i.e. a function where we only know the type, like when a
  function is passed as an argument to another function), **we can't know
  exactly how many arguments to apply.**

We use the eval/apply model of closure evaluation. In this model, we wrap up
all closures in a struct that holds a pointer to the function, the function's
arity, and already-applied arguments to the function (if any). When applying an
unknown function, we first add all the arguments to the closure's array of
arguments. Then, we check the length of the argument array against the arity:

* If there are too many arguments, then we first apply the correct amount to
  the function, and then we apply the remaining arguments to the function's
  return value.
* If there are just enough arguments, call the function and return the result.
* If there are not enough arguments, then just return the closure as-is since
  it is partially applied.

There are some key implementation points to be made:

* Known function calls can be optimized as a normal LLVM `call` to a known
  global address. However, pretty much every other use of a function is assumed
  to be working with a closure. In particular, every time we use a function as
  an argument or return a function from a function, we wrap that sucker up in a
  closure.
* All values need to fit in however many bits we allocate for each element of
  the argument array in closures (currently 64 bits via `int64_t` in C). This
  might be made much more complicated once we have precise garbage collection.
* It is very possible packing arguments into arrays and then unpacking them is
  orders of magnitudes slower than keeping the latest arguments in registers
  like GHC does.
  1. The main strategy here is to hopefully optimize away closures in
     performance sensitive code.
  2. The current implementation is extremely simple, and we don't need to
     generate dozens of premade functions for different call patterns like GHC
     does. For now simplicity wins.
  3. Once we have a respectable benchmark suite and regression tests, we can
     optimize this much more.
