# The Amy Programming Language

Amy is a strict, Haskell-like language that compiles to LLVM.

## Features

* **Powerful type system**: Static typing helps ensure programs are correct and
  safe. Type systems should help you write software, not get in your way.
* **Focus on simplicity and ease of learning**: This is not a grab-bag of
  functional programming paradigms. The language focuses on being small and
  easy to learn, yet still powerful.
* **Strict evaluation**: Strict evaluation makes performance easier to reason
  about, and it also makes it easier to write a compiler that produces
  performant code.
* **Compiles to LLVM**: Compilation to LLVM allows us to reap the benefits of
  the LLVM toolchain, including advanced optimizations and targeting multiple
  backends. This frees compiler developers to focus on the compiler frontend
  instead of low-level details like register allocation and backend code
  generation.
