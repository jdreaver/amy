# Getting started with the compiler

This document explains how to get started hacking on the `amy` compiler.

## Requirements

* Haskell development environment, preferably `stack`
* LLVM development libraries, version 6 (`llvm-6.0-dev` on Ubuntu, see
  http://apt.llvm.org/)
* A recent version of `clang`, presumably the version packed with the LLVM
  libraries you installed (`clang-6.0` on Ubuntu)
* [Boehm Garbage Collector](http://www.hboehm.info/gc/) development libs.
  (`libgc-dev` on Ubuntu)

## Compiling the compiler

Running `make test` should build the `amy` compiler and run all the integration
tests. If you are missing a library like LLVM or `libgc`, surely this process
will complain.
