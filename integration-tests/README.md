# Amy Integration Test Suite

This directory holds the integration test suite for the `amy` compiler.

## Overview

* Tests are defined in `tests.yaml`
* Tests can technically be placed anywhere, but by convention we put passing
  tests under `pass`, and failing tests under `fail`. That way we can point
  users to passing tests as examples of `amy` programs, not failing tests.
* `Main.hs` is the program that actually runs the tests. **It assumes that the
  `amy` compiler is already built**.

## Running tests

The easiest way to run the test suite is via `make test` in the root of this
repo. That ensures the compiler is built, and then it runs the tests.
