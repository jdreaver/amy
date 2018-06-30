# Amy Type Checker

This document explains the Amy type checking algorithm, and the work that
inspired it.

## Papers

The Amy type checking algorithm is based on a few different papers:

* [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism
  (Dunfield 2013)](https://www.cl.cam.ac.uk/~nk480/bidir.pdf): This paper
  describes the core of the Amy type checking algorithm. Everything else is
  essentially added onto this core.
* [Extensible records with scoped labels (Leijen
  2005)](http://www.cs.ioc.ee/tfp-icfp-gpce05/tfp-proc/21num.pdf): This paper
  describes a more thorough extensible implementation than the one we use (we
  actually remove some features from this system).
* [Typing Haskell in Haskell (Jones
  2000)](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf): This paper mainly
  provided details about typing mutually recursive binding groups and binding
  group dependency analysis. This useful information is surprisingly absent
  from many academic type checking papers.
