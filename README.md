# Compatibility library for Random number generation

This library provides access to the various implementation of the Random module
from the OCaml standard library independently of the compiler version.


## Random5 package

The `random5` package uses the same LXM pseudo-random number generator as
the one used in OCaml 5.


### Random5.ocaml package

This package provides a pure OCaml alternative to the `random5` package which uses a C implementation
when updating the PRNG state.

## Random4 package

The `random4` package exposes the  lagged-Fibonacci F(55, 24, +) with a modified addition
function used from OCaml 3.12 to OCaml 4.14 .

