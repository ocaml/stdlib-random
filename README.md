# Compatibility library for Random number generation

This library provides access to the various implementation of the Random module
from the OCaml standard library independently of the compiler version.

## stdlib-random.v5 subpackage

The `stdlib-random.v5` package uses the same LXM pseudo-random number generator as
the one used in OCaml 5.


### stdlib-random.v5o subpackage

The `stdlib-random.v5o` subpackage provides a pure OCaml alternative to the `random5` package which uses a C implementation when updating the PRNG state.

## stdlib-random.v4 subpackage

The `stdlib-random.v4` subpackage exposes the  lagged-Fibonacci F(55, 24, +) PRNG with a modified addition function used from OCaml 3.12 to OCaml 4.14 .


## stdlib-random.v3 subpackage

The `stdlib-random.v3` package exposes the  lagged-Fibonacci F(55, 24, +) PRNG used from OCaml
3.07 to OCaml 3.11 . Note that this module prioritizes compatibility over correctness.
In particular, the state of this generator is not marshallable across
architecture with different word size.
It is thus advisable to use this PRNG only when compatibility with OCaml 3 PRNG is a
strict requirement.

# Signature compatibility

Whenever possible, the signature of the the older Random module has been
extended with functions that were added in newer versions.

Currently, this means that all `Random*` modules before `v5` shares the exact same type
whereas the Random5 module is the only to define a `split` function since
previous PRNGs are not splittable.
