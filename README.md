# Compatibility library for Random number generation

This library provides access to the various implementation of the Random module
from the OCaml standard library independently of the compiler version.

## Signature compatibility

Whenever possible, the signature of the the older Random module has been
extended with functions that were added in newer versions.

Currently, this means that all `Random*` modules before `v5` shares the exact same type
whereas the Random5 module is the only to define a `split` function since
previous PRNGs are not splittable.

## stdlib-random.v5 library

The `stdlib-random.v5` library uses the same LXM pseudo-random number generator as
the one used in OCaml 5.

For versions of OCaml that support multiple domains, the global PRNG of this library
is a domain local state rather than a global state to avoid any potential contention
issue on this global PRNG.
Moreover, whenever a new domain is spawn, its global PRNG is split from the
PRNG of the parent domain which ensures that both PRNGs are essentially
independent.

## stdlib-random.v5o library

The `stdlib-random.v5o` library provides a pure OCaml alternative to
the `stdlib-random.v5` library which uses a C implementation when updating the
PRNG state.

## stdlib-random.v4 library

The `stdlib-random.v4` library exposes the  lagged-Fibonacci F(55, 24, +) PRNG with a modified addition function used from OCaml 3.12 to OCaml 4.14 .


## stdlib-random.v3 library

The `stdlib-random.v3` library exposes the  lagged-Fibonacci F(55, 24, +) PRNG used
from OCaml 3.07 to OCaml 3.11.

Note that this library prioritizes compatibility over correctness.
In particular, the state of this generator is not marshallable across
architecture with different word size.
It is thus advisable to use this PRNG only when compatibility with OCaml 3 PRNG is a
strict requirement.
