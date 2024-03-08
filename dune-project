(lang dune 2.7)
(name stdlib-random)
(version 1.2.0)

(generate_opam_files true)

(strict_package_deps true)
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "Florian Angeletti, <octa@polychoron.fr>")
(authors "Damien Doligez" "Xavier Leroy")
(source (github ocaml/stdlib-random))

(package
 (name stdlib-random)
 (depends (cppo (>= 1.1.0)) (ocaml (>= 4.08.0)))
 (synopsis "Versioned Random module from the OCaml standard library")
 (description
"The stdlib-random package provides a stable and compiler-independent
implementation of all the PRNGs used in the Random module.
Those PRNGs are available in the various libraries:
- stdlib-random.v3: OCaml 3.07 to 3.11 PRNG
- stdlib-random.v4: OCaml 3.12 to 4.14 PRNG
- stdlib-random.v5: current OCaml 5.0 PRNG
- stdlib-random.v5o: pure OCaml version of the OCaml 5 PRNG
All those libraries can be used together and the signature of their
Random$n module has been extended to the latest signature whenever possible.
")
)
