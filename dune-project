(lang dune 3.0)
(name randomV)
(version 1.0.0)

(generate_opam_files true)

(strict_package_deps true)
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "Florian Angeletti, <octa@polychoron.fr>")
(authors "Damien Doligez" "Xavier Leroy")
(source (github ocaml/randomV))

(package
 (name randomV)
 (depends (cppo :dev) (ocaml (>= 4.08.0)))
 (synopsis "Versioned Random module from the OCaml 5 standard library")
 (description
"The randomV package provides implementations of the all implementations
of the Random module ever used in the OCaml standard library.")
)
