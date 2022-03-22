(lang dune 3.0)
(name randomV)

(generate_opam_files true)

(strict_package_deps true)
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "Florian Angeletti, <octa@polychoron.fr>")
(authors "Damien Doligez, Xavier Leroy")
(source (github ocaml/randomV))

(package
 (name random5)
 (synopsis "OCaml 5 Random generator compatibility layer")
 (description "The random5 package provides support the new implementation of the Random module in OCaml 5.0, independently
of the version of your compiler.")
)

(package
 (name random4)
 (synopsis "OCaml 4 Random generator compatibility layer")
 (description "The random4 package provides support for the implementation of the Random module used in OCaml 4.*, independently
of the version of your compiler."))
