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
 (name random5)
 (synopsis "OCaml 5 Random generator compatibility layer")
 (description
"The random5 package provides an implementations of the Random module
of OCaml 5, independently of the version of the OCaml compiler.
A pure OCaml implementation of the module is also provided.")
)


(package
 (name random4)
 (synopsis "OCaml 4 Random generator compatibility layer")
 (description
"The random4 package provides an implementation of the Random module of
OCaml 4, independently of the version of the OCaml compiler.")
)
