(lang dune 3.0)
(name randomV)

(generate_opam_files true)

(strict_package_deps true)
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "Florian Angeletti, <octa@polychoron.fr>")
(authors "Damien Doligez, Xavier Leroy")
(source (github Octachron/RandomV))

(package
 (name random5)
 (synopsis "OCaml 5 Random generator compatibility layer")
 (description "random5 allows you to depend on the new PRNG introduced in OCaml 5.0, independently
of the version of your compiler.")
)

(package
 (name random4)
 (synopsis "OCaml 4 Random generator compatibility layer")
 (description "random4 allows you to depend on the PRNG used by OCaml 4 (from 4.00.0 to 4.14), independently
of the version of your compiler.")
)
