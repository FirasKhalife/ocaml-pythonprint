(lang dune 3.14)
(name python-pp)
(generate_opam_files true)
(maintainers "khalifefiras29@gmail.com")
(authors "Firas Khalife")
(source (github FirasKhalife/ocaml-pythonprint))
(version dev)

(package
 (name python-pp)
 (depends
  (dune (>= 3.6.1))
  (ocaml (>= 4.09.0)))
 (synopsis "The Coq Proof Assistant -- Core Binaries and Tools"))
