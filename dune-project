(lang dune 3.0)
(name pantagruel)
(using menhir 2.1)

(generate_opam_files true)

(license BSD-3-Clause)

(package
 (name pantagruel)
 (synopsis "A specification language checker")
 (license BSD-3-Clause)
 (depends
  (ocaml (>= 4.14))
  menhir
  sedlex
  ppx_deriving
  yojson
  sexplib0
  parsexp
  (alcotest :with-test)))
