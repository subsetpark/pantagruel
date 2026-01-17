(lang dune 3.0)
(name pantagruel)
(using menhir 2.1)

(generate_opam_files true)

(package
 (name pantagruel)
 (synopsis "A specification language checker")
 (depends
  (ocaml (>= 4.14))
  menhir
  sedlex
  ppx_deriving
  (alcotest :with-test)))
