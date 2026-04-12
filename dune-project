(lang dune 3.21)
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
  (alcotest :with-test)
  (qcheck-alcotest :with-test)
  (crowbar :with-test)))

(package
 (name pantagruel-wasm)
 (allow_empty)
 (synopsis "WebAssembly build of the Pantagruel parser")
 (license BSD-3-Clause)
 (depends
  (ocaml (>= 4.14))
  (pantagruel (= :version))
  js_of_ocaml
  js_of_ocaml-ppx
  (js_of_ocaml-compiler :build)
  (wasm_of_ocaml-compiler :build)))
