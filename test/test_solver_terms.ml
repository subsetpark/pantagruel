(* @archlint.module test
   @archlint.domain pantagruel.solver-terms *)

open Alcotest
open Pantagruel

let suffix_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"prime suffix helpers are inverse on bases"
         ~count:1000 ~print:Fun.id QCheck2.Gen.string (fun s ->
           let primed = Solver_terms.add_prime_suffix s in
           Solver_terms.has_prime_suffix primed
           && Solver_terms.strip_prime_suffix primed = s
           && Solver_terms.prime_suffix = "_prime"));
  ]

let display_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"display translation is total" ~count:1000
         ~print:Fun.id QCheck2.Gen.string (fun s ->
           ignore (Solver_terms.translate_value s);
           ignore (Solver_terms.translate_display_name s);
           ignore (Solver_terms.translate_value_sexp (Sexplib0.Sexp.Atom s));
           true));
  ]

let classification_properties =
  let gen_atom =
    QCheck2.Gen.string_size
      ~gen:
        QCheck2.Gen.(
          oneof_weighted [ (8, char_range 'a' 'z'); (2, char_range 'A' 'Z') ])
      (QCheck2.Gen.int_range 1 12)
  in
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"term classification and unprime agree"
         ~count:100 (QCheck2.Gen.pair gen_atom gen_atom) (fun (base, arg) ->
           let primed = Solver_terms.add_prime_suffix base in
           let before_app = "(" ^ base ^ " " ^ arg ^ ")" in
           let after_app = "(" ^ primed ^ " " ^ arg ^ ")" in
           Solver_terms.classify_term base = Solver_terms.ActionParam
           && Solver_terms.classify_term primed = Solver_terms.After
           && Solver_terms.classify_term before_app = Solver_terms.Before
           && Solver_terms.unprime_term primed = Some base
           && Solver_terms.unprime_term after_app = Some before_app));
  ]

let () =
  run "Solver_terms"
    [
      ("suffix", suffix_properties);
      ("display", display_properties);
      ("classification", classification_properties);
    ]
