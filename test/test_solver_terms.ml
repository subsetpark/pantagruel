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
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"term classification and unprime agree"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let base = "balance" in
           let primed = Solver_terms.add_prime_suffix base in
           Solver_terms.classify_term base = Solver_terms.ActionParam
           && Solver_terms.classify_term primed = Solver_terms.After
           && Solver_terms.classify_term ("(" ^ base ^ " a)")
              = Solver_terms.Before
           && Solver_terms.unprime_term primed = Some base
           && Solver_terms.unprime_term ("(" ^ primed ^ " a)")
              = Some ("(" ^ base ^ " a)")));
  ]

let () =
  run "Solver_terms"
    [
      ("suffix", suffix_properties);
      ("display", display_properties);
      ("classification", classification_properties);
    ]
