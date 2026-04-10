(** Tests for solver module — pure functions only, no z3 required *)

open Alcotest
open Pantagruel

(* --- parse_get_value tests --- *)

let test_parse_get_value_simple () =
  let result = Solver.parse_get_value "((x 1)(y 2))" in
  check
    (list (pair string string))
    "two pairs"
    [ ("x", "1"); ("y", "2") ]
    result

let test_parse_get_value_nested () =
  let result = Solver.parse_get_value "((  (balance a) 42  ))" in
  check
    (list (pair string string))
    "applied term"
    [ ("(balance a)", "42") ]
    result

let test_parse_get_value_empty () =
  check (list (pair string string)) "empty input" [] (Solver.parse_get_value "")

let test_parse_get_value_malformed () =
  check
    (list (pair string string))
    "malformed" []
    (Solver.parse_get_value "not-sexp")

(* --- parse_unsat_core tests --- *)

let test_parse_unsat_core_simple () =
  let result = Solver.parse_unsat_core "(a1 a2 a3)" in
  check (list string) "three names" [ "a1"; "a2"; "a3" ] result

let test_parse_unsat_core_empty () =
  check (list string) "empty" [] (Solver.parse_unsat_core "")

(* --- parse_solver_output tests --- *)

let test_parse_solver_output_sat () =
  match Solver.parse_solver_output "sat\n((x 1))" with
  | Solver.Sat values ->
      check (list (pair string string)) "sat values" [ ("x", "1") ] values
  | Solver.Unsat _ | Solver.Unknown _ | Solver.SolverError _ ->
      fail "expected Sat"

let test_parse_solver_output_unsat () =
  match Solver.parse_solver_output "unsat\n(a1 a2)" with
  | Solver.Unsat core -> check (list string) "unsat core" [ "a1"; "a2" ] core
  | Solver.Sat _ | Solver.Unknown _ | Solver.SolverError _ ->
      fail "expected Unsat"

let test_parse_solver_output_unknown () =
  match Solver.parse_solver_output "unknown" with
  | Solver.Unknown reason -> check string "unknown reason" "unknown" reason
  | Solver.Sat _ | Solver.Unsat _ | Solver.SolverError _ ->
      fail "expected Unknown"

let test_parse_solver_output_error () =
  match Solver.parse_solver_output "(error \"timeout\")" with
  | Solver.SolverError _ -> ()
  | Solver.Sat _ | Solver.Unsat _ | Solver.Unknown _ ->
      fail "expected SolverError"

let test_parse_solver_output_empty () =
  match Solver.parse_solver_output "" with
  | Solver.SolverError _ -> ()
  | Solver.Sat _ | Solver.Unsat _ | Solver.Unknown _ ->
      fail "expected SolverError for empty input"

(* --- translate_value tests --- *)

let test_translate_value_negation () =
  check string "z3 negation" "-5" (Solver.translate_value "(- 5)")

let test_translate_value_domain () =
  check string "domain val" "User_0" (Solver.translate_value "User!val!0")

let test_translate_value_plain () =
  check string "plain atom" "42" (Solver.translate_value "42")

let test_translate_value_true () =
  check string "true" "true" (Solver.translate_value "true")

(* --- translate_display_name tests --- *)

let test_translate_display_name_prime () =
  check string "prime suffix" "balance'"
    (Solver.translate_display_name "balance_prime")

let test_translate_display_name_applied () =
  check string "applied term" "balance a"
    (Solver.translate_display_name "(balance a)")

let test_translate_display_name_applied_prime () =
  check string "applied primed" "balance' a"
    (Solver.translate_display_name "(balance_prime a)")

let test_translate_display_name_plain () =
  check string "plain name" "x" (Solver.translate_display_name "x")

(* --- classify_term tests --- *)

let test_classify_term_before () =
  check bool "applied non-prime is Before" true
    (Solver.classify_term "(balance a)" = Solver.Before)

let test_classify_term_after () =
  check bool "applied prime is After" true
    (Solver.classify_term "(balance_prime a)" = Solver.After)

let test_classify_term_action_param () =
  check bool "bare non-prime is ActionParam" true
    (Solver.classify_term "u" = Solver.ActionParam)

let test_classify_term_bare_prime () =
  check bool "bare prime is After" true
    (Solver.classify_term "x_prime" = Solver.After)

(* --- unprime_term tests --- *)

let test_unprime_applied () =
  check (option string) "applied primed" (Some "(balance a)")
    (Solver.unprime_term "(balance_prime a)")

let test_unprime_bare () =
  check (option string) "bare primed" (Some "balance")
    (Solver.unprime_term "balance_prime")

let test_unprime_not_primed () =
  check (option string) "not primed" None (Solver.unprime_term "balance")

let test_unprime_applied_not_primed () =
  check (option string) "applied not primed" None
    (Solver.unprime_term "(balance a)")

(* --- format_counterexample tests --- *)

let test_format_counterexample_empty () =
  check string "empty" "" (Solver.format_counterexample [])

let test_format_counterexample_filters_unchanged () =
  (* If before and after values are the same, they should be filtered *)
  let values =
    [
      ("(balance a)", "100");
      ("(balance_prime a)", "100");
      (* same — should be filtered *)
      ("(score a)", "5");
      ("(score_prime a)", "10");
      (* different — should appear *)
    ]
  in
  let result = Solver.format_counterexample values in
  check bool "contains changed value" true (String.length result > 0);
  let contains s sub =
    let slen = String.length sub in
    let len = String.length s in
    let rec find i =
      if i + slen > len then false
      else if String.sub s i slen = sub then true
      else find (i + 1)
    in
    find 0
  in
  check bool "contains score" true (contains result "score")

(* --- Property-based tests --- *)

let test_translate_display_name_no_crash =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make ~name:"translate_display_name never crashes" ~count:1000
       QCheck.string (fun s ->
         ignore (Solver.translate_display_name s);
         true))

let test_classify_term_no_crash =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make ~name:"classify_term never crashes" ~count:1000
       QCheck.string (fun s ->
         ignore (Solver.classify_term s);
         true))

let test_parse_solver_output_no_crash =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make ~name:"parse_solver_output never crashes" ~count:1000
       QCheck.string (fun s ->
         ignore (Solver.parse_solver_output s);
         true))

let () =
  run "Solver"
    [
      ( "parse_get_value",
        [
          test_case "simple" `Quick test_parse_get_value_simple;
          test_case "nested" `Quick test_parse_get_value_nested;
          test_case "empty" `Quick test_parse_get_value_empty;
          test_case "malformed" `Quick test_parse_get_value_malformed;
        ] );
      ( "parse_unsat_core",
        [
          test_case "simple" `Quick test_parse_unsat_core_simple;
          test_case "empty" `Quick test_parse_unsat_core_empty;
        ] );
      ( "parse_solver_output",
        [
          test_case "sat" `Quick test_parse_solver_output_sat;
          test_case "unsat" `Quick test_parse_solver_output_unsat;
          test_case "unknown" `Quick test_parse_solver_output_unknown;
          test_case "error" `Quick test_parse_solver_output_error;
          test_case "empty" `Quick test_parse_solver_output_empty;
        ] );
      ( "translate_value",
        [
          test_case "negation" `Quick test_translate_value_negation;
          test_case "domain" `Quick test_translate_value_domain;
          test_case "plain" `Quick test_translate_value_plain;
          test_case "true" `Quick test_translate_value_true;
        ] );
      ( "translate_display_name",
        [
          test_case "prime" `Quick test_translate_display_name_prime;
          test_case "applied" `Quick test_translate_display_name_applied;
          test_case "applied_prime" `Quick
            test_translate_display_name_applied_prime;
          test_case "plain" `Quick test_translate_display_name_plain;
        ] );
      ( "classify_term",
        [
          test_case "before" `Quick test_classify_term_before;
          test_case "after" `Quick test_classify_term_after;
          test_case "action_param" `Quick test_classify_term_action_param;
          test_case "bare_prime" `Quick test_classify_term_bare_prime;
        ] );
      ( "unprime_term",
        [
          test_case "applied" `Quick test_unprime_applied;
          test_case "bare" `Quick test_unprime_bare;
          test_case "not_primed" `Quick test_unprime_not_primed;
          test_case "applied_not_primed" `Quick test_unprime_applied_not_primed;
        ] );
      ( "format_counterexample",
        [
          test_case "empty" `Quick test_format_counterexample_empty;
          test_case "filters unchanged" `Quick
            test_format_counterexample_filters_unchanged;
        ] );
      ( "property",
        [
          test_translate_display_name_no_crash;
          test_classify_term_no_crash;
          test_parse_solver_output_no_crash;
        ] );
    ]
