(* @archlint.module test
   @archlint.domain pantagruel.solver-output *)

open Alcotest
open Pantagruel
module SO = Solver_output
module Sexp = Sexplib0.Sexp

let parse_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"solver output parsers handle canonical forms"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Solver_output.parse_get_value "((x 1)(y 2))"
           = [ ("x", "1"); ("y", "2") ]
           && Solver_output.parse_get_value_sexp
                (Sexp.List
                   [
                     Sexp.List [ Sexp.Atom "x"; Sexp.Atom "1" ];
                     Sexp.List [ Sexp.Atom "y"; Sexp.Atom "2" ];
                   ])
              = [ ("x", "1"); ("y", "2") ]
           && Solver_output.parse_unsat_core "(a1 a2)" = [ "a1"; "a2" ]
           && Solver_output.parse_unsat_core_sexp
                (Sexp.List [ Sexp.Atom "a1"; Sexp.Atom "a2" ])
              = [ "a1"; "a2" ]
           && List.map Sexp.to_string (Solver_output.parse_sexps "sat")
              = [ "sat" ]
           &&
           match
             Solver_output.parse_solver_output
               "unknown\n(:reason-unknown timeout)"
           with
           | SO.Unknown "timeout" -> true
           | SO.Sat _ | SO.Unsat _ | SO.Unknown _ | SO.SolverError _ -> false));
  ]

let formatting_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"solver output formatters are total" ~count:100
         (QCheck2.Gen.list
            (QCheck2.Gen.pair QCheck2.Gen.string_printable
               QCheck2.Gen.string_printable))
         (fun values ->
           ignore (Solver_output.format_counterexample values);
           ignore (Solver_output.format_bmc_counterexample values);
           true));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"reason unknown extractor recognizes diagnostics"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Solver_output.extract_reason_unknown
             (Sexp.List [ Sexp.Atom ":reason-unknown"; Sexp.Atom "incomplete" ])
           = Some "incomplete"
           && Solver_output.extract_reason_unknown (Sexp.Atom "sat") = None));
  ]

let () =
  run "Solver_output_core"
    [ ("parse", parse_properties); ("formatting", formatting_properties) ]
