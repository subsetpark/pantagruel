(* @archlint.module test
   @archlint.domain pantagruel.solver-output *)

open Alcotest
open Pantagruel
module SO = Solver_output
module Sexp = Sexplib0.Sexp

let gen_atom =
  QCheck2.Gen.string_size
    ~gen:
      QCheck2.Gen.(
        oneof_weighted
          [ (8, char_range 'a' 'z'); (2, char_range '0' '9'); (1, return '_') ])
    (QCheck2.Gen.int_range 1 10)

let parse_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"solver output parsers handle canonical forms"
         ~count:100
         (QCheck2.Gen.triple
            (QCheck2.Gen.pair gen_atom gen_atom)
            (QCheck2.Gen.pair gen_atom gen_atom)
            gen_atom)
         (fun ((x, xv), (y, yv), reason) ->
           let values_s = Printf.sprintf "((%s %s)(%s %s))" x xv y yv in
           let values = [ (x, xv); (y, yv) ] in
           let core_s = Printf.sprintf "(%s %s)" x y in
           Solver_output.parse_get_value values_s = values
           && Solver_output.parse_get_value_sexp
                (Sexp.List
                   [
                     Sexp.List [ Sexp.Atom x; Sexp.Atom xv ];
                     Sexp.List [ Sexp.Atom y; Sexp.Atom yv ];
                   ])
              = values
           && Solver_output.parse_unsat_core core_s = [ x; y ]
           && Solver_output.parse_unsat_core_sexp
                (Sexp.List [ Sexp.Atom x; Sexp.Atom y ])
              = [ x; y ]
           && List.map Sexp.to_string (Solver_output.parse_sexps reason)
              = [ reason ]
           &&
           match
             Solver_output.parse_solver_output
               ("unknown\n(:reason-unknown " ^ reason ^ ")")
           with
           | SO.Unknown parsed -> String.equal parsed reason
           | SO.Sat _ | SO.Unsat _ | SO.SolverError _ -> false));
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
         ~count:100 gen_atom (fun reason ->
           Solver_output.extract_reason_unknown
             (Sexp.List [ Sexp.Atom ":reason-unknown"; Sexp.Atom reason ])
           = Some reason
           && Solver_output.extract_reason_unknown (Sexp.Atom "sat") = None));
  ]

let () =
  run "Solver_output_core"
    [ ("parse", parse_properties); ("formatting", formatting_properties) ]
