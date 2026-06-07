(* @archlint.module test
   @archlint.domain pantagruel.solver-interpretation *)

open Alcotest
open Pantagruel
module SI = Solver_interpretation
module SO = Solver_output

let query ?(name = "invariant:Act:0") ?(description = "desc")
    ?(kind = Smt.InvariantPreservation) ?(invariant_text = "inv") () =
  Smt.
    {
      name;
      description;
      smt2 = "";
      kind;
      value_terms = [];
      invariant_text;
      assertion_names = [ ("core_0", "x > 0") ];
    }

let interpretation_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"solver interpretation public APIs classify query results"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let q = query () in
           let failed = SI.interpret_result q (SO.Sat [ ("x", "1") ]) in
           let passed = SI.interpret_result q (SO.Unsat []) in
           String.length (SI.format_unsat_core [ "core_0" ] q.assertion_names)
           > 0
           && SI.extract_label { q with name = "precondition:Act" } 13 = "Act"
           && (not failed.passed) && passed.passed
           && SI.extract_holds_clause "foo holds for 3 steps from initial state"
              = "holds for 3 steps from initial state"
           && SI.parse_preservation_info "invariant:Act:42" = Some ("Act", 42)
           && SI.parse_bmc_index "bmc-invariant:42" = Some 42));
  ]

let correlation_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"correlate_results folds bmc support" ~count:100
         QCheck2.Gen.unit (fun () ->
           let preservation_query = query () in
           let bmc_query =
             query ~name:"bmc-invariant:0"
               ~description:"Invariant holds for 2 steps from initial state"
               ~kind:Smt.BMCInvariant ()
           in
           let preservation =
             SI.interpret_result preservation_query (SO.Sat [ ("x", "1") ])
           in
           let bmc = SI.interpret_result bmc_query (SO.Unsat []) in
           match SI.correlate_results [ preservation; bmc ] with
           | [ result ] ->
               result.passed && result.query.name = preservation_query.name
           | _ -> false));
  ]

let () =
  run "Solver_interpretation_core"
    [
      ("interpretation", interpretation_properties);
      ("correlation", correlation_properties);
    ]
