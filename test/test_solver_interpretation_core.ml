(* @archlint.module test
   @archlint.domain pantagruel.solver-interpretation *)

open Alcotest
open Pantagruel
module SI = Solver_interpretation
module SO = Solver_output

let gen_label =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let gen_core_name =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 10)

let query ?(name = "invariant:Act:0") ?(description = "desc")
    ?(kind = Smt.InvariantPreservation) ?(invariant_text = "inv")
    ?(assertion_names = [ ("core_0", "x > 0") ]) () =
  Smt.
    {
      name;
      description;
      smt2 = "";
      kind;
      value_terms = [];
      invariant_text;
      assertion_names;
    }

let interpretation_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"solver interpretation public APIs classify query results"
         ~count:100
         (QCheck2.Gen.triple gen_label
            (QCheck2.Gen.int_range 0 100)
            gen_core_name)
         (fun (label, index, core_name) ->
           let q =
             query
               ~name:(Printf.sprintf "invariant:%s:%d" label index)
               ~assertion_names:[ (core_name, label ^ " holds") ]
               ()
           in
           let failed = SI.interpret_result q (SO.Sat [ ("x", "1") ]) in
           let passed = SI.interpret_result q (SO.Unsat []) in
           String.contains
             (SI.format_unsat_core [ core_name ] q.assertion_names)
             label.[0]
           && SI.extract_label { q with name = "precondition:" ^ label } 13
              = label
           && (not failed.passed) && passed.passed
           && SI.extract_holds_clause
                (label ^ " holds for 3 steps from initial state")
              = "holds for 3 steps from initial state"
           && SI.parse_preservation_info q.name = Some (label, index)
           && SI.parse_bmc_index (Printf.sprintf "bmc-invariant:%d" index)
              = Some index));
  ]

let correlation_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"correlate_results folds bmc support" ~count:100
         (QCheck2.Gen.pair gen_label (QCheck2.Gen.int_range 0 20))
         (fun (label, index) ->
           let preservation_query =
             query ~name:(Printf.sprintf "invariant:%s:%d" label index) ()
           in
           let bmc_query =
             query
               ~name:(Printf.sprintf "bmc-invariant:%d" index)
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
