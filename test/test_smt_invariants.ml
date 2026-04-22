(** Layer 1 SMT translation tests: structural invariants on emitted SMT-LIB2.

    For each [.pant] fixture in [samples/], [samples/smt-examples/], and
    [test/regression/], runs the full translation pipeline and checks the
    emitted SMT against the structural invariants defined in [Smt_check].

    Sample failures may be allow-listed in
    [test/regression/expected_failures.txt] while known-pending bugs are being
    fixed. The harness fails if an allowlist entry does not correspond to an
    actual failure (so stale entries are caught). *)

open Alcotest

let sample_dir =
  Test_util.find_dir
    [
      "samples";
      "../samples";
      "../../samples";
      Filename.concat (Sys.getcwd ()) "samples";
    ]

let smt_examples_dir =
  Test_util.find_dir
    [
      "samples/smt-examples";
      "../samples/smt-examples";
      "../../samples/smt-examples";
      Filename.concat (Sys.getcwd ()) "samples/smt-examples";
    ]

let regression_dir =
  Test_util.find_dir
    [
      "test/regression";
      "../test/regression";
      "../../test/regression";
      "regression";
      "../regression";
      Filename.concat (Sys.getcwd ()) "test/regression";
    ]

(** Parse + translate a [.pant] file to SMT queries; [None] if the upstream
    pipeline rejects the fixture (no SMT is generated, so structural checks have
    nothing to inspect). *)
let queries_of_path path : Pantagruel.Smt.query list option =
  match Test_util.translate_to_queries (Test_util.parse_pant_file path) with
  | Ok qs -> Some qs
  | Error _ -> None

(** Parse + translate a [.pant] file, returning the translation result directly.
    Unlike [queries_of_path] this preserves the [Error] variant so regression
    tests can fail loudly when a fixture they expect to translate cleanly
    regresses into a pipeline rejection. *)
let translate_path path : (Pantagruel.Smt.query list, string) result =
  Test_util.translate_to_queries (Test_util.parse_pant_file path)

(* ------------------------------------------------------------------ *)
(* Allowlist.                                                           *)
(* ------------------------------------------------------------------ *)

module StringMap = Map.Make (String)
(** Per-fixture set of failure kinds that are tolerated. *)

module KindSet = Set.Make (String)

let parse_allowlist (lines : string list) : KindSet.t StringMap.t =
  List.fold_left
    (fun acc raw ->
      let trimmed = String.trim raw in
      if trimmed = "" || trimmed.[0] = '#' then acc
      else
        match String.index_opt trimmed ':' with
        | None -> acc
        | Some i ->
            let fixture = String.trim (String.sub trimmed 0 i) in
            let kinds_str =
              String.sub trimmed (i + 1) (String.length trimmed - i - 1)
            in
            let kinds =
              String.split_on_char ',' kinds_str
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
              |> KindSet.of_list
            in
            let prev =
              StringMap.find_opt fixture acc
              |> Option.value ~default:KindSet.empty
            in
            StringMap.add fixture (KindSet.union prev kinds) acc)
    StringMap.empty lines

let load_allowlist () : KindSet.t StringMap.t =
  match regression_dir with
  | None -> StringMap.empty
  | Some dir ->
      let path = Filename.concat dir "expected_failures.txt" in
      if not (Sys.file_exists path) then StringMap.empty
      else
        let ic = open_in path in
        let rec read acc =
          match input_line ic with
          | line -> read (line :: acc)
          | exception End_of_file -> List.rev acc
        in
        let lines = read [] in
        close_in ic;
        parse_allowlist lines

(* ------------------------------------------------------------------ *)
(* Per-fixture check.                                                   *)
(* ------------------------------------------------------------------ *)

(** Run [Smt_check] over every emitted query for one fixture, returning the flat
    list of (query_name, failure) pairs. The translation pipeline runs exactly
    once per fixture; [observed_kinds] and [format_failures] both derive from
    this list. *)
let collect_failures path : (string * Smt_check.failure) list =
  match queries_of_path path with
  | None -> []
  | Some queries ->
      List.concat_map
        (fun (q : Pantagruel.Smt.query) ->
          List.map (fun f -> (q.name, f)) (Smt_check.check_query q.smt2))
        queries

let observed_kinds (failures : (string * Smt_check.failure) list) : KindSet.t =
  List.fold_left
    (fun acc (_, (f : Smt_check.failure)) ->
      KindSet.add (Smt_check.failure_kind_tag f.kind) acc)
    KindSet.empty failures

let format_failures failures =
  let buf = Buffer.create 256 in
  List.iter
    (fun (qname, f) ->
      Buffer.add_string buf "  - ";
      Buffer.add_string buf qname;
      Buffer.add_string buf ": ";
      Buffer.add_string buf (Smt_check.format_failure f);
      Buffer.add_char buf '\n')
    failures;
  Buffer.contents buf

(** Sample / smt-examples fixture test. Allowlist-aware. *)
let test_sample_fixture allowlist dir name () =
  let path = Filename.concat dir name in
  let failures = collect_failures path in
  let observed = observed_kinds failures in
  let allowed =
    StringMap.find_opt name allowlist |> Option.value ~default:KindSet.empty
  in
  let unexpected = KindSet.diff observed allowed |> KindSet.elements in
  let missing = KindSet.diff allowed observed |> KindSet.elements in
  if unexpected <> [] || missing <> [] then
    let parts =
      List.filter_map
        (fun (label, items) ->
          if items = [] then None
          else Some (Printf.sprintf "%s: [%s]" label (String.concat ", " items)))
        [
          ("unexpected failures", unexpected);
          ("stale allowlist entries", missing);
        ]
    in
    failf "%s — %s\n%s" name (String.concat "; " parts)
      (format_failures failures)

(* ------------------------------------------------------------------ *)
(* Regression cases — explicit assertions on the two known bugs.        *)
(* ------------------------------------------------------------------ *)

(** Assert that [fixture] in the regression dir produces every kind in
    [expect_kinds]. Used to lock in behavior pending bug fixes; once a bug is
    fixed, flip [expect_kinds] to [[]] so the test asserts cleanliness. Fails
    loudly if the fixture is rejected by the upstream pipeline — otherwise a
    regression that stops producing SMT altogether would look identical to a
    clean post-fix run. *)
let test_regression_fixture fixture expect_kinds () =
  match regression_dir with
  | None -> failf "regression directory not found"
  | Some dir ->
      let path = Filename.concat dir fixture in
      if not (Sys.file_exists path) then failf "missing fixture: %s" path;
      (match translate_path path with
      | Ok _ -> ()
      | Error msg -> failf "%s — translation failed: %s" fixture msg);
      let observed = observed_kinds (collect_failures path) in
      let expected = KindSet.of_list expect_kinds in
      let missing = KindSet.diff expected observed |> KindSet.elements in
      let extra = KindSet.diff observed expected |> KindSet.elements in
      if missing <> [] || extra <> [] then
        failf "%s — missing expected: [%s]; extra unexpected: [%s]" fixture
          (String.concat ", " missing)
          (String.concat ", " extra)

(** Assert the SMT emitted for [fixture] contains no universal quantifier that
    binds a name also declared as a constant. This catches the
    [bind_head_params] action-param shadow bug: when an action param's name
    coincides with a head-rule-param's name, the action body's free reference
    must resolve to the declared constant, not a wrapping
    [(forall ((<name> ...)) ...)]. *)
let test_no_shadowing_forall fixture shadowed_name () =
  match regression_dir with
  | None -> failf "regression directory not found"
  | Some dir ->
      let path = Filename.concat dir fixture in
      if not (Sys.file_exists path) then failf "missing fixture: %s" path;
      let queries =
        match translate_path path with
        | Ok qs -> qs
        | Error msg -> failf "%s — translation failed: %s" fixture msg
      in
      let needle = Printf.sprintf "(%s " shadowed_name in
      List.iter
        (fun (q : Pantagruel.Smt.query) ->
          (* Only action queries declare the name as a constant; other queries
             may legitimately bind it in a head-level forall. *)
          let declares_const =
            let decl = Printf.sprintf "(declare-const %s " shadowed_name in
            let len = String.length decl in
            let rec scan i =
              if i + len > String.length q.smt2 then false
              else if String.sub q.smt2 i len = decl then true
              else scan (i + 1)
            in
            scan 0
          in
          if declares_const then
            let forall_prefix = Printf.sprintf "(forall ((%s " shadowed_name in
            let len = String.length forall_prefix in
            let rec scan i =
              if i + len > String.length q.smt2 then ()
              else if String.sub q.smt2 i len = forall_prefix then
                failf "%s: query %S contains %S while also declaring %S" fixture
                  q.name needle shadowed_name
              else scan (i + 1)
            in
            scan 0)
        queries

(* ------------------------------------------------------------------ *)
(* Test suite assembly.                                                 *)
(* ------------------------------------------------------------------ *)

let sample_cases () =
  let allowlist = load_allowlist () in
  let from dir =
    Test_util.pant_files dir
    |> List.map (fun n ->
        test_case n `Quick (test_sample_fixture allowlist dir n))
  in
  let main = match sample_dir with None -> [] | Some d -> from d in
  let smt = match smt_examples_dir with None -> [] | Some d -> from d in
  main @ smt

let regression_cases () =
  [
    test_case "bug_overquantify.pant — clean post-fix" `Quick
      (test_regression_fixture "bug_overquantify.pant" []);
    test_case "bug_card_zero.pant — fallback emission" `Quick
      (test_regression_fixture "bug_card_zero.pant" [ "fallback_emission" ]);
    test_case "bug_action_body_rule_params.pant — clean post-fix" `Quick
      (test_regression_fixture "bug_action_body_rule_params.pant" []);
    test_case "bug_action_param_shadows_rule.pant — translates cleanly" `Quick
      (test_regression_fixture "bug_action_param_shadows_rule.pant" []);
    test_case "bug_action_param_shadows_rule.pant — no shadow forall" `Quick
      (test_no_shadowing_forall "bug_action_param_shadows_rule.pant" "a1");
  ]

let () =
  run "SmtInvariants"
    [ ("samples", sample_cases ()); ("regression", regression_cases ()) ]
