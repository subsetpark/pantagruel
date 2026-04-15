(** Layer 1 SMT translation tests: structural invariants on emitted SMT-LIB2.

    For each [.pant] fixture in [samples/], [samples/smt-examples/], and
    [test/regression/], runs the full translation pipeline and checks the
    emitted SMT against the structural invariants defined in [Smt_check].

    Sample failures may be allow-listed in
    [test/regression/expected_failures.txt] while known-pending bugs are being
    fixed. The harness fails if an allowlist entry does not correspond to an
    actual failure (so stale entries are caught). *)

open Alcotest
open Pantagruel

(* ------------------------------------------------------------------ *)
(* Filesystem helpers — mirror test_e2e.ml.                             *)
(* ------------------------------------------------------------------ *)

let find_dir candidates = List.find_opt Sys.file_exists candidates

let sample_dir =
  find_dir
    [
      "samples";
      "../samples";
      "../../samples";
      Filename.concat (Sys.getcwd ()) "samples";
    ]

let smt_examples_dir =
  find_dir
    [
      "samples/smt-examples";
      "../samples/smt-examples";
      "../../samples/smt-examples";
      Filename.concat (Sys.getcwd ()) "samples/smt-examples";
    ]

let regression_dir =
  find_dir
    [
      "test/regression";
      "../test/regression";
      "../../test/regression";
      "regression";
      "../regression";
      Filename.concat (Sys.getcwd ()) "test/regression";
    ]

let pant_files dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".pant")
  |> List.sort String.compare

let parse_file path =
  let channel = open_in path in
  let lexer = Lexer.create_from_channel path channel in
  let supplier = Lexer.menhir_token lexer in
  let doc =
    MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier
  in
  close_in channel;
  doc

(* ------------------------------------------------------------------ *)
(* Pipeline.                                                            *)
(* ------------------------------------------------------------------ *)

(** Parse, collect, type-check and translate a [.pant] file to SMT queries.
    Returns [None] when the upstream pipeline rejects the fixture (no SMT is
    generated, so structural checks have nothing to inspect). *)
let queries_of_path path : Smt.query list option =
  let doc = parse_file path in
  let mod_name = Option.fold ~none:"" ~some:Ast.upper_name doc.module_name in
  match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
  | Error _ -> None
  | Ok env -> (
      match Check.check_document env doc with
      | Error _ -> None
      | Ok _ ->
          let domain_bounds = Smt.compute_domain_bounds 3 env in
          let config =
            Smt.make_config ~bound:3 ~steps:1 ~domain_bounds ~inject_guards:true
          in
          Some (Smt.generate_queries config env doc))

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

(** Run [Smt_check] over every emitted query for one fixture. Returns the set of
    failure kinds observed (deduplicated). *)
let observed_kinds_for path : KindSet.t =
  match queries_of_path path with
  | None -> KindSet.empty
  | Some queries ->
      List.fold_left
        (fun acc (q : Smt.query) ->
          List.fold_left
            (fun acc (f : Smt_check.failure) ->
              KindSet.add (Smt_check.failure_kind_tag f.kind) acc)
            acc
            (Smt_check.check_query q.smt2))
        KindSet.empty queries

(** Diff observed vs allowed kinds, returning (unexpected, missing). Each is a
    list of failure-kind tags. [unexpected] = observed but not allowed.
    [missing] = allowed but not observed (stale allowlist entries). *)
let diff_kinds ~observed ~allowed =
  let unexpected = KindSet.diff observed allowed |> KindSet.elements in
  let missing = KindSet.diff allowed observed |> KindSet.elements in
  (unexpected, missing)

(** Format a per-fixture failure report for alcotest. *)
let format_report fixture path ~unexpected ~missing =
  let detail =
    match queries_of_path path with
    | None -> "  (no queries generated; upstream pipeline rejected)\n"
    | Some queries ->
        let buf = Buffer.create 256 in
        List.iter
          (fun (q : Smt.query) ->
            List.iter
              (fun f ->
                Buffer.add_string buf "  - ";
                Buffer.add_string buf q.name;
                Buffer.add_string buf ": ";
                Buffer.add_string buf (Smt_check.format_failure f);
                Buffer.add_char buf '\n')
              (Smt_check.check_query q.smt2))
          queries;
        Buffer.contents buf
  in
  let parts = ref [] in
  if unexpected <> [] then
    parts :=
      Printf.sprintf "unexpected failures: [%s]" (String.concat ", " unexpected)
      :: !parts;
  if missing <> [] then
    parts :=
      Printf.sprintf "stale allowlist entries: [%s]"
        (String.concat ", " missing)
      :: !parts;
  Printf.sprintf "%s — %s\n%s" fixture (String.concat "; " !parts) detail

(** Sample / smt-examples fixture test. Allowlist-aware. *)
let test_sample_fixture allowlist dir name () =
  let path = Filename.concat dir name in
  let observed = observed_kinds_for path in
  let allowed =
    StringMap.find_opt name allowlist |> Option.value ~default:KindSet.empty
  in
  let unexpected, missing = diff_kinds ~observed ~allowed in
  if unexpected <> [] || missing <> [] then
    failf "%s" (format_report name path ~unexpected ~missing)

(* ------------------------------------------------------------------ *)
(* Regression cases — explicit assertions on the two known bugs.        *)
(* ------------------------------------------------------------------ *)

(** Assert that [fixture] in the regression dir produces every kind in
    [expect_kinds]. Used to lock in behavior pending bug fixes; once a bug is
    fixed, flip [expect_kinds] to [[]] so the test asserts cleanliness. *)
let test_regression_fixture fixture expect_kinds () =
  match regression_dir with
  | None -> failf "regression directory not found"
  | Some dir ->
      let path = Filename.concat dir fixture in
      if not (Sys.file_exists path) then failf "missing fixture: %s" path;
      let observed = observed_kinds_for path in
      let expected = KindSet.of_list expect_kinds in
      let missing = KindSet.diff expected observed |> KindSet.elements in
      let extra = KindSet.diff observed expected |> KindSet.elements in
      if missing <> [] || extra <> [] then
        failf "%s — missing expected: [%s]; extra unexpected: [%s]" fixture
          (String.concat ", " missing)
          (String.concat ", " extra)

(* ------------------------------------------------------------------ *)
(* Test suite assembly.                                                 *)
(* ------------------------------------------------------------------ *)

let sample_cases () =
  let allowlist = load_allowlist () in
  let from dir =
    pant_files dir
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
  ]

let () =
  run "SmtInvariants"
    [ ("samples", sample_cases ()); ("regression", regression_cases ()) ]
