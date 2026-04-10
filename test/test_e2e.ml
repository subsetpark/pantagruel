(** End-to-end tests: run the full pipeline on sample .pant files *)

open Alcotest
open Pantagruel

let find_dir candidates = List.find_opt Sys.file_exists candidates

let sample_dir =
  find_dir
    [
      "samples";
      "../samples";
      "../../samples";
      Filename.concat (Sys.getcwd ()) "samples";
    ]

let snapshot_dir =
  find_dir
    [
      "test/snapshots";
      "../test/snapshots";
      "../../test/snapshots";
      Filename.concat (Sys.getcwd ()) "test/snapshots";
    ]

let sample_files () =
  match sample_dir with
  | None -> []
  | Some dir ->
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

let with_sample_dir f = match sample_dir with None -> () | Some dir -> f dir

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

(** Test that each sample parses, collects, and type-checks *)
let test_sample_typecheck name () =
  with_sample_dir (fun dir ->
      let path = Filename.concat dir name in
      let doc = parse_file path in
      let mod_name = Option.value ~default:"" doc.module_name in
      match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
      | Error e ->
          failf "Collection error in %s: %s" name (Collect.show_collect_error e)
      | Ok env -> (
          match Check.check_document env doc with
          | Error e ->
              failf "Type error in %s: %s" name (Check.show_type_error e)
          | Ok _warnings -> ()))

(** Test that each sample produces non-empty JSON output *)
let test_sample_json name () =
  with_sample_dir (fun dir ->
      let path = Filename.concat dir name in
      let doc = parse_file path in
      let mod_name = Option.value ~default:"" doc.module_name in
      match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
      | Error _ -> ()
      | Ok env ->
          let json = Json_output.document_to_json env doc in
          let s = Yojson.Basic.pretty_to_string json in
          check bool (name ^ " JSON is non-empty") true (String.length s > 0))

(** Test that each sample produces non-empty markdown output *)
let test_sample_markdown name () =
  with_sample_dir (fun dir ->
      let path = Filename.concat dir name in
      let doc = parse_file path in
      let mod_name = Option.value ~default:"" doc.module_name in
      match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
      | Error _ -> ()
      | Ok env ->
          let procs = Markdown_output.rule_names_of_env env in
          let buf = Buffer.create 1024 in
          let fmt = Format.formatter_of_buffer buf in
          Format.pp_set_margin fmt 10000;
          Markdown_output.pp_document procs fmt doc;
          Format.pp_print_flush fmt ();
          let s = Buffer.contents buf in
          check bool (name ^ " markdown is non-empty") true (String.length s > 0))

(** Test that each sample produces non-empty pretty-printed output *)
let test_sample_pretty name () =
  with_sample_dir (fun dir ->
      let path = Filename.concat dir name in
      let doc = parse_file path in
      let buf = Buffer.create 1024 in
      let fmt = Format.formatter_of_buffer buf in
      Format.pp_set_margin fmt 10000;
      Pretty.output_document ~width:10000 fmt doc;
      Format.pp_print_flush fmt ();
      let s = Buffer.contents buf in
      check bool
        (name ^ " pretty output is non-empty")
        true
        (String.length s > 0))

(** Test that each sample generates SMT queries without error *)
let test_sample_smt name () =
  with_sample_dir (fun dir ->
      let path = Filename.concat dir name in
      let doc = parse_file path in
      let mod_name = Option.value ~default:"" doc.module_name in
      match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
      | Error _ -> ()
      | Ok env ->
          let domain_bounds = Smt.compute_domain_bounds 3 env in
          let config =
            Smt.make_config ~bound:3 ~steps:1 ~domain_bounds ~inject_guards:true
          in
          let queries = Smt.generate_queries config env doc in
          ignore queries)

(** Test that JSON output matches stored snapshot *)
let test_snapshot_json name () =
  with_sample_dir (fun dir ->
      let path = Filename.concat dir name in
      let doc = parse_file path in
      let mod_name = Option.value ~default:"" doc.module_name in
      match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
      | Error _ -> ()
      | Ok env -> (
          let json = Json_output.document_to_json env doc in
          let actual = Yojson.Basic.pretty_to_string json ^ "\n" in
          let base = Filename.chop_extension name in
          let snap_path =
            match snapshot_dir with
            | None -> None
            | Some sd ->
                let p = Filename.concat sd ("json/" ^ base ^ ".json") in
                if Sys.file_exists p then Some p else None
          in
          match snap_path with
          | Some p ->
              let expected = read_file p in
              check string (name ^ " JSON snapshot") expected actual
          | None -> ()))

let () =
  let samples = sample_files () in
  if samples = [] then
    run "E2E"
      [ ("skip", [ test_case "no samples found" `Quick (fun () -> ()) ]) ]
  else
    run "E2E"
      [
        ( "typecheck",
          List.map
            (fun name -> test_case name `Quick (test_sample_typecheck name))
            samples );
        ( "json",
          List.map
            (fun name -> test_case name `Quick (test_sample_json name))
            samples );
        ( "markdown",
          List.map
            (fun name -> test_case name `Quick (test_sample_markdown name))
            samples );
        ( "pretty",
          List.map
            (fun name -> test_case name `Quick (test_sample_pretty name))
            samples );
        ( "smt",
          List.map
            (fun name -> test_case name `Quick (test_sample_smt name))
            samples );
        ( "snapshot_json",
          List.map
            (fun name -> test_case name `Quick (test_snapshot_json name))
            samples );
      ]
