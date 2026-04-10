(** Tests for the module system *)

open Alcotest
open Pantagruel

let tmp_dir = Filename.get_temp_dir_name ()

let write_temp_file name content =
  let path = Filename.concat tmp_dir name in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  path

let cleanup_temp_file path = try Sys.remove path with Sys_error _ -> ()

(* --- parse_module_header tests --- *)

let test_parse_module_header_valid () =
  let path =
    write_temp_file "test_mod_valid.pant" "module TEST.\n\nFoo.\n---\n"
  in
  (match Module.parse_module_header path with
  | Ok name -> check string "module name" "TEST" name
  | Error msg -> fail (Printf.sprintf "Expected Ok, got Error: %s" msg));
  cleanup_temp_file path

let test_parse_module_header_no_module () =
  let path = write_temp_file "test_mod_none.pant" "Foo.\n---\ntrue.\n" in
  (match Module.parse_module_header path with
  | Ok name -> fail (Printf.sprintf "Expected Error, got Ok %s" name)
  | Error _ -> ());
  cleanup_temp_file path

let test_parse_module_header_missing_dot () =
  let path = write_temp_file "test_mod_nodot.pant" "module TEST\nFoo.\n---\n" in
  (match Module.parse_module_header path with
  | Ok name -> fail (Printf.sprintf "Expected Error, got Ok %s" name)
  | Error _ -> ());
  cleanup_temp_file path

let test_parse_module_header_nonexistent () =
  match Module.parse_module_header "/nonexistent/path.pant" with
  | Ok name -> fail (Printf.sprintf "Expected Error, got Ok %s" name)
  | Error _ -> ()

(* --- scan_module_path tests --- *)

let test_scan_module_path () =
  (* scan_module_path on samples/ should find modules *)
  let candidates =
    [
      "samples";
      "../samples";
      "../../samples";
      Filename.concat (Sys.getcwd ()) "samples";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | None -> () (* Skip if samples not found *)
  | Some dir ->
      let registry = Module.scan_module_path dir in
      (* Should find at least the BASICS module *)
      check bool "registry is non-empty" true
        (Module.StringMap.cardinal registry.modules > 0)

(* --- check_with_imports tests --- *)

let test_check_with_imports_no_imports () =
  let candidates =
    [
      "samples";
      "../samples";
      "../../samples";
      Filename.concat (Sys.getcwd ()) "samples";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | None -> ()
  | Some dir ->
      let registry = Module.scan_module_path dir in
      let path = Filename.concat dir "01-basics.pant" in
      let channel = open_in path in
      let lexer = Lexer.create_from_channel path channel in
      let supplier = Lexer.menhir_token lexer in
      let doc =
        MenhirLib.Convert.Simplified.traditional2revised Parser.document
          supplier
      in
      close_in channel;
      (match Module.check_with_imports registry doc with
      | Ok (_env, _warnings) -> ()
      | Error e ->
          fail
            (Printf.sprintf "Expected Ok, got Error: %s"
               (Module.show_module_error e)));
      ()

let () =
  run "Module"
    [
      ( "parse_module_header",
        [
          test_case "valid" `Quick test_parse_module_header_valid;
          test_case "no module" `Quick test_parse_module_header_no_module;
          test_case "missing dot" `Quick test_parse_module_header_missing_dot;
          test_case "nonexistent" `Quick test_parse_module_header_nonexistent;
        ] );
      ( "scan_module_path",
        [ test_case "samples dir" `Quick test_scan_module_path ] );
      ( "check_with_imports",
        [ test_case "no imports" `Quick test_check_with_imports_no_imports ] );
    ]
