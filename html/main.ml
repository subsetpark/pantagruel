(** Extract module name from first line if it matches "module NAME." *)
let extract_module_name file =
  let ic = open_in file in
  let line = try input_line ic with End_of_file -> "" in
  close_in ic;
  let line = String.trim line in
  if String.length line > 8
     && String.sub line 0 7 = "module "
     && line.[String.length line - 1] = '.'
  then
    Some (String.sub line 7 (String.length line - 8))
  else None

let () =
  let file = Sys.argv.(1) in
  let base = Filename.remove_extension (Filename.basename file) in
  let out = base ^ ".html" in
  let title =
    match extract_module_name file with
    | Some name -> name
    | None -> base
  in
  (* Parse and check the document *)
  let doc =
    match Pantagruel.Module.parse_file file with
    | Error e ->
        Printf.eprintf "%s\n"
          (match e with
          | Pantagruel.Module.ModuleNotFound name ->
              Printf.sprintf "error: Module '%s' not found" name
          | Pantagruel.Module.CyclicImport modules ->
              Printf.sprintf "error: Cyclic import: %s"
                (String.concat " -> " modules)
          | Pantagruel.Module.ParseError (_, msg) -> msg);
        exit 1
    | Ok doc -> doc
  in
  let dir = Filename.dirname file in
  let registry = Pantagruel.Module.scan_module_path dir in
  let env =
    match Pantagruel.Module.check_with_imports registry doc with
    | Error e ->
        Printf.eprintf "%s\n"
          (match e with
          | Pantagruel.Module.ModuleNotFound name ->
              Printf.sprintf "error: Module '%s' not found" name
          | Pantagruel.Module.CyclicImport modules ->
              Printf.sprintf "error: Cyclic import: %s"
                (String.concat " -> " modules)
          | Pantagruel.Module.ParseError (_, msg) -> msg);
        exit 1
    | Ok env -> env
  in
  (* Pipe markdown through pandoc *)
  let md_cmd =
    Printf.sprintf "pandoc -s --css=style.css --metadata title=%s -o %s"
      (Filename.quote title) (Filename.quote out)
  in
  let oc = Unix.open_process_out md_cmd in
  let fmt = Format.formatter_of_out_channel oc in
  Format.pp_set_margin fmt 10000;
  let procs = Pantagruel.Markdown_output.rule_names_of_env env in
  Pantagruel.Markdown_output.pp_document procs fmt doc;
  Format.pp_print_flush fmt ();
  match Unix.close_process_out oc with
  | Unix.WEXITED code -> exit code
  | _ -> exit 1
