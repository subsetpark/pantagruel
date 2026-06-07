(* @archlint.module shell
   @archlint.domain pantagruel.html-title *)

let () =
  let file = Sys.argv.(1) in
  let base = Filename.remove_extension (Filename.basename file) in
  let out = base ^ ".html" in
  let title =
    let ic = open_in file in
    let line = try input_line ic with End_of_file -> "" in
    close_in ic;
    match Pantagruel.Html_title.module_name_from_first_line line with
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
    | Ok (env, warnings) ->
        List.iter
          (fun w ->
            Printf.eprintf "%s\n" (Pantagruel.Error.format_type_warning w))
          warnings;
        env
  in
  (* Pipe markdown through pandoc *)
  let md_cmd =
    Printf.sprintf "pandoc -s --css=style.css --metadata title=%s -o %s"
      (Filename.quote title) (Filename.quote out)
  in
  let oc = Unix.open_process_out md_cmd in
  output_string oc (Pantagruel.Markdown_output.document_to_markdown env doc);
  match Unix.close_process_out oc with
  | Unix.WEXITED code -> exit code
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> exit 1
