(** Pantagruel CLI *)

let version = "0.1.0"

let usage = "pant [options] [file.pant]

Pantagruel: A specification language checker

If no file is given, reads from stdin.

Options:"

let print_ast = ref false
let print_json = ref false
let print_markdown = ref false
let do_normalize = ref false
let module_path = ref "."
let files = ref []

let specs = [
  ("--module-path", Arg.Set_string module_path,
   "DIR  Directory to search for imported modules (default: .)");
  ("--ast", Arg.Set print_ast,
   "     Print AST (OCaml format) and exit");
  ("--json", Arg.Set print_json,
   "    Output JSON and exit");
  ("--markdown", Arg.Set print_markdown,
   " Output Pandoc Markdown with LaTeX math and exit");
  ("--normalize", Arg.Set do_normalize,
   " Output N-normal form and exit");
  ("--version", Arg.Unit (fun () -> print_endline ("pant " ^ version); exit 0),
   "  Print version and exit");
]

let add_file f = files := f :: !files

(** Format module errors for human-readable output *)
let format_module_error = function
  | Pantagruel.Module.ModuleNotFound name ->
      Printf.sprintf "error: Module '%s' not found" name
  | Pantagruel.Module.CyclicImport modules ->
      Printf.sprintf "error: Cyclic import: %s" (String.concat " -> " modules)
  | Pantagruel.Module.ImportCollision (name, mod1, mod2) ->
      Printf.sprintf "error: Import collision for '%s' between %s and %s"
        name mod1 mod2
  | Pantagruel.Module.ParseError (_, msg) ->
      msg  (* Already formatted by Error module *)

let check_doc doc =
  if !print_ast then begin
    print_endline (Pantagruel.Ast.show_document doc);
    true
  end
  else begin
    (* Set up module registry *)
    let registry = Pantagruel.Module.scan_module_path !module_path in

    (* Check the document *)
    match Pantagruel.Module.check_with_imports registry doc with
    | Ok env ->
        if !do_normalize then begin
          let normalized = Pantagruel.Normalize.normalize doc in
          Pantagruel.Pretty.output normalized
        end
        else if !print_json then
          Pantagruel.Json_output.output_json env doc
        else if !print_markdown then
          Pantagruel.Markdown_output.output doc
        else
          print_endline "OK";
        true
    | Error e ->
        prerr_endline (format_module_error e);
        false
  end

let check_file path =
  match Pantagruel.Module.parse_file path with
  | Error e ->
      prerr_endline (format_module_error e);
      false
  | Ok doc -> check_doc doc

let check_stdin () =
  match Pantagruel.Module.parse_channel "<stdin>" stdin with
  | Error e ->
      prerr_endline (format_module_error e);
      false
  | Ok doc -> check_doc doc

let () =
  Arg.parse specs add_file usage;

  match !files with
  | [] ->
      let ok = check_stdin () in
      exit (if ok then 0 else 1)
  | _ ->
      let all_ok = List.for_all check_file (List.rev !files) in
      exit (if all_ok then 0 else 1)
