(** Pantagruel CLI *)

let version = "0.1.0"

let usage = "pantagruel [options] <file.pant>

Pantagruel: A specification language checker

Options:"

let print_ast = ref false
let module_path = ref "."
let files = ref []

let specs = [
  ("--module-path", Arg.Set_string module_path,
   "DIR  Directory to search for imported modules (default: .)");
  ("--ast", Arg.Set print_ast,
   "     Print AST and exit");
  ("--version", Arg.Unit (fun () -> print_endline ("pantagruel " ^ version); exit 0),
   "  Print version and exit");
]

let add_file f = files := f :: !files

let check_file path =
  (* Parse the file *)
  match Pantagruel.Module.parse_file path with
  | Error (Pantagruel.Module.ParseError (_, msg)) ->
      prerr_endline msg;
      false
  | Error e ->
      prerr_endline (Pantagruel.Module.show_module_error e);
      false
  | Ok doc ->
      if !print_ast then begin
        print_endline (Pantagruel.Ast.show_document doc);
        true
      end
      else begin
        (* Set up module registry *)
        let registry = Pantagruel.Module.scan_module_path !module_path in

        (* Check the document *)
        match Pantagruel.Module.check_with_imports registry doc with
        | Ok () ->
            print_endline "OK";
            true
        | Error (Pantagruel.Module.ParseError (_, msg)) ->
            prerr_endline msg;
            false
        | Error e ->
            prerr_endline (Pantagruel.Module.show_module_error e);
            false
      end

let () =
  Arg.parse specs add_file usage;

  match !files with
  | [] ->
      prerr_endline "Error: no input file";
      prerr_endline usage;
      exit 1
  | _ ->
      let all_ok = List.for_all check_file (List.rev !files) in
      exit (if all_ok then 0 else 1)
