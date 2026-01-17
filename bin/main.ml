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

let check_file path =
  (* Parse the file *)
  match Pantagruel.Module.parse_file path with
  | Error e ->
      prerr_endline (format_module_error e);
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
        | Error e ->
            prerr_endline (format_module_error e);
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
