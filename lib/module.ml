(** Module system: loading and import resolution *)

module StringMap = Map.Make (String)

type module_error =
  | ModuleNotFound of string
  | CyclicImport of string list
  | ParseError of string * string  (** file, message *)
[@@deriving show]

type module_entry = {
  name : string;
  path : string;
  mutable ast : Ast.document option;
  mutable env : Env.t option;
}
(** Information about a module *)

type registry = {
  modules : module_entry StringMap.t;
  mutable loading : string list;  (** Stack for cycle detection *)
}
(** Module registry *)

let create_registry () = { modules = StringMap.empty; loading = [] }

(** Parse a file and extract just the module name *)
let parse_module_header path =
  try
    let channel = open_in path in
    let lexer = Lexer.create_from_channel path channel in
    (* Read tokens until we find MODULE <name> DOT *)
    let rec find_module () =
      match Lexer.token lexer with
      | Parser.MODULE -> (
          match Lexer.token lexer with
          | Parser.UPPER_IDENT name -> (
              match Lexer.token lexer with
              | Parser.DOT ->
                  close_in channel;
                  Ok name
              | _ ->
                  close_in channel;
                  Error "Expected '.' after module name")
          | _ ->
              close_in channel;
              Error "Expected module name")
      | Parser.EOF ->
          close_in channel;
          Error "No module declaration found"
      | _ -> find_module ()
    in
    find_module ()
  with
  | Lexer.Lexer_error (_, msg) -> Error msg
  | Sys_error msg -> Error msg

(** Parse a full document from a channel *)

module I = Parser.MenhirInterpreter

let parse_channel filename channel =
  try
    let lexer = Lexer.create_from_channel filename channel in
    Lexer.set_current lexer;
    (* Set current lexer for doc comment access *)
    let supplier = Lexer.menhir_token lexer in
    let initial_pos = Lexer.lexing_position lexer in
    let checkpoint = Parser.Incremental.document initial_pos in
    I.loop_handle_undo
      (fun doc -> Ok doc)
      (fun inputneeded_cp _error_cp ->
        let loc = Lexer.current_loc lexer in
        let unexpected =
          match lexer.last_token with
          | Some tok -> Lexer.string_of_token tok
          | None -> "start of input"
        in
        let expected =
          let pos = Lexer.lexing_position lexer in
          List.filter_map
            (fun tok ->
              if I.acceptable inputneeded_cp tok pos then
                Some (Lexer.describe_token tok)
              else None)
            Lexer.all_tokens
          |> List.sort_uniq String.compare
        in
        let expected_str =
          match expected with
          | [] -> ""
          | _ -> ", expected " ^ String.concat ", " expected
        in
        Error
          (ParseError
             ( filename,
               Printf.sprintf "%s:%d:%d: error: Parse error: unexpected %s%s"
                 loc.Ast.file loc.Ast.line loc.Ast.col unexpected expected_str
             )))
      supplier checkpoint
  with Lexer.Lexer_error (loc, msg) ->
    Error
      (ParseError
         ( filename,
           Printf.sprintf "%s:%d:%d: error: %s" loc.Ast.file loc.Ast.line
             loc.Ast.col msg ))

(** Parse a full document from a file *)
let parse_file path =
  try
    let channel = open_in path in
    let result = parse_channel path channel in
    close_in channel;
    result
  with Sys_error msg -> Error (ParseError (path, msg))

open Util

(** Scan a directory for .pant files and build registry *)
let scan_module_path dir =
  let registry = create_registry () in
  try
    let entries = Sys.readdir dir in
    let modules =
      Array.fold_left
        (fun acc filename ->
          if Filename.check_suffix filename ".pant" then
            let path = Filename.concat dir filename in
            match parse_module_header path with
            | Ok name ->
                StringMap.add name { name; path; ast = None; env = None } acc
            | Error _ -> acc (* Skip unparseable files *)
          else acc)
        registry.modules entries
    in
    { registry with modules }
  with Sys_error _ -> registry (* Directory doesn't exist or can't be read *)

(** Load and process a module *)
let rec load_module registry name =
  match StringMap.find_opt name registry.modules with
  | None -> Error (ModuleNotFound name)
  | Some entry ->
      (* Check for cycles *)
      if List.mem name registry.loading then
        Error (CyclicImport (List.rev (name :: registry.loading)))
      else begin
        (* Return cached result if available *)
        match entry.env with
        | Some env -> Ok env
        | None ->
            (* Parse if needed *)
            let* ast =
              match entry.ast with
              | Some ast -> Ok ast
              | None -> (
                  match parse_file entry.path with
                  | Ok doc ->
                      entry.ast <- Some doc;
                      Ok doc
                  | Error e -> Error e)
            in

            (* Track loading for cycle detection *)
            registry.loading <- name :: registry.loading;

            let result =
              let* import_env =
                List.fold_left
                  (fun env_result imp ->
                    let* env = env_result in
                    let* imp_env = load_module registry imp.Ast.value in
                    Ok (Env.add_import env imp_env imp.Ast.value))
                  (Ok (Env.empty name))
                  ast.Ast.imports
              in

              (* Collect declarations on top of import env *)
              let* full_env =
                match Collect.collect_all ~base_env:import_env ast with
                | Ok env -> Ok env
                | Error e ->
                    Error
                      (ParseError (entry.path, Error.format_collect_error e))
              in

              (* Cache result *)
              entry.env <- Some full_env;
              Ok full_env
            in

            (* Always pop loading stack, even on error *)
            registry.loading <- List.tl registry.loading;
            result
      end

(** Check a document with its imports, returning the resolved environment *)
let check_with_imports registry (doc : Ast.document) =
  let mod_name = Option.value ~default:"" doc.module_name in
  (* Load all imports *)
  let* import_env =
    List.fold_left
      (fun env_result imp ->
        let* env = env_result in
        let* imp_env = load_module registry imp.Ast.value in
        Ok (Env.add_import env imp_env imp.Ast.value))
      (Ok (Env.empty mod_name))
      doc.imports
  in

  (* Collect declarations on top of import env *)
  let* full_env =
    match Collect.collect_all ~base_env:import_env doc with
    | Ok env -> Ok env
    | Error e -> Error (ParseError ("<main>", Error.format_collect_error e))
  in

  (* Type check *)
  match Check.check_document full_env doc with
  | Ok () -> Ok full_env
  | Error e -> Error (ParseError ("<main>", Error.format_type_error e))
