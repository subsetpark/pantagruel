(* @archlint.module core
   @archlint.domain pantagruel.wasm-document *)

(** Pure document and expression services used by the WASM bindings. *)

let parse_expr_string (text : string) : (Ast.expr, string) result =
  try
    let lexer = Lexer.create_from_string "<annotation>" text in
    let supplier = Lexer.menhir_token lexer in
    Ok
      (MenhirLib.Convert.Simplified.traditional2revised Parser.standalone_expr
         supplier)
  with
  | Lexer.Lexer_error (_, msg) -> Error msg
  | _ -> Error (Printf.sprintf "Parse error in: %s" text)

let parse_document_string (filename : string) (text : string) :
    (Ast.document, string) result =
  try
    let lexer = Lexer.create_from_string filename text in
    Lexer.set_current lexer;
    let supplier = Lexer.menhir_token lexer in
    Ok
      (MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier)
  with
  | Lexer.Lexer_error (loc, msg) ->
      Error
        (Printf.sprintf "%s:%d:%d: error: %s" loc.Ast.file loc.Ast.line
           loc.Ast.col msg)
  | _ -> Error (Printf.sprintf "%s: error: parse error" filename)

let check_document_string (text : string) : string option =
  match parse_document_string "<wasm-input>" text with
  | Error msg -> Some msg
  | Ok doc -> (
      if doc.Ast.imports <> [] then
        Some
          "error: wasm typechecker does not support imports; use the pant CLI \
           for cross-module checking"
      else
        let mod_name =
          Option.fold ~none:"" ~some:Ast.upper_name doc.module_name
        in
        let base_env = Env.empty mod_name in
        match Collect.collect_all ~base_env doc with
        | Error e -> Some (Error.format_collect_error e)
        | Ok env -> (
            match Check.check_document env doc with
            | Ok _warnings -> None
            | Error e -> Some (Error.format_type_error e)))

let format_module_error : Module.module_error -> string = function
  | Module.ModuleNotFound name ->
      Printf.sprintf "error: module not found: %s" name
  | Module.CyclicImport chain ->
      Printf.sprintf "error: cyclic import: %s" (String.concat " -> " chain)
  | Module.ParseError (_file, msg) -> msg

let check_document_with_deps (consumer_text : string)
    (deps : (string * string) list) : string option =
  match parse_document_string "<wasm-input>" consumer_text with
  | Error msg -> Some msg
  | Ok consumer_doc -> (
      let parse_one_dep (name, text) =
        match parse_document_string ("<dep:" ^ name ^ ">") text with
        | Error msg -> Error msg
        | Ok ast -> Ok (name, ast)
      in
      let rec parse_deps = function
        | [] -> Ok []
        | dep :: rest -> (
            match parse_one_dep dep with
            | Error msg -> Error msg
            | Ok parsed -> (
                match parse_deps rest with
                | Error msg -> Error msg
                | Ok rest' -> Ok (parsed :: rest')))
      in
      match parse_deps deps with
      | Error msg -> Some msg
      | Ok parsed_deps -> (
          let modules =
            List.fold_left
              (fun acc (name, ast) ->
                let entry : Module.module_entry =
                  {
                    name;
                    path = "<in-memory:" ^ name ^ ">";
                    ast = Some ast;
                    env = None;
                  }
                in
                Module.StringMap.add name entry acc)
              Module.StringMap.empty parsed_deps
          in
          let registry = { Module.modules; loading = [] } in
          match Module.check_with_imports registry consumer_doc with
          | Ok _ -> None
          | Error e -> Some (format_module_error e)))
