open Js_of_ocaml
open Pantagruel_parser

(** Parse a proposition string into an AST expression. *)
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

(** Parse a full document string. Returns an error message with location on
    parse failure. *)
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

(** Type-check a full Pantagruel document string. Returns [None] on success or
    [Some message] on failure. Documents with imports are rejected — the wasm
    build has no module registry, so cross-module checking must go through the
    [pant] CLI. *)
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

(** Format a [Module.module_error] into a human-readable string. The pant CLI
    produces these via [Error.format_module_error]; we format inline here to
    match the wasm-side error-string convention used by [check_document_string].
*)
let format_module_error : Module.module_error -> string = function
  | Module.ModuleNotFound name ->
      Printf.sprintf "error: module not found: %s" name
  | Module.CyclicImport chain ->
      Printf.sprintf "error: cyclic import: %s" (String.concat " -> " chain)
  | Module.ParseError (_file, msg) ->
      (* [msg] already carries file:line:col when sourced from a parse error;
         pass through verbatim so callers see one error string, not a doubled
         "file: file:line:col: ..." prefix. *)
      msg

(** Type-check a consumer document against an in-memory bundle of dep modules.
    Each [(name, text)] pair is parsed and registered as a [module_entry] keyed
    by [name] — the same key the consumer's [import NAME.] declarations resolve
    against. Returns [None] on success or [Some message] on failure. Mirrors the
    pant CLI's import semantics ([Module.check_with_imports]) by populating
    [module_entry.ast] from in-memory text rather than reading a file. *)
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

(** Collect names bound by guards (GIn introduces a binding). *)
let bound_names_from_guards (guards : Ast.guard list) : string list =
  List.filter_map
    (function
      | Ast.GIn (name, _) -> Some (Ast.lower_name name)
      | Ast.GParam _ | Ast.GExpr _ -> None)
    guards

(** Apply variable renames to an AST expression. Capture-avoiding: renames are
    dropped when the source or destination name is bound by a quantifier (params
    or GIn guards), preventing both self-rename and variable capture. *)
let rec rename_expr (renames : (string * string) list) (e : Ast.expr) : Ast.expr
    =
  let r = rename_expr renames in
  let rename_var name =
    match List.assoc_opt name renames with
    | Some new_name -> new_name
    | None -> name
  in
  match e with
  | EVar (Lower name) -> EVar (Lower (rename_var name))
  | EPrimed (Lower name) -> EPrimed (Lower (rename_var name))
  | EDomain (Upper name) -> (
      match(* User annotations carry TS-cased identifiers — including
         UPPER_CASE constants like `UNSUPPORTED_UNKNOWN` — that the Pant
         parser routes to [EDomain] because uppercase identifiers lex
         as [UPPER_IDENT]. The rename layer carries
         `UNSUPPORTED_UNKNOWN -> unsupported-unknown` (the kebab'd name
         of a 0-arity rule emitted by ts2pant's module-const extractor),
         and the rewrite has to switch the AST constructor too: the new
         lowercase name is a term reference [EVar (Lower _)], not a
         domain. Names that don't downcase under the rename remain
         [EDomain]. *)
           [@warning "-4"]
        List.assoc_opt name renames
      with
      | Some new_name when String.length new_name > 0 ->
          let first = new_name.[0] in
          if first >= 'a' && first <= 'z' then EVar (Lower new_name)
          else EDomain (Upper new_name)
      | Some _ | None -> EDomain (Upper name))
  | EApp (f, args) -> EApp (r f, List.map r args)
  | EBinop (op, e1, e2) -> EBinop (op, r e1, r e2)
  | EUnop (op, e1) -> EUnop (op, r e1)
  | EForall (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let renames' =
        List.filter
          (fun (old_name, new_name) ->
            (not (List.mem old_name bound)) && not (List.mem new_name bound))
          renames
      in
      Ast.make_forall params
        (List.map (rename_guard renames') guards)
        (rename_expr renames' body)
  | EExists (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let renames' =
        List.filter
          (fun (old_name, new_name) ->
            (not (List.mem old_name bound)) && not (List.mem new_name bound))
          renames
      in
      Ast.make_exists params
        (List.map (rename_guard renames') guards)
        (rename_expr renames' body)
  | EEach (mb, metas, comb) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let renames' =
        List.filter
          (fun (old_name, new_name) ->
            (not (List.mem old_name bound)) && not (List.mem new_name bound))
          renames
      in
      Ast.make_each params
        (List.map (rename_guard renames') guards)
        comb
        (rename_expr renames' body)
  | ECond arms -> ECond (List.map (fun (g, c) -> (r g, r c)) arms)
  | ETuple es -> ETuple (List.map r es)
  | EProj (e1, n) -> EProj (r e1, n)
  | EOverride (Lower name, pairs) ->
      EOverride
        (Lower (rename_var name), List.map (fun (k, v) -> (r k, r v)) pairs)
  | EInitially e1 -> EInitially (r e1)
  | EQualified _ | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ -> e

and rename_guard renames = function
  | Ast.GParam p -> Ast.GParam p
  | Ast.GIn (name, e) -> Ast.GIn (name, rename_expr renames e)
  | Ast.GExpr e -> Ast.GExpr (rename_expr renames e)

(** Collect free variable names in an expression (for capture checks). *)
let rec free_vars (e : Ast.expr) : string list =
  match e with
  | EVar (Lower n) -> [ n ]
  | EPrimed (Lower n) -> [ n ]
  | EApp (f, args) -> free_vars f @ List.concat_map free_vars args
  | EBinop (_, e1, e2) -> free_vars e1 @ free_vars e2
  | EUnop (_, e1) -> free_vars e1
  | EForall (mb, metas) | EExists (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let inner = List.concat_map free_vars_guard guards @ free_vars body in
      List.filter (fun n -> not (List.mem n bound)) inner
  | EEach (mb, metas, _) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let inner = List.concat_map free_vars_guard guards @ free_vars body in
      List.filter (fun n -> not (List.mem n bound)) inner
  | ECond arms -> List.concat_map (fun (g, c) -> free_vars g @ free_vars c) arms
  | ETuple es -> List.concat_map free_vars es
  | EProj (e1, _) -> free_vars e1
  | EOverride (Lower n, pairs) ->
      n :: List.concat_map (fun (k, v) -> free_vars k @ free_vars v) pairs
  | EInitially e1 -> free_vars e1
  | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
  | ELitBool _ ->
      []

and free_vars_guard = function
  | Ast.GParam _ -> []
  | Ast.GIn (_, e) -> free_vars e
  | Ast.GExpr e -> free_vars e

(** Substitute a variable name with an arbitrary expression. Capture-avoiding:
    skips substitution under binders that shadow the target name or would
    capture free variables from the replacement expression. *)
let rec subst_var (name : string) (replacement : Ast.expr) (e : Ast.expr) :
    Ast.expr =
  let s = subst_var name replacement in
  match e with
  | EVar (Lower n) when n = name -> replacement
  | EVar _ | EPrimed _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _
  | ELitString _ | ELitBool _ ->
      e
  | EApp (f, args) -> EApp (s f, List.map s args)
  | EBinop (op, e1, e2) -> EBinop (op, s e1, s e2)
  | EUnop (op, e1) -> EUnop (op, s e1)
  | EForall (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      in
      if List.mem name bound then e
      else if List.exists (fun n -> List.mem n bound) (free_vars replacement)
      then e
      else
        Ast.make_forall params
          (List.map (subst_guard name replacement) guards)
          (s body)
  | EExists (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      in
      if List.mem name bound then e
      else if List.exists (fun n -> List.mem n bound) (free_vars replacement)
      then e
      else
        Ast.make_exists params
          (List.map (subst_guard name replacement) guards)
          (s body)
  | EEach (mb, metas, comb) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      in
      if List.mem name bound then e
      else if List.exists (fun n -> List.mem n bound) (free_vars replacement)
      then e
      else
        Ast.make_each params
          (List.map (subst_guard name replacement) guards)
          comb (s body)
  | ECond arms -> ECond (List.map (fun (g, c) -> (s g, s c)) arms)
  | ETuple es -> ETuple (List.map s es)
  | EProj (e1, n) -> EProj (s e1, n)
  | EOverride (Lower n, pairs) ->
      EOverride (Lower n, List.map (fun (k, v) -> (s k, s v)) pairs)
  | EInitially e1 -> EInitially (s e1)

and subst_guard name replacement = function
  | Ast.GParam p -> Ast.GParam p
  | Ast.GIn (n, e) -> Ast.GIn (n, subst_var name replacement e)
  | Ast.GExpr e -> Ast.GExpr (subst_var name replacement e)

(* --- Helpers for JS array conversion --- *)

let js_array_to_list arr = Array.to_list (Js.to_array arr)

let js_array_to_pair_list arr =
  js_array_to_list arr
  |> List.map (fun pair ->
      let a = Js.to_array pair in
      (Array.get a 0, Array.get a 1))

(** Parser exports: parse, rename, and pretty-print Pantagruel expressions. *)
let () =
  Js.export "pantParser"
    (object%js
       method parseAndRename text renames_js =
         let text = Js.to_string text in
         let renames =
           Js.to_array renames_js |> Array.to_list
           |> List.map (fun pair ->
               let arr = Js.to_array pair in
               (Js.to_string (Array.get arr 0), Js.to_string (Array.get arr 1)))
         in
         match parse_expr_string text with
         | Ok expr ->
             let renamed = rename_expr renames expr in
             Js.some (Js.string (Pretty.str_expr renamed))
         | Error _ -> Js.null

       method prettyPrint text =
         let text = Js.to_string text in
         match parse_expr_string text with
         | Ok expr -> Js.some (Js.string (Pretty.str_expr expr))
         | Error _ -> Js.null

       method checkDocument text =
         let text = Js.to_string text in
         match check_document_string text with
         | None -> Js.null
         | Some msg -> Js.some (Js.string msg)

       method checkDocumentWithDeps consumer_text deps_array =
         let consumer = Js.to_string consumer_text in
         let deps =
           Js.to_array deps_array |> Array.to_list
           |> List.map (fun pair ->
               let arr = Js.to_array pair in
               (Js.to_string (Array.get arr 0), Js.to_string (Array.get arr 1)))
         in
         match check_document_with_deps consumer deps with
         | None -> Js.null
         | Some msg -> Js.some (Js.string msg)
    end)

(** AST constructor exports: build Pantagruel AST nodes from JavaScript. *)
let () =
  Js.export "pantAst"
    (object%js
       method var name = Ast.EVar (Lower (Js.to_string name))
       method domain name = Ast.EDomain (Upper (Js.to_string name))
       method app fn args = Ast.EApp (fn, js_array_to_list args)
       method primed name = Ast.EPrimed (Lower (Js.to_string name))
       method binop op l r = Ast.EBinop (op, l, r)
       method unop op e = Ast.EUnop (op, e)
       method litNat n = Ast.ELitNat n
       method litBool b = Ast.ELitBool (Js.to_bool b)
       method litString s = Ast.ELitString (Js.to_string s)
       method cond arms = Ast.ECond (js_array_to_pair_list arms)
       method tuple es = Ast.ETuple (js_array_to_list es)
       method proj e n = Ast.EProj (e, n)

       method override name pairs =
         Ast.EOverride (Lower (Js.to_string name), js_array_to_pair_list pairs)

       method initially e = Ast.EInitially e

       method forall params guards body =
         Ast.make_forall (js_array_to_list params) (js_array_to_list guards)
           body

       method each params guards body =
         Ast.make_each (js_array_to_list params) (js_array_to_list guards) None
           body

       method eachComb params guards comb body =
         Ast.make_each (js_array_to_list params) (js_array_to_list guards)
           (Some comb) body

       method exists params guards body =
         Ast.make_exists (js_array_to_list params) (js_array_to_list guards)
           body

       method opAnd = Ast.OpAnd
       method opOr = Ast.OpOr
       method opImpl = Ast.OpImpl
       method opIff = Ast.OpIff
       method opEq = Ast.OpEq
       method opNeq = Ast.OpNeq
       method opLt = Ast.OpLt
       method opGt = Ast.OpGt
       method opLe = Ast.OpLe
       method opGe = Ast.OpGe
       method opIn = Ast.OpIn
       method opSubset = Ast.OpSubset
       method opAdd = Ast.OpAdd
       method opSub = Ast.OpSub
       method opMul = Ast.OpMul
       method opDiv = Ast.OpDiv
       method opNot = Ast.OpNot
       method opNeg = Ast.OpNeg
       method opCard = Ast.OpCard
       method combAdd = Ast.CombAdd
       method combMul = Ast.CombMul
       method combAnd = Ast.CombAnd
       method combOr = Ast.CombOr
       method combMin = Ast.CombMin
       method combMax = Ast.CombMax

       method param name typeExpr =
         ({ Ast.param_name = Lower (Js.to_string name); param_type = typeExpr }
           : Ast.param)

       method tName name = Ast.TName (Upper (Js.to_string name))
       method tList t = Ast.TList t
       method tProduct ts = Ast.TProduct (js_array_to_list ts)
       method tSum ts = Ast.TSum (js_array_to_list ts)
       method gExpr e = Ast.GExpr e
       method gIn name e = Ast.GIn (Lower (Js.to_string name), e)
       method gParam p = Ast.GParam p
       method declDomain name = Ast.DeclDomain (Upper (Js.to_string name))

       method declAlias name typeExpr =
         Ast.DeclAlias (Upper (Js.to_string name), typeExpr)

       method declRule name params guards returnType =
         Ast.DeclRule
           {
             name = Lower (Js.to_string name);
             params = js_array_to_list params;
             guards = js_array_to_list guards;
             return_type = returnType;
             contexts = [];
           }

       method declAction label params guards =
         Ast.DeclAction
           {
             label = Js.to_string label;
             params = js_array_to_list params;
             guards = js_array_to_list guards;
             contexts = [];
           }

       method strExpr e = Js.string (Pretty.str_expr e)
       method strTypeExpr te = Js.string (Pretty.str_type_expr te)
       method strDecl d = Js.string (Pretty.str_declaration d)

       method substituteBinder expr name replacement =
         subst_var (Js.to_string name) replacement expr
    end)
