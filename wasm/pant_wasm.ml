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

(** Collect names bound by guards (GIn introduces a binding). *)
let bound_names_from_guards (guards : Ast.guard list) : string list =
  List.filter_map
    (function
      | Ast.GIn (name, _) -> Some (Ast.lower_name name)
      | Ast.GParam _ | Ast.GExpr _ -> None)
    guards

(** Apply variable renames to an AST expression. Quantifier-bound names
    (from params and GIn guards) are not renamed (they introduce fresh bindings). *)
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
  | EApp (f, args) -> EApp (r f, List.map r args)
  | EBinop (op, e1, e2) -> EBinop (op, r e1, r e2)
  | EUnop (op, e1) -> EUnop (op, r e1)
  | EForall (params, guards, body) ->
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let renames' =
        List.filter (fun (old_name, _) -> not (List.mem old_name bound)) renames
      in
      EForall
        ( params,
          List.map (rename_guard renames') guards,
          rename_expr renames' body )
  | EExists (params, guards, body) ->
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let renames' =
        List.filter (fun (old_name, _) -> not (List.mem old_name bound)) renames
      in
      EExists
        ( params,
          List.map (rename_guard renames') guards,
          rename_expr renames' body )
  | EEach (params, guards, comb, body) ->
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
        @ bound_names_from_guards guards
      in
      let renames' =
        List.filter (fun (old_name, _) -> not (List.mem old_name bound)) renames
      in
      EEach
        ( params,
          List.map (rename_guard renames') guards,
          comb,
          rename_expr renames' body )
  | ECond arms -> ECond (List.map (fun (g, c) -> (r g, r c)) arms)
  | ETuple es -> ETuple (List.map r es)
  | EProj (e1, n) -> EProj (r e1, n)
  | EOverride (Lower name, pairs) ->
      EOverride
        (Lower (rename_var name), List.map (fun (k, v) -> (r k, r v)) pairs)
  | EInitially e1 -> EInitially (r e1)
  | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
  | ELitBool _ ->
      e

and rename_guard renames = function
  | Ast.GParam p -> Ast.GParam p
  | Ast.GIn (name, e) -> Ast.GIn (name, rename_expr renames e)
  | Ast.GExpr e -> Ast.GExpr (rename_expr renames e)

(** Substitute a variable name with an arbitrary expression. Respects
    quantifier-bound variable shadowing. *)
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
  | EForall (params, guards, body) ->
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      in
      if List.mem name bound then e
      else
        EForall (params, List.map (subst_guard name replacement) guards, s body)
  | EExists (params, guards, body) ->
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      in
      if List.mem name bound then e
      else
        EExists (params, List.map (subst_guard name replacement) guards, s body)
  | EEach (params, guards, comb, body) ->
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      in
      if List.mem name bound then e
      else
        EEach
          (params, List.map (subst_guard name replacement) guards, comb, s body)
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
         Ast.EForall (js_array_to_list params, js_array_to_list guards, body)

       method each params guards body =
         Ast.EEach (js_array_to_list params, js_array_to_list guards, None, body)

       method eachComb params guards comb body =
         Ast.EEach
           (js_array_to_list params, js_array_to_list guards, Some comb, body)

       method exists params guards body =
         Ast.EExists (js_array_to_list params, js_array_to_list guards, body)

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
