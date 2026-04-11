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

(** Apply variable renames to an AST expression. Quantifier-bound names are not
    renamed (they introduce fresh bindings). *)
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
      (* Shadow any renames that are re-bound by quantifier params *)
      let bound =
        List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
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

(** Main exports: parse, rename, and pretty-print Pantagruel expressions. *)
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
       (** Parse a proposition, apply variable renames, pretty-print back.
           Returns null on parse failure. *)

       method prettyPrint text =
         let text = Js.to_string text in
         match parse_expr_string text with
         | Ok expr -> Js.some (Js.string (Pretty.str_expr expr))
         | Error _ -> Js.null
       (** Parse and pretty-print (round-trip). Returns null on parse failure.
       *)
    end)
