(* @archlint.module core
   @archlint.domain pantagruel.wasm-expr *)

(** Pure expression transformations used by the WASM bindings. *)

open Ast

let bound_names_from_guards (guards : Ast.guard list) : string list =
  List.filter_map
    (function
      | Ast.GIn (name, _) -> Some (Ast.lower_name name)
      | Ast.GParam p -> Some (Ast.lower_name p.param_name)
      | Ast.GExpr _ -> None)
    guards

let rec rename_expr (renames : (string * string) list) (e : Ast.expr) : Ast.expr
    =
  let r = rename_expr renames in
  let rename_var name =
    match List.assoc_opt name renames with
    | Some new_name -> new_name
    | None -> name
  in
  let filter_bound_renames params guards =
    let bound =
      List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      @ bound_names_from_guards guards
    in
    List.filter
      (fun (old_name, new_name) ->
        (not (List.mem old_name bound)) && not (List.mem new_name bound))
      renames
  in
  let rename_guard renames = function
    | Ast.GParam p -> Ast.GParam p
    | Ast.GIn (name, e) -> Ast.GIn (name, rename_expr renames e)
    | Ast.GExpr e -> Ast.GExpr (rename_expr renames e)
  in
  match e with
  | EVar (Lower name) -> EVar (Lower (rename_var name))
  | EPrimed (Lower name) -> EPrimed (Lower (rename_var name))
  | EDomain (Upper name) -> (
      match[@warning "-4"] List.assoc_opt name renames with
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
      let renames' = filter_bound_renames params guards in
      Ast.make_forall params
        (List.map (rename_guard renames') guards)
        (rename_expr renames' body)
  | EExists (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let renames' = filter_bound_renames params guards in
      Ast.make_exists params
        (List.map (rename_guard renames') guards)
        (rename_expr renames' body)
  | EEach (mb, metas, comb) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      let renames' = filter_bound_renames params guards in
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

let free_vars (e : Ast.expr) : string list =
  let rec expr e =
    match e with
    | EVar (Lower n) -> [ n ]
    | EPrimed (Lower n) -> [ n ]
    | EApp (f, args) -> expr f @ List.concat_map expr args
    | EBinop (_, e1, e2) -> expr e1 @ expr e2
    | EUnop (_, e1) -> expr e1
    | EForall (mb, metas) | EExists (mb, metas) ->
        let params, guards, body = Ast.unbind_quant mb metas in
        let bound =
          List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
          @ bound_names_from_guards guards
        in
        let inner = List.concat_map guard guards @ expr body in
        List.filter (fun n -> not (List.mem n bound)) inner
    | EEach (mb, metas, _) ->
        let params, guards, body = Ast.unbind_quant mb metas in
        let bound =
          List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
          @ bound_names_from_guards guards
        in
        let inner = List.concat_map guard guards @ expr body in
        List.filter (fun n -> not (List.mem n bound)) inner
    | ECond arms -> List.concat_map (fun (g, c) -> expr g @ expr c) arms
    | ETuple es -> List.concat_map expr es
    | EProj (e1, _) -> expr e1
    | EOverride (Lower n, pairs) ->
        n :: List.concat_map (fun (k, v) -> expr k @ expr v) pairs
    | EInitially e1 -> expr e1
    | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ ->
        []
  and guard = function
    | Ast.GParam _ -> []
    | Ast.GIn (_, e) -> expr e
    | Ast.GExpr e -> expr e
  in
  expr e

let rec subst_var (name : string) (replacement : Ast.expr) (e : Ast.expr) :
    Ast.expr =
  let s = subst_var name replacement in
  let subst_guard name replacement = function
    | Ast.GParam p -> Ast.GParam p
    | Ast.GIn (n, e) -> Ast.GIn (n, subst_var name replacement e)
    | Ast.GExpr e -> Ast.GExpr (subst_var name replacement e)
  in
  let rebuild_quant make params guards body =
    make params (List.map (subst_guard name replacement) guards) (s body)
  in
  let may_substitute params guards =
    let bound =
      List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
      @ bound_names_from_guards guards
    in
    (not (List.mem name bound))
    && not (List.exists (fun n -> List.mem n bound) (free_vars replacement))
  in
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
      if may_substitute params guards then
        rebuild_quant Ast.make_forall params guards body
      else e
  | EExists (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      if may_substitute params guards then
        rebuild_quant Ast.make_exists params guards body
      else e
  | EEach (mb, metas, comb) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      if may_substitute params guards then
        Ast.make_each params
          (List.map (subst_guard name replacement) guards)
          comb (s body)
      else e
  | ECond arms -> ECond (List.map (fun (g, c) -> (s g, s c)) arms)
  | ETuple es -> ETuple (List.map s es)
  | EProj (e1, n) -> EProj (s e1, n)
  | EOverride (Lower n, pairs) ->
      EOverride (Lower n, List.map (fun (k, v) -> (s k, s v)) pairs)
  | EInitially e1 -> EInitially (s e1)
