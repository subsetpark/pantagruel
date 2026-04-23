(** SMT-LIB2 document analysis: chapter classification, invariant/action
    collection, frame conditions, action parameter handling *)

open Ast
open Types
open Smt_types
open Smt_preamble

(** Classify chapters into invariant chapters and action chapters *)
type chapter_class =
  | Invariant of {
      head_bindings : param list;
      propositions : expr located list;
      checks : expr located list;
    }
  | Action of {
      label : string;
      params : param list;
      guards : guard list;
      contexts : string list;
      head_bindings : param list;
          (** Rule parameters declared in the same chapter head as the action.
              These are visible in action-body propositions per REFERENCE.md
              "Forward Declaration Rules" but are not declared as SMT constants
              (only action params are). Used by [collect_actions] to
              auto-quantify propositions that reference them, matching the
              treatment [bind_head_params] gives to invariants. *)
      propositions : expr located list;
      checks : expr located list;
    }

let classify_chapter (chapter : chapter) =
  let action =
    List.find_map
      (fun (decl : declaration located) ->
        match decl.value with
        | DeclAction { label; params; guards; contexts } ->
            Some (label, params, guards, List.map Ast.upper_name contexts)
        | DeclDomain _ | DeclAlias _ | DeclRule _ | DeclClosure _ -> None)
      chapter.head
  in
  let head_bindings =
    List.concat_map
      (fun (decl : declaration located) ->
        match decl.value with
        | DeclRule { params; _ } -> params
        | DeclDomain _ | DeclAlias _ | DeclAction _ | DeclClosure _ -> [])
      chapter.head
  in
  match action with
  | Some (label, params, guards, contexts) ->
      Action
        {
          label;
          params;
          guards;
          contexts;
          head_bindings;
          propositions = chapter.body;
          checks = chapter.checks;
        }
  | None ->
      Invariant
        { head_bindings; propositions = chapter.body; checks = chapter.checks }

let classify_chapters (doc : document) = List.map classify_chapter doc.chapters

module StringSet = Set.Make (String)
(** [free_vars e] is the set of [lower_ident] names that appear free in [e].
    Used by [bind_head_params] to decide which head-level rule parameters
    actually need to be universally quantified for a given proposition. *)

let free_vars (e : expr) : StringSet.t =
  let bound_of_params (params : param list) =
    List.fold_left
      (fun s (p : param) -> StringSet.add (Ast.lower_name p.param_name) s)
      StringSet.empty params
  in
  let rec go acc = function
    | EVar (Lower n) | EPrimed (Lower n) -> StringSet.add n acc
    | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ | EDomain _
    | EQualified _ ->
        acc
    | EApp (f, args) -> List.fold_left go (go acc f) args
    | ETuple exprs -> List.fold_left go acc exprs
    | EProj (e, _) -> go acc e
    | EBinop (_, e1, e2) -> go (go acc e1) e2
    | EUnop (_, e) -> go acc e
    | EOverride (Lower n, pairs) ->
        let acc = StringSet.add n acc in
        List.fold_left (fun acc (k, v) -> go (go acc k) v) acc pairs
    | EForall (mb, metas) | EExists (mb, metas) ->
        let params, guards, body = Ast.unbind_quant mb metas in
        scope_quantifier acc params guards body
    | EEach (mb, metas, _comb) ->
        let params, guards, body = Ast.unbind_quant mb metas in
        scope_quantifier acc params guards body
    | ECond arms -> List.fold_left (fun acc (g, c) -> go (go acc g) c) acc arms
    | EInitially e -> go acc e
  and scope_quantifier acc params guards body =
    let initial_bound = bound_of_params params in
    let final_bound, guard_free = scan_guards initial_bound guards in
    let body_free = go StringSet.empty body in
    let local_free =
      StringSet.union guard_free (StringSet.diff body_free final_bound)
    in
    StringSet.union acc local_free
  and scan_guards initial_bound guards =
    List.fold_left
      (fun (bound, acc) g ->
        match g with
        | GParam p -> (StringSet.add (Ast.lower_name p.param_name) bound, acc)
        | GIn (Lower n, list_expr) ->
            (* List expression is evaluated in the OUTER scope: its free
               vars are filtered only by names bound BEFORE this guard. *)
            let list_free =
              StringSet.diff (go StringSet.empty list_expr) bound
            in
            (StringSet.add n bound, StringSet.union acc list_free)
        | GExpr e ->
            let e_free = StringSet.diff (go StringSet.empty e) bound in
            (bound, StringSet.union acc e_free))
      (initial_bound, StringSet.empty)
      guards
  in
  go StringSet.empty e

(** Set of [lower_ident] names bound by [params]. Used by callers of
    [bind_head_params] to exclude shadowed names — e.g. action params whose
    names happen to collide with a rule param in the same chapter head. *)
let param_name_set (params : param list) =
  List.fold_left
    (fun acc (p : param) -> StringSet.add (Ast.lower_name p.param_name) acc)
    StringSet.empty params

(** Wrap a proposition in a universal quantifier over the head bindings that
    actually appear free in the proposition. Deduplicates by parameter name
    (first declaration wins) so that chapters declaring multiple rules with a
    shared parameter name don't introduce duplicate quantifier binders. Always
    wraps when at least one binding survives the filter, even if the proposition
    is itself quantified — inner quantifiers may still reference head-level
    variables.

    [?exclude] names are skipped even if they appear free. Callers use this for
    action params: their names are declared as SMT constants, and if one happens
    to coincide with a head-rule-param name the free occurrence refers to the
    action constant — quantifying it would silently strengthen the proposition
    by shadowing the constant. *)
let bind_head_params ?(exclude = StringSet.empty) (bindings : param list)
    (p : expr located) =
  match bindings with
  | [] -> p
  | _ -> (
      let free = free_vars p.value in
      let _, kept_rev =
        List.fold_left
          (fun (seen, acc) (param : param) ->
            let name = Ast.lower_name param.param_name in
            if StringSet.mem name seen then (seen, acc)
            else if StringSet.mem name exclude then (seen, acc)
            else if not (StringSet.mem name free) then (seen, acc)
            else (StringSet.add name seen, param :: acc))
          (StringSet.empty, []) bindings
      in
      let kept = List.rev kept_rev in
      match kept with
      | [] -> p
      | _ -> { p with value = Ast.make_forall kept [] p.value })

(** Collect all invariants from the document (non-initially propositions) *)
let collect_invariants chapters =
  List.concat_map
    (fun c ->
      match c with
      | Invariant { head_bindings; propositions; _ } ->
          List.filter_map
            (fun (p : expr located) ->
              match p.value with
              | EInitially _ -> None
              | EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _
              | ELitString _ | ELitBool _ | EApp _ | EPrimed _ | EOverride _
              | ETuple _ | EProj _ | EBinop _ | EUnop _ | EForall _ | EExists _
              | EEach _ | ECond _ ->
                  Some (bind_head_params head_bindings p))
            propositions
      | Action _ -> [])
    chapters

(** Collect all initial-state propositions, stripping the EInitially wrapper *)
let collect_initial_props chapters =
  List.concat_map
    (fun c ->
      match c with
      | Invariant { head_bindings; propositions; _ } ->
          List.filter_map
            (fun (p : expr located) ->
              match p.value with
              | EInitially e ->
                  Some (bind_head_params head_bindings { p with value = e })
              | EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _
              | ELitString _ | ELitBool _ | EApp _ | EPrimed _ | EOverride _
              | ETuple _ | EProj _ | EBinop _ | EUnop _ | EForall _ | EExists _
              | EEach _ | ECond _ ->
                  None)
            propositions
      | Action _ -> [])
    chapters

type action_info = {
  a_label : string;
  a_params : param list;
  a_guards : guard list;
  a_contexts : string list;
  a_propositions : expr located list;
}

(** Collect all actions from the document. Applies [bind_head_params] to each
    proposition so that references to rule parameters declared in the same
    chapter head (which are in type-check scope per REFERENCE.md but are not
    SMT-declared — only action params are) get wrapped in a universal quantifier
    over the rule param. Without this, a proposition like
    [count' a1 = count a1.] — where [a1] is [count]'s declared param — fails at
    SMT time with "unknown constant a1". Mirrors the invariant treatment in
    [collect_invariants]. *)
let collect_actions chapters =
  List.filter_map
    (fun c ->
      match c with
      | Action
          { label; params; guards; contexts; head_bindings; propositions; _ } ->
          let exclude = param_name_set params in
          let bind_action = bind_head_params ~exclude head_bindings in
          Some
            {
              a_label = label;
              a_params = params;
              a_guards = guards;
              a_contexts = contexts;
              a_propositions = List.map bind_action propositions;
            }
      | Invariant _ -> None)
    chapters

(** Check context: either from an invariant or action chapter *)
type check_context =
  | CheckInvariant of expr located list
  | CheckAction of action_info

(** Collect all check (entailment goal) propositions, paired with their chapter
    context *)
let collect_checks chapters =
  List.concat_map
    (fun c ->
      match c with
      | Invariant { head_bindings; propositions; checks } ->
          let bound_props =
            List.map (bind_head_params head_bindings) propositions
          in
          let bound_checks = List.map (bind_head_params head_bindings) checks in
          List.map (fun chk -> (chk, CheckInvariant bound_props)) bound_checks
      | Action
          {
            label;
            params;
            guards;
            contexts;
            head_bindings;
            propositions;
            checks;
          } ->
          let exclude = param_name_set params in
          let bind_action = bind_head_params ~exclude head_bindings in
          let bound_props = List.map bind_action propositions in
          let bound_checks = List.map bind_action checks in
          let action =
            {
              a_label = label;
              a_params = params;
              a_guards = guards;
              a_contexts = contexts;
              a_propositions = bound_props;
            }
          in
          List.map (fun chk -> (chk, CheckAction action)) bound_checks)
    chapters

(** Generate frame condition expressions: for every rule NOT in the action's
    context, produce the SMT expression asserting f_prime = f. Returns a list of
    (function_name, smt_expr) pairs. *)
let collect_frame_exprs _config env contexts =
  match contexts with
  | [] -> []
  | _ ->
      let all_members =
        List.concat_map
          (fun ctx_name ->
            match Env.lookup_context ctx_name env with
            | Some members -> members
            | None -> [])
          contexts
      in
      Env.fold_terms
        (fun name entry acc ->
          match entry.Env.kind with
          | Env.KRule ty when not (List.mem name all_members) -> (
              match decompose_func_ty ty with
              | Some ([], _ret) ->
                  let sname = smt_rule_name env name 0 in
                  (name, Printf.sprintf "(= %s_prime %s)" sname sname) :: acc
              | Some (params, _ret) ->
                  let sname = smt_rule_name env name (List.length params) in
                  let param_sorts = List.map sort_of_ty params in
                  let param_names =
                    List.mapi (fun i _ -> Printf.sprintf "frame_x_%d" i) params
                  in
                  let bindings =
                    List.map2
                      (fun n s -> Printf.sprintf "(%s %s)" n s)
                      param_names param_sorts
                  in
                  let args = String.concat " " param_names in
                  ( name,
                    Printf.sprintf "(forall (%s) (= (%s_prime %s) (%s %s)))"
                      (String.concat " " bindings)
                      sname args sname args )
                  :: acc
              | None -> acc)
          | Env.KRule _ | Env.KDomain | Env.KAlias _ | Env.KVar _
          | Env.KClosure _ ->
              acc)
        env []
      |> List.rev

(** Generate frame conditions as plain assertions (for invariant queries) *)
let generate_frame_conditions config env context =
  let exprs = collect_frame_exprs config env context in
  match exprs with
  | [] -> ""
  | _ ->
      let buf = Buffer.create 256 in
      Buffer.add_string buf "; --- Frame conditions ---\n";
      List.iter
        (fun (_name, smt_expr) ->
          Buffer.add_string buf (Printf.sprintf "(assert %s)\n" smt_expr))
        exprs;
      Buffer.contents buf

(** Extend the type environment with action parameter bindings so that
    expressions referencing action params can be type-inferred during SMT
    translation (e.g., membership guards like [x in f param]). *)
let env_with_action_params env (params : param list) =
  Env.with_vars (resolve_param_bindings env params) env

(** Generate parameter declarations for an action *)
let declare_action_params env (params : param list) =
  let buf = Buffer.create 128 in
  List.iter
    (fun (p : param) ->
      let sort =
        match resolve_param_sort env p.param_type with
        | Some s -> s
        | None ->
            failwith
              (Printf.sprintf
                 "SMT translation: cannot resolve sort for action parameter \
                  '%s'"
                 (Ast.lower_name p.param_name))
      in
      Buffer.add_string buf
        (Printf.sprintf "(declare-const %s %s)\n"
           (sanitize_ident (Ast.lower_name p.param_name))
           sort))
    params;
  Buffer.contents buf

(** Generate parameter type constraints (Nat >= 1, Nat0 >= 0) *)
let declare_param_constraints env (params : param list) =
  let buf = Buffer.create 128 in
  List.iter
    (fun (p : param) ->
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok TyNat ->
          Buffer.add_string buf
            (Printf.sprintf "(assert (>= %s 1))\n"
               (sanitize_ident (Ast.lower_name p.param_name)))
      | Ok TyNat0 ->
          Buffer.add_string buf
            (Printf.sprintf "(assert (>= %s 0))\n"
               (sanitize_ident (Ast.lower_name p.param_name)))
      | Ok
          ( TyBool | TyInt | TyReal | TyString | TyNothing | TyDomain _
          | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
      | Error _ ->
          ())
    params;
  Buffer.contents buf

(** Extract preconditions from action guards (non-binding boolean conditions).
    Returns raw AST exprs. *)
let extract_precondition_exprs (guards : guard list) =
  List.filter_map
    (fun g -> match g with GExpr e -> Some e | GIn _ | GParam _ -> None)
    guards

(** Check if an action has no boolean preconditions (always enabled) *)
let action_always_enabled action =
  extract_precondition_exprs action.a_guards = []

(** Collect all function names referenced in an expression *)
let rec collect_function_refs (e : expr) =
  match e with
  | EVar (Lower name) -> [ name ]
  | EApp (func, args) ->
      collect_function_refs func @ List.concat_map collect_function_refs args
  | EPrimed (Lower name) -> [ name ]
  | EBinop (_, e1, e2) -> collect_function_refs e1 @ collect_function_refs e2
  | EUnop (_, e) -> collect_function_refs e
  | EForall (mb, metas) | EExists (mb, metas) ->
      collect_function_refs_q mb metas
  | EEach (mb, metas, _) -> collect_function_refs_q mb metas
  | ETuple es -> List.concat_map collect_function_refs es
  | EProj (e, _) -> collect_function_refs e
  | EOverride (Lower name, pairs) ->
      name
      :: List.concat_map
           (fun (k, v) -> collect_function_refs k @ collect_function_refs v)
           pairs
  | ECond arms ->
      List.concat_map
        (fun (arm, cons) ->
          collect_function_refs arm @ collect_function_refs cons)
        arms
  | EInitially e -> collect_function_refs e
  | ELitBool _ | ELitNat _ | ELitReal _ | ELitString _ | EDomain _
  | EQualified _ ->
      []

and collect_guard_refs = function
  | GParam _ -> []
  | GIn (_, e) -> collect_function_refs e
  | GExpr e -> collect_function_refs e

and collect_function_refs_q mb metas =
  let _, gs, body = Ast.unbind_quant mb metas in
  List.concat_map collect_guard_refs gs @ collect_function_refs body

(** Check if invariant propositions touch any contextual functions. If they only
    reference extracontextual functions, the frame conditions guarantee
    preservation and we can skip the check. *)
let invariant_touches_context env contexts invariant_props =
  match contexts with
  | [] -> true (* No context = check everything *)
  | _ ->
      let all_members =
        List.concat_map
          (fun ctx_name ->
            match Env.lookup_context ctx_name env with
            | Some members -> members
            | None -> [])
          contexts
      in
      let refs =
        List.concat_map
          (fun (p : expr located) -> collect_function_refs p.value)
          invariant_props
      in
      List.exists (fun r -> List.mem r all_members) refs
