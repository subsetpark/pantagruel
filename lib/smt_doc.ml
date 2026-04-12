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
  match action with
  | Some (label, params, guards, contexts) ->
      Action
        {
          label;
          params;
          guards;
          contexts;
          propositions = chapter.body;
          checks = chapter.checks;
        }
  | None ->
      let head_bindings =
        List.concat_map
          (fun (decl : declaration located) ->
            match decl.value with
            | DeclRule { params; _ } -> params
            | DeclDomain _ | DeclAlias _ | DeclAction _ | DeclClosure _ -> [])
          chapter.head
      in
      Invariant
        { head_bindings; propositions = chapter.body; checks = chapter.checks }

let classify_chapters (doc : document) = List.map classify_chapter doc.chapters

(** Wrap a proposition in a universal quantifier over the given head bindings.
    Always wraps, even if the proposition is already quantified, because inner
    quantifiers may still reference head-level variables. *)
let bind_head_params (bindings : param list) (p : expr located) =
  match bindings with
  | [] -> p
  | _ -> { p with value = EForall (bindings, [], p.value) }

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

(** Collect all actions from the document *)
let collect_actions chapters =
  List.filter_map
    (fun c ->
      match c with
      | Action { label; params; guards; contexts; propositions; _ } ->
          Some
            {
              a_label = label;
              a_params = params;
              a_guards = guards;
              a_contexts = contexts;
              a_propositions = propositions;
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
      | Action { label; params; guards; contexts; propositions; checks } ->
          let action =
            {
              a_label = label;
              a_params = params;
              a_guards = guards;
              a_contexts = contexts;
              a_propositions = propositions;
            }
          in
          List.map (fun chk -> (chk, CheckAction action)) checks)
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
              let sname = sanitize_ident name in
              match decompose_func_ty ty with
              | Some ([], _ret) ->
                  (name, Printf.sprintf "(= %s_prime %s)" sname sname) :: acc
              | Some (params, _ret) ->
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
  | EForall (_, gs, body) | EExists (_, gs, body) | EEach (_, gs, _, body) ->
      List.concat_map collect_guard_refs gs @ collect_function_refs body
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
