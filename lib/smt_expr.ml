(** SMT expression transformation utilities: substitution, priming, guard
    collection, and comprehension expansion.

    Capture-avoiding substitution and priming on quantifier binders are
    implemented via the [Binder.Mbinder] primitives — the hand-rolled walkers
    that used to filter substitution domains by bound names have collapsed to
    unbind / recurse / rebind. *)

open Ast
open Types
open Smt_types
open Smt_preamble

(** Resolve the iteration variable, domain name, and any implicit guard for a
    comprehension. Supports two forms:
    - Typed binding: [all x: D, guards | body] — params = [x:D], iterates over D
    - Membership binding: [all x in xs, guards | body] — params = [], guards
      starts with GIn(x, xs), iterates over element domain of xs with implicit
      (select xs elem) guard *)
let resolve_comprehension_binding env params guards =
  match params with
  | [ (p : param) ] -> (
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok (TyDomain dname) ->
          let pn = Ast.lower_name p.param_name in
          Ok (pn, dname, None, [ (pn, TyDomain dname) ])
      | Ok ((TyNat | TyNat0 | TyInt) as ty) ->
          Error
            (Printf.sprintf
               "SMT translation: comprehension over unbounded %s is only \
                supported as μ-search (`min over each j: %s, … | j`); add an \
                explicit upper-bound guard (e.g. `j < N`) to enable general \
                enumeration"
               (format_ty ty) (format_ty ty))
      | Ok TyReal ->
          Error
            "SMT translation: comprehension over unbounded Real is not \
             supported in SMT; μ-search requires a discrete well-ordered \
             numeric type"
      | Ok
          ( TyBool | TyString | TyNothing | TyList _ | TyProduct _ | TySum _
          | TyFunc _ )
      | Error _ ->
          Error "SMT translation: comprehension parameter must be a domain type"
      )
  | [] -> (
      (* Look for a leading GIn guard *)
      match guards with
      | GIn (Lower name, list_expr) :: _rest -> (
          match[@warning "-4"]
            Check.infer_type { Check.env; loc = dummy_loc } list_expr
          with
          | Ok (TyList (TyDomain dname)) ->
              Ok (name, dname, Some list_expr, [ (name, TyDomain dname) ])
          | _ ->
              Error
                "SMT translation: membership comprehension requires a domain \
                 list")
      | GExpr _ :: _ | GParam _ :: _ | [] ->
          Error
            "SMT translation: comprehension requires a typed or membership \
             binding")
  | _ ->
      Error
        "SMT translation: multi-parameter comprehensions not supported in SMT"

(** Resolve a single-parameter numeric-typed comprehension binder (Nat / Nat0 /
    Int). Unlike [resolve_comprehension_binding], does not synthesize a finite
    domain — it is used by symbolic-predicate translations (currently just
    μ-search minimization) that don't enumerate over a bounded set. Returns
    [Error] for domain / list / membership bindings and multi-parameter
    comprehensions so callers can fall back to the enumerating path. *)
let resolve_numeric_comprehension_binding env params =
  match params with
  | [ (p : param) ] -> (
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok ((TyNat | TyNat0 | TyInt) as ty) ->
          let pn = Ast.lower_name p.param_name in
          Ok (pn, ty, [ (pn, ty) ])
      | Ok
          ( TyBool | TyReal | TyString | TyNothing | TyDomain _ | TyList _
          | TyProduct _ | TySum _ | TyFunc _ )
      | Error _ ->
          Error "comprehension binder is not numeric")
  | _ -> Error "comprehension binder is not numeric"

(** Expand a comprehension over finite domain elements. Returns a list of
    (guard_str option, value_str) pairs for each domain element substitution.
    [translate] is the expression translator (passed to break mutual recursion).
*)
let expand_comprehension translate config env params guards body =
  match resolve_comprehension_binding env params guards with
  | Error msg -> failwith msg
  | Ok (var_name, dname, membership_expr, bindings) ->
      let env_inner = Env.with_vars bindings env in
      let elems = domain_elements dname (bound_for config dname) in
      let pname = sanitize_ident var_name in
      (* Translate body and guards as templates, then substitute *)
      let body_template = translate config env_inner body in
      (* Collect explicit guard conditions (GExpr only; skip GIn/GParam) *)
      let guard_templates =
        List.filter_map
          (fun g ->
            match g with
            | GExpr e -> Some (translate config env_inner e)
            | GIn _ | GParam _ -> None)
          guards
      in
      (* For GIn bindings, add implicit membership guard *)
      let membership_template =
        match membership_expr with
        | Some list_e ->
            let list_str = translate config env list_e in
            Some (Printf.sprintf "(select %s %s)" list_str pname)
        | None -> None
      in
      List.map
        (fun elem ->
          let sub s = replace_word ~from:pname ~to_:elem s in
          let value_str = sub body_template in
          let all_guards =
            (match membership_template with Some t -> [ sub t ] | None -> [])
            @ List.map sub guard_templates
          in
          let guard_str =
            match all_guards with
            | [] -> None
            | [ g ] -> Some g
            | gs -> Some (Printf.sprintf "(and %s)" (String.concat " " gs))
          in
          (guard_str, value_str))
        elems

(** Capture-avoiding substitution: replace EVar names according to the mapping.
    When the substitution's range contains a free name that matches a quantifier
    binder along the way, the binder is alpha-renamed to a fresh name before
    substituting so the introduced occurrence stays free. The rename cascades
    through nested binders that happen to share the fresh name, preserving
    standard capture-avoidance semantics. *)
let rec substitute_vars (subst : (string * expr) list) (e : expr) : expr =
  match e with
  | EVar (Lower name) -> (
      match List.assoc_opt name subst with Some e' -> e' | None -> e)
  | EApp (func, args) ->
      EApp (substitute_vars subst func, List.map (substitute_vars subst) args)
  | EBinop (op, e1, e2) ->
      EBinop (op, substitute_vars subst e1, substitute_vars subst e2)
  | EUnop (op, e1) -> EUnop (op, substitute_vars subst e1)
  | ETuple es -> ETuple (List.map (substitute_vars subst) es)
  | EProj (e1, i) -> EProj (substitute_vars subst e1, i)
  | EOverride (Lower name, pairs) ->
      let name_expr =
        match[@warning "-4"] List.assoc_opt name subst with
        | Some (EVar n) -> n
        | _ -> Lower name
      in
      EOverride
        ( name_expr,
          List.map
            (fun (k, v) -> (substitute_vars subst k, substitute_vars subst v))
            pairs )
  | EForall (mb, metas) ->
      let params, gs, body =
        substitute_quant_children subst (Ast.unbind_quant mb metas)
      in
      Ast.make_forall params gs body
  | EExists (mb, metas) ->
      let params, gs, body =
        substitute_quant_children subst (Ast.unbind_quant mb metas)
      in
      Ast.make_exists params gs body
  | EEach (mb, metas, comb) ->
      let params, gs, body =
        substitute_quant_children subst (Ast.unbind_quant mb metas)
      in
      Ast.make_each params gs comb body
  | ECond arms ->
      ECond
        (List.map
           (fun (arm, cons) ->
             (substitute_vars subst arm, substitute_vars subst cons))
           arms)
  | EInitially e1 -> EInitially (substitute_vars subst e1)
  | EPrimed _ | ELitBool _ | ELitNat _ | ELitReal _ | ELitString _ | EDomain _
  | EQualified _ ->
      e

(** Rewrite a quantifier's unbound triple under [subst], alpha-renaming any
    binder — top-level [params] or the [GParam] / [GIn] binders introduced
    sequentially by the guard list — whose name would otherwise capture a free
    occurrence in the substitution's range. Returns the rewritten
    [(params, guards, body)] ready to feed into [Ast.make_forall] /
    [make_exists] / [make_each]. *)
and substitute_quant_children (subst : (string * expr) list)
    ((params, gs, body) : param list * guard list * expr) :
    param list * guard list * expr =
  (* Seed a "used" set with every name already in sight: subst-range free
     vars, body and guard free vars, binder-bound names. Fresh-name
     generation updates this ref as it picks names, so later renames at this
     level see earlier ones. *)
  let subst_free =
    List.fold_left
      (fun acc (_, rep) -> Smt_doc.StringSet.union acc (Smt_doc.free_vars rep))
      Smt_doc.StringSet.empty subst
  in
  let body_free = Smt_doc.free_vars body in
  let guard_free =
    List.fold_left
      (fun acc g ->
        match g with
        | GIn (_, e) | GExpr e ->
            Smt_doc.StringSet.union acc (Smt_doc.free_vars e)
        | GParam _ -> acc)
      Smt_doc.StringSet.empty gs
  in
  let guard_bound =
    List.fold_left
      (fun acc g ->
        match g with
        | GParam p -> Smt_doc.StringSet.add (Ast.lower_name p.param_name) acc
        | GIn (Lower n, _) -> Smt_doc.StringSet.add n acc
        | GExpr _ -> acc)
      Smt_doc.StringSet.empty gs
  in
  let params_set =
    List.fold_left
      (fun acc (p : param) ->
        Smt_doc.StringSet.add (Ast.lower_name p.param_name) acc)
      Smt_doc.StringSet.empty params
  in
  let used =
    ref
      (List.fold_left Smt_doc.StringSet.union Smt_doc.StringSet.empty
         [ subst_free; body_free; guard_free; guard_bound; params_set ])
  in
  let fresh_for base =
    let rec go i =
      let candidate = Printf.sprintf "%s_%d" base i in
      if Smt_doc.StringSet.mem candidate !used then go (i + 1)
      else begin
        used := Smt_doc.StringSet.add candidate !used;
        candidate
      end
    in
    go 1
  in
  (* A binder's name conflicts when it appears free in any remaining subst
     value — substituting through the binder's scope would otherwise capture
     the introduced occurrence. *)
  let name_conflicts (subst : (string * expr) list) (name : string) : bool =
    List.exists
      (fun (_, rep) -> Smt_doc.StringSet.mem name (Smt_doc.free_vars rep))
      subst
  in
  (* Process a single binder name, returning the (possibly renamed) name and
     the updated substitution for its scope. A renamed binder adds its
     [old -> fresh] rename to subst so later references in guards and body
     pick up the new name. *)
  let process_binder (subst : (string * expr) list) (name : string) :
      string * (string * expr) list =
    if name_conflicts subst name then
      let new_name = fresh_for name in
      let subst' = List.filter (fun (k, _) -> k <> new_name) subst in
      (new_name, (name, EVar (Lower new_name)) :: subst')
    else (name, List.filter (fun (k, _) -> k <> name) subst)
  in
  let params_rev, subst_after_params =
    List.fold_left
      (fun (acc, s) (p : param) ->
        let new_name, s' = process_binder s (Ast.lower_name p.param_name) in
        ({ p with param_name = Lower new_name } :: acc, s'))
      ([], subst) params
  in
  let new_params = List.rev params_rev in
  let gs_rev, subst_after_gs =
    List.fold_left
      (fun (acc, s) g ->
        match g with
        | GParam p ->
            let new_name, s' = process_binder s (Ast.lower_name p.param_name) in
            (GParam { p with param_name = Lower new_name } :: acc, s')
        | GIn (Lower name, e) ->
            (* The list expression is evaluated in the *outer* scope — before
               this binder shadows its name — so substitute in [e] under [s]
               before processing the binder. *)
            let e' = substitute_vars s e in
            let new_name, s' = process_binder s name in
            (GIn (Lower new_name, e') :: acc, s')
        | GExpr e -> (GExpr (substitute_vars s e) :: acc, s))
      ([], subst_after_params) gs
  in
  let new_gs = List.rev gs_rev in
  let new_body = substitute_vars subst_after_gs body in
  (new_params, new_gs, new_body)

(** Fold a substitution through a guard list, shadowing the domain as each
    [GParam] / [GIn] binder comes into scope. Used by the rest of the pipeline
    (e.g. [prime_guards] tracks its own [bound]); [substitute_quant_children]
    does not call this — it handles shadowing and renaming in one pass. *)
and substitute_guards subst gs =
  List.fold_left
    (fun (subst, acc) g ->
      match g with
      | GParam p ->
          let subst' =
            List.filter (fun (n, _) -> n <> Ast.lower_name p.param_name) subst
          in
          (subst', acc @ [ GParam p ])
      | GIn (Lower name, e) ->
          let subst' = List.filter (fun (n, _) -> n <> name) subst in
          (subst', acc @ [ GIn (Lower name, substitute_vars subst e) ])
      | GExpr e -> (subst, acc @ [ GExpr (substitute_vars subst e) ]))
    (subst, []) gs

(** Substitute primed names in an expression (for invariant checking in next
    state). Capture avoidance for quantifier binders is delegated to
    [substitute_quant_children] above; the [?bound] parameter just tracks which
    names refer to a binder introduced by an enclosing quantifier so they don't
    get primed. *)
let rec prime_expr ?(bound = []) (e : expr) : expr =
  match e with
  | EVar (Lower name) -> if List.mem name bound then e else EPrimed (Lower name)
  | EApp (func, args) ->
      EApp (prime_expr ~bound func, List.map (prime_expr ~bound) args)
  | EBinop (op, e1, e2) ->
      EBinop (op, prime_expr ~bound e1, prime_expr ~bound e2)
  | EUnop (op, e) -> EUnop (op, prime_expr ~bound e)
  | ETuple es -> ETuple (List.map (prime_expr ~bound) es)
  | EProj (e, i) -> EProj (prime_expr ~bound e, i)
  | EForall (mb, metas) ->
      let params, gs, body = Ast.unbind_quant mb metas in
      let bound' =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) params @ bound
      in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      Ast.make_forall params gs' (prime_expr ~bound:bound'' body)
  | EExists (mb, metas) ->
      let params, gs, body = Ast.unbind_quant mb metas in
      let bound' =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) params @ bound
      in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      Ast.make_exists params gs' (prime_expr ~bound:bound'' body)
  | EEach (mb, metas, comb) ->
      let params, gs, body = Ast.unbind_quant mb metas in
      let bound' =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) params @ bound
      in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      Ast.make_each params gs' comb (prime_expr ~bound:bound'' body)
  | ECond arms ->
      ECond
        (List.map
           (fun (arm, cons) -> (prime_expr ~bound arm, prime_expr ~bound cons))
           arms)
  | EOverride (name, pairs) ->
      EOverride
        ( name,
          List.map
            (fun (k, v) -> (prime_expr ~bound k, prime_expr ~bound v))
            pairs )
  | EInitially e -> EInitially (prime_expr ~bound e)
  | EPrimed _ | ELitBool _ | ELitNat _ | ELitReal _ | ELitString _ | EDomain _
  | EQualified _ ->
      e

(** Reverse priming: convert EPrimed back to EVar for type inference. *)
and unprime_expr (e : expr) : expr =
  match e with
  | EPrimed name -> EVar name
  | EApp (func, args) -> EApp (unprime_expr func, List.map unprime_expr args)
  | EBinop (op, e1, e2) -> EBinop (op, unprime_expr e1, unprime_expr e2)
  | EUnop (op, e) -> EUnop (op, unprime_expr e)
  | EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
  | ELitBool _ | ETuple _ | EProj _ | EOverride _ | EForall _ | EExists _
  | EEach _ | ECond _ | EInitially _ ->
      e

and prime_guards ~bound gs =
  List.fold_left
    (fun (bound, acc) g ->
      match g with
      | GParam p -> (Ast.lower_name p.param_name :: bound, acc @ [ GParam p ])
      | GIn (Lower name, e) ->
          (name :: bound, acc @ [ GIn (Lower name, prime_expr ~bound e) ])
      | GExpr e -> (bound, acc @ [ GExpr (prime_expr ~bound e) ]))
    (bound, []) gs

(** Collect guard expressions from applications of guarded functions in an
    expression. Stops at nested quantifiers (they handle their own guards).
    Returns a list of AST guard expressions with actual args substituted.
    [~bound] tracks quantifier-bound variable names so that [prime_expr] inside
    primed-application handling doesn't incorrectly prime them. *)
let collect_body_guards ?(bound = []) env (e : expr) : expr list =
  let guards = ref [] in
  let rec walk = function
    | EApp (func, args) ->
        (* List-search guard: xs x where xs : [T] and x : T injects (x in xs).
           Primed terms fail Check.infer_type outside action-context, so we
           retry against the unprimed form. *)
        let infer_with_unprime e =
          match Check.infer_type { Check.env; loc = Ast.dummy_loc } e with
          | Ok ty -> Some ty
          | Error _ -> (
              match
                Check.infer_type
                  { Check.env; loc = Ast.dummy_loc }
                  (unprime_expr e)
              with
              | Ok ty -> Some ty
              | Error _ -> None)
        in
        (match args with
        | [ arg ] -> (
            match[@warning "-4"] infer_with_unprime func with
            | Some (TyList elem_ty) -> (
                match[@warning "-4"] infer_with_unprime arg with
                | Some arg_ty
                  when is_subtype arg_ty elem_ty
                       && (not (is_subtype arg_ty TyNat))
                       && not (is_numeric elem_ty) ->
                    guards := EBinop (OpIn, arg, func) :: !guards
                | _ -> ())
            | _ -> ())
        | _ -> ());
        (* A guarded nullary rule returning a list can appear here in
           list-search form (e.g. [colors red] where [colors] has no formals
           but [red] is the search key). Its declaration guards reference
           only its own formals — which are zero — so no substitution is
           needed; skip [List.combine] when the lengths disagree. *)
        let subst_for formal_params =
          let formal_names =
            List.map
              (fun (p : param) -> Ast.lower_name p.param_name)
              formal_params
          in
          if List.length formal_names = List.length args then
            List.combine formal_names args
          else []
        in
        let n_args = List.length args in
        (* Guard lookup falls back to arity-0 when arity-n_args has no
           guards — this handles list-search on a guarded nullary list-
           returning rule (e.g. [colors red]: colors is arity-0 but the
           application has 1 arg; its declaration guards live under the
           nullary key). subst_for already zeros out the substitution
           when formal_params and args lengths disagree. *)
        let lookup_guards name =
          match Env.lookup_rule_guards_arity name n_args env with
          | Some _ as r -> r
          | None -> Env.lookup_rule_guards_arity name 0 env
        in
        (match[@warning "-4"] func with
        | EVar (Lower name) -> (
            match lookup_guards name with
            | Some (formal_params, rule_guards) ->
                let subst = subst_for formal_params in
                List.iter
                  (fun (g : guard) ->
                    match g with
                    | GExpr ge ->
                        let ge = substitute_vars subst ge in
                        walk ge;
                        guards := ge :: !guards
                    | GIn _ | GParam _ -> ())
                  rule_guards
            | None -> ())
        | EPrimed (Lower name) -> (
            (* Primed application: collect guards in primed form *)
            match lookup_guards name with
            | Some (formal_params, rule_guards) ->
                let subst = subst_for formal_params in
                List.iter
                  (fun (g : guard) ->
                    match g with
                    | GExpr ge ->
                        let ge = prime_expr ~bound (substitute_vars subst ge) in
                        walk ge;
                        guards := ge :: !guards
                    | GIn _ | GParam _ -> ())
                  rule_guards
            | None -> ())
        | _ -> walk func);
        List.iter walk args
    | EVar (Lower name) -> (
        match(* Nullary auto-applied rule with guards *)
             [@warning "-4"]
          Env.lookup_term_arity name 0 env
        with
        | Some { kind = Env.KRule (TyFunc ([], Some _)); _ } -> (
            match Env.lookup_rule_guards_arity name 0 env with
            | Some (_, rule_guards) ->
                List.iter
                  (fun (g : guard) ->
                    match g with
                    | GExpr ge -> guards := ge :: !guards
                    | GIn _ | GParam _ -> ())
                  rule_guards
            | None -> ())
        | _ -> ())
    | EPrimed (Lower name) -> (
        match(* Nullary auto-applied primed rule with guards *)
             [@warning "-4"]
          Env.lookup_term_arity name 0 env
        with
        | Some { kind = Env.KRule (TyFunc ([], Some _)); _ } -> (
            match Env.lookup_rule_guards_arity name 0 env with
            | Some (_, rule_guards) ->
                List.iter
                  (fun (g : guard) ->
                    match g with
                    | GExpr ge -> guards := prime_expr ~bound ge :: !guards
                    | GIn _ | GParam _ -> ())
                  rule_guards
            | None -> ())
        | _ -> ())
    | EBinop (_, e1, e2) ->
        walk e1;
        walk e2
    | EUnop (_, e1) -> walk e1
    | ETuple es -> List.iter walk es
    | EProj (e1, _) -> walk e1
    | EOverride (_, pairs) ->
        List.iter
          (fun (k, v) ->
            walk k;
            walk v)
          pairs
    | EInitially e1 -> walk e1
    | ECond arms ->
        List.iter
          (fun (arm, cons) ->
            walk arm;
            walk cons)
          arms
    (* Stop at nested quantifiers — they inject their own guards *)
    | EForall _ | EExists _ | EEach _ -> ()
    | ELitBool _ | ELitNat _ | ELitReal _ | ELitString _ | EDomain _
    | EQualified _ ->
        ()
  in
  walk e;
  (* Deduplicate *)
  List.sort_uniq compare (List.rev !guards)
