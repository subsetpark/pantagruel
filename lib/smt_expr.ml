(** SMT expression transformation utilities: substitution, priming, guard
    collection, and comprehension expansion *)

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
      | Ok
          ( TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing
          | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
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
    Stops at quantifier/comprehension boundaries to avoid capturing bound vars.
*)
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
  | EForall (ps, gs, body) ->
      let bound =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) ps
      in
      let subst' = List.filter (fun (n, _) -> not (List.mem n bound)) subst in
      let subst'', gs' = substitute_guards subst' gs in
      EForall (ps, gs', substitute_vars subst'' body)
  | EExists (ps, gs, body) ->
      let bound =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) ps
      in
      let subst' = List.filter (fun (n, _) -> not (List.mem n bound)) subst in
      let subst'', gs' = substitute_guards subst' gs in
      EExists (ps, gs', substitute_vars subst'' body)
  | EEach (ps, gs, comb, body) ->
      let bound =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) ps
      in
      let subst' = List.filter (fun (n, _) -> not (List.mem n bound)) subst in
      let subst'', gs' = substitute_guards subst' gs in
      EEach (ps, gs', comb, substitute_vars subst'' body)
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
    state). Tracks locally-bound names (from quantifiers) to avoid priming them.
*)
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
  | EForall (ps, gs, body) ->
      let bound' =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) ps @ bound
      in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      EForall (ps, gs', prime_expr ~bound:bound'' body)
  | EExists (ps, gs, body) ->
      let bound' =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) ps @ bound
      in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      EExists (ps, gs', prime_expr ~bound:bound'' body)
  | EEach (ps, gs, comb, body) ->
      let bound' =
        List.map (fun (p : param) -> Ast.lower_name p.param_name) ps @ bound
      in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      EEach (ps, gs', comb, prime_expr ~bound:bound'' body)
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
    | EApp (EVar (Lower name), args) ->
        (match Env.lookup_rule_guards name env with
        | Some (formal_params, rule_guards) ->
            let subst =
              List.combine
                (List.map
                   (fun (p : param) -> Ast.lower_name p.param_name)
                   formal_params)
                args
            in
            List.iter
              (fun (g : guard) ->
                match g with
                | GExpr ge -> guards := substitute_vars subst ge :: !guards
                | GIn _ | GParam _ -> ())
              rule_guards
        | None -> ());
        List.iter walk args
    | EApp (EPrimed (Lower name), args) ->
        (* Primed application: collect guards in primed form *)
        (match Env.lookup_rule_guards name env with
        | Some (formal_params, rule_guards) ->
            let subst =
              List.combine
                (List.map
                   (fun (p : param) -> Ast.lower_name p.param_name)
                   formal_params)
                args
            in
            List.iter
              (fun (g : guard) ->
                match g with
                | GExpr ge ->
                    guards :=
                      prime_expr ~bound (substitute_vars subst ge) :: !guards
                | GIn _ | GParam _ -> ())
              rule_guards
        | None -> ());
        List.iter walk args
    | EApp (func, args) ->
        walk func;
        List.iter walk args
    | EVar (Lower name) -> (
        match(* Nullary auto-applied rule with guards *)
             [@warning "-4"]
          Env.lookup_term name env
        with
        | Some { kind = Env.KRule (TyFunc ([], Some _)); _ } -> (
            match Env.lookup_rule_guards name env with
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
          Env.lookup_term name env
        with
        | Some { kind = Env.KRule (TyFunc ([], Some _)); _ } -> (
            match Env.lookup_rule_guards name env with
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
