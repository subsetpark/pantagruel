(** SMT-LIB2 translation for bounded model checking *)

open Ast
open Types

(* Re-export all submodules for backward compatibility — external code uses
   Smt.config, Smt.query, Smt.make_config, Smt.declare_domain_sorts, etc. *)
include Smt_types
include Smt_preamble
include Smt_doc
include Smt_expr

(** Alpha-rename quantifier binders whose string names collide with a declared
    rule / closure symbol at the SMT top level.

    This is a *name-collision* pass, not a capture-avoidance pass: Bindlib's
    mbinder already keeps binder identity free of capture in the AST. But
    [Ast.lower_name] returns the user-chosen string unchanged, and SMT's scope
    rules shadow the top-level declaration within the quantifier body. So an
    emission like [(forall ((name ...)) (name x))] — with [name] a declared rule
    — would read [name x] as applying the bound variable, not the rule.

    The rewrite itself delegates to [Smt_expr.substitute_vars] (library-backed
    via Bindlib); no hand-rolled walker. We just compute fresh strings that
    don't collide with sibling binders, outer quantifier binders in [env], or
    any declared rule / closure, and hand the rename map to the substitution
    primitive. *)
let alpha_rename_binders env (params : Ast.param list) (guards : Ast.guard list)
    (body : Ast.expr) : Ast.param list * Ast.guard list * Ast.expr =
  let is_rule_name name =
    match[@warning "-4"] Env.lookup_term name env with
    | Some { kind = Env.KRule _ | Env.KClosure _; _ } -> true
    | _ -> false
  in
  let binder_names =
    let from_params =
      List.map (fun (p : Ast.param) -> Ast.lower_name p.param_name) params
    in
    let from_guards =
      List.filter_map
        (fun g ->
          match g with
          | GParam p -> Some (Ast.lower_name p.param_name)
          | GIn (Lower n, _) -> Some n
          | GExpr _ -> None)
        guards
    in
    from_params @ from_guards
  in
  (* Seed [occupied] with sibling binders in this quantifier and any outer
     quantifier binder in scope via [env.vars]. A fresh name matching an
     enclosing binder would capture outer references when the body rebinds;
     a fresh name matching a sibling would duplicate. Rules / closures are
     checked separately in [fresh_for]. *)
  let occupied =
    ref
      (Env.fold_all_terms
         (fun name entry acc ->
           match entry.Env.kind with
           | Env.KVar _ -> Smt_doc.StringSet.add name acc
           | Env.KRule _ | Env.KClosure _ | Env.KDomain | Env.KAlias _ -> acc)
         env
         (Smt_doc.StringSet.of_list binder_names))
  in
  let fresh_for orig =
    let rec try_n n =
      let cand =
        if n = 0 then Printf.sprintf "%s_q" orig
        else Printf.sprintf "%s_q%d" orig n
      in
      if Smt_doc.StringSet.mem cand !occupied || is_rule_name cand then
        try_n (n + 1)
      else cand
    in
    let name = try_n 0 in
    occupied := Smt_doc.StringSet.add name !occupied;
    name
  in
  let renames =
    List.filter_map
      (fun orig ->
        if is_rule_name orig then Some (orig, fresh_for orig) else None)
      binder_names
  in
  if renames = [] then (params, guards, body)
  else
    let rename_param (p : Ast.param) =
      match List.assoc_opt (Ast.lower_name p.param_name) renames with
      | Some fresh -> { p with param_name = Lower fresh }
      | None -> p
    in
    let params' = List.map rename_param params in
    (* Params are in scope for every guard and the body; seed [active] with
       their renames. Guards fold left-to-right, adding each binder's rename
       to [active] only AFTER the guard that introduces it — so the guard's
       own expression (and earlier guards' expressions) see the pre-binder
       scope. This matters when a binder name shadows an outer rule: the
       reference in an earlier guard should resolve to the rule and must not
       be rewritten to the bound-variable form. GIn's list expression is
       evaluated in the OUTER scope per [ast.ml], so it too is substituted
       under the pre-binder [active]. *)
    let param_subst =
      List.filter_map
        (fun (p : Ast.param) ->
          let n = Ast.lower_name p.param_name in
          match List.assoc_opt n renames with
          | Some fresh -> Some (n, EVar (Lower fresh))
          | None -> None)
        params
    in
    let extend_subst subst name =
      match List.assoc_opt name renames with
      | Some fresh -> (name, EVar (Lower fresh)) :: subst
      | None -> subst
    in
    let guards_rev, final_subst =
      List.fold_left
        (fun (acc, active) g ->
          match g with
          | GExpr e -> (GExpr (Smt_expr.substitute_vars active e) :: acc, active)
          | GParam p ->
              let n = Ast.lower_name p.param_name in
              (GParam (rename_param p) :: acc, extend_subst active n)
          | GIn (Lower n, e) ->
              let e' = Smt_expr.substitute_vars active e in
              let n' =
                match List.assoc_opt n renames with Some x -> x | None -> n
              in
              (GIn (Lower n', e') :: acc, extend_subst active n))
        ([], param_subst) guards
    in
    let guards' = List.rev guards_rev in
    let body' = Smt_expr.substitute_vars final_subst body in
    (params', guards', body')

(** Translate an expression to SMT-LIB2 term string *)
let rec translate_expr config env (e : expr) =
  match e with
  | ELitBool true -> "true"
  | ELitBool false -> "false"
  | ELitNat n -> string_of_int n
  | ELitReal f -> Printf.sprintf "%.17g" f
  | ELitString s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | EVar (Lower name) -> sanitize_ident name
  | EDomain (Upper name) ->
      (* Domain as a set value shouldn't appear standalone — OpIn, OpSubset,
         and OpCard all handle EDomain specially. If we reach here, it's an
         internal error in the translation. *)
      failwith
        (Printf.sprintf
           "SMT translation: EDomain '%s' appeared in standalone position" name)
  | EQualified (_, name) -> sanitize_ident name
  | EPrimed (Lower name) -> sanitize_ident name ^ "_prime"
  | EApp (func, args) -> translate_app config env func args
  | ETuple exprs ->
      let ts = List.map (translate_expr config env) exprs in
      (* Infer component types to build the correct constructor name *)
      let component_sorts =
        List.map
          (fun sub_e ->
            match Check.infer_type { Check.env; loc = dummy_loc } sub_e with
            | Ok ty -> sort_base_name ty
            | Error _ ->
                failwith "SMT translation: cannot infer tuple component type")
          exprs
      in
      let ctor = "mk_Pair_" ^ String.concat "_" component_sorts in
      Printf.sprintf "(%s %s)" ctor (String.concat " " ts)
  | EProj (e, idx) ->
      Printf.sprintf "(fst_%d %s)" idx (translate_expr config env e)
  | EBinop (op, e1, e2) -> translate_binop config env op e1 e2
  | EUnop (op, e) -> translate_unop config env op e
  | EForall (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      translate_quantifier config env "forall" params guards body
  | EEach (mb, metas, None) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      translate_forall_comprehension config env params guards body
  | EEach (mb, metas, Some comb) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      translate_aggregate config env comb params guards body
  | EExists (mb, metas) ->
      let params, guards, body = Ast.unbind_quant mb metas in
      translate_quantifier config env "exists" params guards body
  | ECond arms -> translate_cond config env arms
  | EInitially e -> translate_expr config env e
  | EOverride (Lower name, pairs) -> translate_override config env name pairs

and translate_app config env func args =
  (* List-search: xs x where xs : [T] and x : T (non-numeric T, non-Nat x)
     emits a fresh uninterpreted Int. The (x in xs) guard injected by
     collect_body_guards makes the value sound when x is present; when
     absent, the guard absorbs the assertion so the value doesn't matter.
     Primed terms fail Check.infer_type outside action-context, so we
     retry against the unprimed form. Identical [(func, arg)] translations
     share a placeholder via [intern_list_search_symbol] so that repeats
     like [xs x = xs x] stay referentially stable. *)
  let infer_with_unprime e =
    match Check.infer_type { Check.env; loc = dummy_loc } e with
    | Ok ty -> Some ty
    | Error _ -> (
        match
          Check.infer_type { Check.env; loc = dummy_loc } (unprime_expr e)
        with
        | Ok ty -> Some ty
        | Error _ -> None)
  in
  let list_search =
    match args with
    | [ arg ] -> (
        match[@warning "-4"] infer_with_unprime func with
        | Some (TyList elem_ty) -> (
            match[@warning "-4"] infer_with_unprime arg with
            | Some arg_ty
              when is_subtype arg_ty elem_ty
                   && (not (is_subtype arg_ty TyNat))
                   && not (is_numeric elem_ty) ->
                (* Key on a pure AST serialization: translate_expr is
                   stateful (mints fallback/cond-default names and queues
                   declarations), so using its output would break interning
                   for any list-search whose subexpressions trigger those
                   paths and would leave orphaned declarations behind. *)
                let func_s = Ast.show_expr func in
                let arg_s = Ast.show_expr arg in
                Some (intern_list_search_symbol ~func_s ~arg_s)
            | _ -> None)
        | _ -> None)
    | _ -> None
  in
  match list_search with
  | Some name -> name
  | None -> (
      match[@warning "-4"] func with
      | EOverride (Lower name, pairs) ->
          (* f[k |-> v] applied to args: inline ite chain. For arity-1 rules
             the key is a bare expression compared against the single arg;
             for arity-N rules the key is an N-tuple whose components are
             compared componentwise against the args, yielding a conjunctive
             guard — McCarthy's [store] extended to multi-index arrays
             (Kroening & Strichman Ch. 7). *)
          let sname = sanitize_ident name in
          let args_str = List.map (translate_expr config env) args in
          let applied_args = String.concat " " args_str in
          (match args_str with
          | [] -> failwith "SMT translation: override applied with 0 arguments"
          | _ -> ());
          let rec build_chain = function
            | [] -> Printf.sprintf "(%s %s)" sname applied_args
            | (k, v) :: rest ->
                let guard_str =
                  match args_str with
                  | [ arg ] ->
                      (* Arity-1: bare-expression key, even when the key is
                         itself a tuple literal (e.g. a rule whose single
                         parameter has a product type). *)
                      Printf.sprintf "(= %s %s)" arg
                        (translate_expr config env k)
                  | _ -> (
                      match[@warning "-4"] k with
                      | ETuple parts ->
                          if List.length parts <> List.length args_str then
                            failwith
                              "SMT translation: override key arity does not \
                               match application arity";
                          let eqs =
                            List.map2
                              (fun part arg ->
                                Printf.sprintf "(= %s %s)" arg
                                  (translate_expr config env part))
                              parts args_str
                          in
                          Printf.sprintf "(and %s)" (String.concat " " eqs)
                      | _ ->
                          failwith
                            "SMT translation: override key arity does not \
                             match application arity")
                in
                Printf.sprintf "(ite %s %s %s)" guard_str
                  (translate_expr config env v)
                  (build_chain rest)
          in
          build_chain pairs
      | _ ->
          let func_str =
            match[@warning "-4"] func with
            | EVar (Lower name) -> sanitize_ident name
            | EPrimed (Lower name) -> sanitize_ident name ^ "_prime"
            | _ -> translate_expr config env func
          in
          let args_str = List.map (translate_expr config env) args in
          Printf.sprintf "(%s %s)" func_str (String.concat " " args_str))

and translate_binop config env op e1 e2 =
  match op with
  (* OpIn and OpSubset handle EDomain specially — don't eagerly translate *)
  | OpIn -> translate_in config env e1 e2
  | OpSubset -> translate_subset config env e1 e2
  | OpAnd | OpOr | OpImpl | OpIff | OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe
  | OpAdd | OpSub | OpMul | OpDiv -> (
      let t1 = translate_expr config env e1 in
      let t2 = translate_expr config env e2 in
      match op with
      | OpAnd -> Printf.sprintf "(and %s %s)" t1 t2
      | OpOr -> Printf.sprintf "(or %s %s)" t1 t2
      | OpImpl -> Printf.sprintf "(=> %s %s)" t1 t2
      | OpIff -> Printf.sprintf "(= %s %s)" t1 t2
      | OpEq -> Printf.sprintf "(= %s %s)" t1 t2
      | OpNeq -> Printf.sprintf "(not (= %s %s))" t1 t2
      | OpLt -> Printf.sprintf "(< %s %s)" t1 t2
      | OpGt -> Printf.sprintf "(> %s %s)" t1 t2
      | OpLe -> Printf.sprintf "(<= %s %s)" t1 t2
      | OpGe -> Printf.sprintf "(>= %s %s)" t1 t2
      | OpAdd -> Printf.sprintf "(+ %s %s)" t1 t2
      | OpSub -> Printf.sprintf "(- %s %s)" t1 t2
      | OpMul -> Printf.sprintf "(* %s %s)" t1 t2
      | OpDiv -> Printf.sprintf "(div %s %s)" t1 t2
      | OpIn | OpSubset -> assert false (* handled above *))

and translate_in config env elem set =
  match[@warning "-4"] set with
  | EDomain (Upper name) -> (
      (* x in Domain → disjunction over domain elements *)
      let elems = domain_elements name (bound_for config name) in
      let elem_str = translate_expr config env elem in
      let disj =
        List.map (fun e -> Printf.sprintf "(= %s %s)" elem_str e) elems
      in
      match disj with
      | [] -> "false"
      | [ single ] -> single
      | _ -> Printf.sprintf "(or %s)" (String.concat " " disj))
  | EEach (mb, metas, None) -> (
      (* y in (each x: D | f x) → disjunction: (= y (f d0)) ∨ (= y (f d1)) ...
         y in (each x: D, g x | f x) → (g d0 ∧ = y (f d0)) ∨ ... *)
      let params, guards, body = Ast.unbind_quant mb metas in
      let expanded =
        expand_comprehension translate_expr config env params guards body
      in
      let elem_str = translate_expr config env elem in
      let disj =
        List.map
          (fun (guard_opt, value_str) ->
            let eq = Printf.sprintf "(= %s %s)" elem_str value_str in
            match guard_opt with
            | None -> eq
            | Some g -> Printf.sprintf "(and %s %s)" g eq)
          expanded
      in
      match disj with
      | [ single ] -> single
      | _ -> Printf.sprintf "(or %s)" (String.concat " " disj))
  | _ ->
      (* x in xs where xs : (Array T Bool) → (select xs x) *)
      let elem_str = translate_expr config env elem in
      let set_str = translate_expr config env set in
      Printf.sprintf "(select %s %s)" set_str elem_str

and translate_subset config env e1 e2 =
  let s1 = translate_expr config env e1 in
  (* xs subset Domain → every member of xs is in the domain (tautological
     for well-typed programs, but expand over finite elements for soundness) *)
  match[@warning "-4"] e2 with
  | EEach (mb, metas, None) -> (
      (* xs subset (each x: D | f x) → for every elem of xs, elem is in the
         comprehension. Expand: for each domain elem d of the LHS element type,
         (select xs d) => (exists comprehension elem matching d). *)
      let params, guards, body = Ast.unbind_quant mb metas in
      let expanded =
        expand_comprehension translate_expr config env params guards body
      in
      (* Infer LHS element type to get its domain *)
      match[@warning "-4"]
        Check.infer_type { Check.env; loc = dummy_loc } e1
      with
      | Ok (TyList (TyDomain lhs_dname)) ->
          let lhs_elems =
            domain_elements lhs_dname (bound_for config lhs_dname)
          in
          let conjuncts =
            List.map
              (fun d ->
                let in_comp =
                  List.map
                    (fun (guard_opt, value_str) ->
                      let eq = Printf.sprintf "(= %s %s)" d value_str in
                      match guard_opt with
                      | None -> eq
                      | Some g -> Printf.sprintf "(and %s %s)" g eq)
                    expanded
                in
                Printf.sprintf "(=> (select %s %s) (or %s))" s1 d
                  (String.concat " " in_comp))
              lhs_elems
          in
          Printf.sprintf "(and %s)" (String.concat " " conjuncts)
      | _ ->
          failwith
            "SMT translation: subset with comprehension RHS requires domain \
             element type on LHS")
  | EDomain (Upper name) ->
      let elems = domain_elements name (bound_for config name) in
      let conjuncts =
        List.map
          (fun d ->
            Printf.sprintf "(=> (select %s %s) (or %s))" s1 d
              (String.concat " "
                 (List.map (fun e -> Printf.sprintf "(= %s %s)" d e) elems)))
          elems
      in
      Printf.sprintf "(and %s)" (String.concat " " conjuncts)
  | _ -> (
      let s2 = translate_expr config env e2 in
      (* xs subset ys: infer element type for quantifier binding *)
      match[@warning "-4"]
        Check.infer_type { Check.env; loc = dummy_loc } e1
      with
      | Ok (TyList (TyDomain name)) ->
          (* For domain lists, expand over finite domain elements *)
          let elems = domain_elements name (bound_for config name) in
          let conjuncts =
            List.map
              (fun d ->
                Printf.sprintf "(=> (select %s %s) (select %s %s))" s1 d s2 d)
              elems
          in
          Printf.sprintf "(and %s)" (String.concat " " conjuncts)
      | Ok (TyList elem_ty) ->
          let sort = sort_of_ty elem_ty in
          Printf.sprintf
            "(forall ((x_sub %s)) (=> (select %s x_sub) (select %s x_sub)))"
            sort s1 s2
      | _ ->
          Printf.sprintf
            "(forall ((x_sub Int)) (=> (select %s x_sub) (select %s x_sub)))" s1
            s2)

and translate_unop config env op e =
  match op with
  | OpNot -> Printf.sprintf "(not %s)" (translate_expr config env e)
  | OpNeg -> Printf.sprintf "(- %s)" (translate_expr config env e)
  | OpCard -> translate_card config env e

and translate_card config env e =
  match[@warning "-4"] e with
  | EDomain (Upper name) ->
      (* #Domain = bound (all elements exist) *)
      string_of_int (bound_for config name)
  | EEach (mb, metas, None) -> (
      (* #(each x: D | f x) — count distinct values in the comprehension.
         Requires the range type to be a bounded domain. Expand over range
         domain elements, check if each is produced by any source element. *)
      let params, guards, body = Ast.unbind_quant mb metas in
      let expanded =
        expand_comprehension translate_expr config env params guards body
      in
      (* Infer the body type to determine the range domain *)
      let param_bindings = resolve_param_bindings env params in
      let guard_bindings =
        List.filter_map
          (fun g ->
            match g with
            | GIn (Lower name, list_expr) -> (
                let infer e =
                  match[@warning "-4"]
                    Check.infer_type { Check.env; loc = dummy_loc } e
                  with
                  | Ok (TyList elem_ty) -> Some (name, elem_ty)
                  | _ -> None
                in
                match infer list_expr with
                | Some _ as r -> r
                | None -> infer (unprime_expr list_expr))
            | GExpr _ | GParam _ -> None)
          guards
      in
      let env_inner = Env.with_vars (param_bindings @ guard_bindings) env in
      match[@warning "-4"]
        Check.infer_type { Check.env = env_inner; loc = dummy_loc } body
      with
      | Ok (TyDomain range_dname) ->
          let range_elems =
            domain_elements range_dname (bound_for config range_dname)
          in
          let terms =
            List.map
              (fun u ->
                let in_comp =
                  List.map
                    (fun (guard_opt, value_str) ->
                      let eq = Printf.sprintf "(= %s %s)" u value_str in
                      match guard_opt with
                      | None -> eq
                      | Some g -> Printf.sprintf "(and %s %s)" g eq)
                    expanded
                in
                Printf.sprintf "(ite (or %s) 1 0)" (String.concat " " in_comp))
              range_elems
          in
          Printf.sprintf "(+ %s)" (String.concat " " terms)
      | _ ->
          failwith
            "SMT translation: cardinality of comprehension requires domain \
             range type")
  | _ -> (
      (* #xs where xs : (Array T Bool) — count members over finite domain *)
      let set_str = translate_expr config env e in
      match[@warning "-4"]
        Check.infer_type { Check.env; loc = dummy_loc } e
      with
      | Ok (TyList (TyDomain name)) ->
          (* Sum over domain elements: (+ (ite (select xs d0) 1 0) ...) *)
          let elems = domain_elements name (bound_for config name) in
          let terms =
            List.map
              (fun d -> Printf.sprintf "(ite (select %s %s) 1 0)" set_str d)
              elems
          in
          Printf.sprintf "(+ %s)" (String.concat " " terms)
      | Ok (TyList _elem_ty) ->
          (* Non-domain list: element type is unbounded (e.g., Int) so
             cardinality can't be computed in bounded model checking. Emit a
             fresh non-negative integer constant so the gap is visible to
             downstream tooling rather than silently constrained to 0. *)
          let _ = set_str in
          let name = fresh_fallback ~kind:"card" ~sort:"Int" in
          add_fallback_assert (Printf.sprintf "(>= %s 0)" name);
          name
      | _ ->
          (* Can't determine element type at all — same fallback. *)
          let _ = set_str in
          let name = fresh_fallback ~kind:"card" ~sort:"Int" in
          add_fallback_assert (Printf.sprintf "(>= %s 0)" name);
          name)

and translate_forall_comprehension config env params guards body =
  (* Standalone comprehension: (all x: D | f x) → array where exactly
     the comprehension results are members. Build a store chain:
     (let ((_cl_0 ((as const (Array <sort> Bool)) false)))
     (let ((_cl_1 (store _cl_0 v0 true)))
     (let ((_cl_2 (ite g1 (store _cl_1 v1 true) _cl_1)))
       _cl_2))) *)
  let expanded =
    expand_comprehension translate_expr config env params guards body
  in
  (* Infer body type to determine element sort *)
  let param_bindings = resolve_param_bindings env params in
  let guard_bindings =
    List.filter_map
      (fun g ->
        match g with
        | GIn (Lower name, list_expr) -> (
            let infer e =
              match[@warning "-4"]
                Check.infer_type { Check.env; loc = dummy_loc } e
              with
              | Ok (TyList elem_ty) -> Some (name, elem_ty)
              | _ -> None
            in
            match infer list_expr with
            | Some _ as r -> r
            | None -> infer (unprime_expr list_expr))
        | GExpr _ | GParam _ -> None)
      guards
  in
  let env_inner = Env.with_vars (param_bindings @ guard_bindings) env in
  let elem_sort =
    match Check.infer_type { Check.env = env_inner; loc = dummy_loc } body with
    | Ok ty -> sort_of_ty ty
    | Error _ ->
        failwith
          "SMT translation: cannot infer element type for standalone \
           comprehension"
  in
  let empty = Printf.sprintf "((as const (Array %s Bool)) false)" elem_sort in
  let bindings =
    List.mapi
      (fun i (guard_opt, value_str) ->
        let prev = Printf.sprintf "_cl_%d" i in
        let next = Printf.sprintf "_cl_%d" (i + 1) in
        let store_expr = Printf.sprintf "(store %s %s true)" prev value_str in
        let bind_expr =
          match guard_opt with
          | None -> store_expr
          | Some g -> Printf.sprintf "(ite %s %s %s)" g store_expr prev
        in
        (next, bind_expr))
      expanded
  in
  let n = List.length bindings in
  let final_var = if n = 0 then "_cl_0" else Printf.sprintf "_cl_%d" n in
  let inner =
    List.fold_right
      (fun (name, expr) acc ->
        Printf.sprintf "(let ((%s %s)) %s)" name expr acc)
      bindings final_var
  in
  Printf.sprintf "(let ((_cl_0 %s)) %s)" empty inner

and translate_aggregate config env (comb : combiner) params guards body =
  (* Aggregate: COMB over each x: D, g | f x
     Expands over finite domain elements and combines with the operator.
     For +, *, and, or: guarded elements use identity when guard is false.
     For min, max: pairwise fold using ite. *)
  let local_bound =
    List.map (fun (p : param) -> Ast.lower_name p.param_name) params
    @ List.concat_map
        (function
          | GParam p -> [ Ast.lower_name p.param_name ]
          | GIn (n, _) -> [ Ast.lower_name n ]
          | GExpr _ -> [])
        guards
  in
  let inner_config =
    { config with quant_bound = local_bound @ config.quant_bound }
  in
  let expanded =
    expand_comprehension translate_expr inner_config env params guards body
  in
  (* Inject declaration guards from guarded rule applications in the body *)
  let expanded =
    if not config.inject_guards then expanded
    else
      match resolve_comprehension_binding env params guards with
      | Error _ -> expanded
      | Ok (var_name, dname, _, bindings) ->
          let env_inner = Env.with_vars bindings env in
          let bound_names = local_bound @ config.quant_bound in
          let app_guards =
            collect_body_guards ~bound:bound_names env_inner body
          in
          if app_guards = [] then expanded
          else
            let pname = sanitize_ident var_name in
            let guard_templates =
              List.map (translate_expr inner_config env_inner) app_guards
            in
            let elems = domain_elements dname (bound_for inner_config dname) in
            List.map2
              (fun (guard_opt, value_str) elem ->
                let sub s = replace_word ~from:pname ~to_:elem s in
                let app_guard_strs = List.map sub guard_templates in
                let all_guards =
                  (match guard_opt with Some g -> [ g ] | None -> [])
                  @ app_guard_strs
                in
                let merged =
                  match all_guards with
                  | [] -> None
                  | [ g ] -> Some g
                  | gs ->
                      Some (Printf.sprintf "(and %s)" (String.concat " " gs))
                in
                (merged, value_str))
              expanded elems
  in
  (* Infer body type to choose correct SMT sort for identity values *)
  let body_is_real =
    match resolve_comprehension_binding env params guards with
    | Ok (_, _, _, bindings) -> (
        let env_inner = Env.with_vars bindings env in
        match
          Check.infer_type { Check.env = env_inner; loc = dummy_loc } body
        with
        | Ok TyReal -> true
        | Ok
            ( TyBool | TyNat | TyNat0 | TyInt | TyString | TyNothing
            | TyDomain _ | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
        | Error _ ->
            false)
    | Error _ -> false
  in
  let smt_op_and_identity =
    match comb with
    | CombAdd -> Some ("+", if body_is_real then "0.0" else "0")
    | CombMul -> Some ("*", if body_is_real then "1.0" else "1")
    | CombAnd -> Some ("and", "true")
    | CombOr -> Some ("or", "false")
    | CombMin | CombMax -> None
  in
  match smt_op_and_identity with
  | Some (smt_op, identity) -> (
      let values =
        List.map
          (fun (guard_opt, value_str) ->
            match guard_opt with
            | None -> value_str
            | Some g -> Printf.sprintf "(ite %s %s %s)" g value_str identity)
          expanded
      in
      match values with
      | [] -> identity
      | [ single ] -> single
      | _ -> Printf.sprintf "(%s %s)" smt_op (String.concat " " values))
  | None -> (
      (* min/max: no identity element, inline comparison with ite.
         For guarded elements, only include when the guard holds;
         use a seen/acc pair so the first accepted value seeds the fold. *)
      let cmp_op =
        match comb with
        | CombMin -> "<="
        | CombMax -> ">="
        | CombAdd | CombMul | CombAnd | CombOr -> assert false
      in
      let inline_minmax a b =
        Printf.sprintf "(ite (%s %s %s) %s %s)" cmp_op a b a b
      in
      match expanded with
      | [] -> failwith "SMT: min/max over empty domain"
      | _ ->
          (* Each element is either unconditional or guarded. We fold using
             a (seen_flag, accumulator) pair encoded in SMT: seen_flag is a
             Bool that tracks whether any element has been accepted yet.
             When seen is false, the new value replaces acc unconditionally. *)
          let seen_var = "_agg_seen" in
          let acc_var = "_agg_acc" in
          let seen_ref = ref seen_var in
          let acc_ref = ref acc_var in
          let bindings = ref [] in
          let cnt = ref 0 in
          List.iter
            (fun (guard_opt, value_str) ->
              let i = !cnt in
              incr cnt;
              let prev_seen = !seen_ref in
              let prev_acc = !acc_ref in
              let new_seen = Printf.sprintf "_agg_s%d" i in
              let new_acc = Printf.sprintf "_agg_a%d" i in
              let merged =
                match guard_opt with
                | None ->
                    (* Unconditional: always accepted *)
                    let acc_expr =
                      Printf.sprintf "(ite %s %s %s)" prev_seen
                        (inline_minmax prev_acc value_str)
                        value_str
                    in
                    (new_seen, "true", new_acc, acc_expr)
                | Some g ->
                    (* Guarded: only accepted when guard holds *)
                    let seen_expr = Printf.sprintf "(or %s %s)" prev_seen g in
                    let acc_expr =
                      Printf.sprintf "(ite %s (ite %s %s %s) %s)" g prev_seen
                        (inline_minmax prev_acc value_str)
                        value_str prev_acc
                    in
                    (new_seen, seen_expr, new_acc, acc_expr)
              in
              let ns, se, na, ae = merged in
              bindings := (na, ae) :: (ns, se) :: !bindings;
              seen_ref := ns;
              acc_ref := na)
            expanded;
          (* Wrap in nested lets, seeded with seen=false and acc=0 (dummy) *)
          let inner = !acc_ref in
          let lets =
            List.fold_left
              (fun body (name, expr) ->
                Printf.sprintf "(let ((%s %s)) %s)" name expr body)
              inner (List.rev !bindings)
          in
          let dummy_acc = if body_is_real then "0.0" else "0" in
          Printf.sprintf "(let ((%s false) (%s %s)) %s)" seen_var acc_var
            dummy_acc lets)

and translate_quantifier config env quant params guards body =
  (* Alpha-rename any binder whose string name would shadow a declared rule /
     closure symbol at the SMT top level — Bindlib keeps binder *identity*
     fresh but preserves the user-chosen name string, and SMT's scope rules
     would reinterpret [(name x)] inside the quantifier body as applying the
     bound variable. See [alpha_rename_binders]. *)
  let params, guards, body = alpha_rename_binders env params guards body in
  (* Enrich env with formal parameter bindings so that type inference
     on guard expressions (e.g., GIn list exprs) can resolve them. *)
  let param_bindings = resolve_param_bindings env params in
  let env = Env.with_vars param_bindings env in
  (* Collect bindings, guard conditions, and type constraints for Nat/Nat0 *)
  let nat_constraint_of_param env (p : param) =
    match Collect.resolve_type env p.param_type dummy_loc with
    | Ok TyNat ->
        Some
          (Printf.sprintf "(>= %s 1)"
             (sanitize_ident (Ast.lower_name p.param_name)))
    | Ok TyNat0 ->
        Some
          (Printf.sprintf "(>= %s 0)"
             (sanitize_ident (Ast.lower_name p.param_name)))
    | Ok
        ( TyBool | TyInt | TyReal | TyString | TyNothing | TyDomain _ | TyList _
        | TyProduct _ | TySum _ | TyFunc _ )
    | Error _ ->
        None
  in
  let bindings =
    List.map
      (fun (p : param) ->
        let sort =
          match resolve_param_sort env p.param_type with
          | Some s -> s
          | None ->
              failwith
                (Printf.sprintf
                   "SMT translation: cannot resolve sort for parameter '%s'"
                   (Ast.lower_name p.param_name))
        in
        Printf.sprintf "(%s %s)"
          (sanitize_ident (Ast.lower_name p.param_name))
          sort)
      params
  in
  let param_type_conditions =
    List.filter_map (nat_constraint_of_param env) params
  in
  let guard_bindings, guard_conditions, env =
    List.fold_left
      (fun (binds, conds, env) g ->
        match g with
        | GParam p ->
            let sort =
              match resolve_param_sort env p.param_type with
              | Some s -> s
              | None ->
                  failwith
                    (Printf.sprintf
                       "SMT translation: cannot resolve sort for guard \
                        parameter '%s'"
                       (Ast.lower_name p.param_name))
            in
            let env =
              match Collect.resolve_type env p.param_type dummy_loc with
              | Ok ty -> Env.with_vars [ (Ast.lower_name p.param_name, ty) ] env
              | Error _ -> env
            in
            let conds =
              match nat_constraint_of_param env p with
              | Some c -> c :: conds
              | None -> conds
            in
            ( Printf.sprintf "(%s %s)"
                (sanitize_ident (Ast.lower_name p.param_name))
                sort
              :: binds,
              conds,
              env )
        | GIn (Lower name, list_expr) ->
            (* Bind name as element type; resolve from list expression type *)
            let infer_elem_ty e =
              match[@warning "-4"]
                Check.infer_type { Check.env; loc = dummy_loc } e
              with
              | Ok (TyList elem_ty) -> Some elem_ty
              | Ok (TyDomain _ as ty) -> Some ty
              | _ -> None
            in
            (* When the expression has been primed (for invariant preservation
               checks), the primed name won't be in the type environment.
               Fall back to unpriming before inference. *)
            let elem_ty_opt =
              match infer_elem_ty list_expr with
              | Some _ as r -> r
              | None -> infer_elem_ty (unprime_expr list_expr)
            in
            let elem_sort =
              match elem_ty_opt with
              | Some elem_ty -> sort_of_ty elem_ty
              | None ->
                  failwith
                    (Printf.sprintf
                       "SMT translation: cannot infer element type for '%s' in \
                        membership guard"
                       name)
            in
            let env =
              match elem_ty_opt with
              | Some elem_ty -> Env.with_vars [ (name, elem_ty) ] env
              | None -> env
            in
            let guard_str =
              translate_in config env (EVar (Lower name)) list_expr
            in
            ( Printf.sprintf "(%s %s)" (sanitize_ident name) elem_sort :: binds,
              guard_str :: conds,
              env )
        | GExpr e -> (binds, translate_expr config env e :: conds, env))
      ([], [], env) guards
  in
  let all_bindings = bindings @ List.rev guard_bindings in
  (* Collect guards from guarded function applications in body *)
  let local_bound =
    List.map (fun (p : param) -> Ast.lower_name p.param_name) params
    @ List.concat_map
        (fun g ->
          match g with
          | GParam p -> [ Ast.lower_name p.param_name ]
          | GIn (n, _) -> [ Ast.lower_name n ]
          | GExpr _ -> [])
        guards
  in
  (* Combine with outer quantifier-bound names so that guard injection
     in nested quantifiers does not incorrectly prime outer-bound vars. *)
  let bound_names = local_bound @ config.quant_bound in
  let app_guards =
    if config.inject_guards then collect_body_guards ~bound:bound_names env body
    else []
  in
  let app_guard_strs = List.map (translate_expr config env) app_guards in
  let inner_config = { config with quant_bound = bound_names } in
  let body_str = translate_expr inner_config env body in
  let conditions =
    param_type_conditions @ List.rev guard_conditions @ app_guard_strs
  in
  let binding_str = String.concat " " all_bindings in
  match (quant, conditions) with
  | "forall", _ :: _ ->
      Printf.sprintf "(%s (%s) (=> (and %s) %s))" quant binding_str
        (String.concat " " conditions)
        body_str
  | "exists", _ :: _ ->
      Printf.sprintf "(%s (%s) (and %s %s))" quant binding_str
        (String.concat " " conditions)
        body_str
  | _ -> Printf.sprintf "(%s (%s) %s)" quant binding_str body_str

and translate_cond config env = function[@warning "-4"]
  | [] -> assert false
  | [ (ELitBool true, cons) ] ->
      (* Catch-all: last arm is unconditional *)
      translate_expr config env cons
  | [ (arm, cons) ] ->
      (* Last arm has a real guard — don't drop it. Use a fresh uninterpreted
         constant for the else-branch so the gap is unconstrained. *)
      let sort =
        match Check.infer_type { Check.env; loc = dummy_loc } cons with
        | Ok ty -> sort_of_ty ty
        | Error _ -> "Int"
      in
      let default = fresh_cond_default sort in
      Printf.sprintf "(ite %s %s %s)"
        (translate_expr config env arm)
        (translate_expr config env cons)
        default
  | (arm, cons) :: rest ->
      Printf.sprintf "(ite %s %s %s)"
        (translate_expr config env arm)
        (translate_expr config env cons)
        (translate_cond config env rest)

and translate_override _config _env name _pairs =
  (* Standalone override (not applied) — can't be directly represented in
     SMT-LIB2 without higher-order functions. Applied overrides are handled
     in translate_app. Emit a fresh constant so the gap is visible to
     downstream tooling rather than emitting a malformed token. The base
     function name is recorded only for debug context. *)
  let _ = name in
  fresh_fallback ~kind:"override" ~sort:"Int"

(** Translate a proposition expression to SMT, injecting guards from guarded
    function applications. For quantified expressions, delegates to
    translate_expr (guards are injected inside translate_quantifier). For
    non-quantified expressions, wraps with guard antecedent if needed. *)
let translate_proposition config env (e : expr) =
  match e with
  | EForall _ | EExists _ | EEach _ ->
      (* Quantifiers handle their own guard injection *)
      translate_expr config env e
  | EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
  | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
  | EBinop _ | EUnop _ | ECond _ | EInitially _ -> (
      if not config.inject_guards then translate_expr config env e
      else
        let app_guards = collect_body_guards env e in
        let smt_expr = translate_expr config env e in
        match app_guards with
        | [] -> smt_expr
        | _ ->
            let guard_strs = List.map (translate_expr config env) app_guards in
            let guard_conj =
              match guard_strs with
              | [ g ] -> g
              | gs -> Printf.sprintf "(and %s)" (String.concat " " gs)
            in
            Printf.sprintf "(=> %s %s)" guard_conj smt_expr)

(** Translate a list of propositions into a conjunction *)
let conjoin_propositions config env props =
  match props with
  | [] -> "true"
  | [ p ] -> translate_proposition config env p.value
  | _ ->
      let parts =
        List.map
          (fun (p : expr located) -> translate_proposition config env p.value)
          props
      in
      Printf.sprintf "(and %s)" (String.concat " " parts)

(** Translate a precondition expression, conjoining any implicit guards (from
    declaration guards or list-search membership) as additional conjuncts. A
    precondition that mentions e.g. [xs x] semantically requires both
    [(x in xs)] and the body — unlike an invariant, the guard cannot be an
    implication, because [(=> G P)] is vacuously true when [G] is false and
    would let contradiction/precondition/BMC queries miss real infeasibilities.
*)
let translate_precondition config env (e : expr) : string =
  if not config.inject_guards then translate_expr config env e
  else
    let app_guards = collect_body_guards env e in
    let body = translate_expr config env e in
    match app_guards with
    | [] -> body
    | _ ->
        let guard_strs = List.map (translate_expr config env) app_guards in
        Printf.sprintf "(and %s %s)" (String.concat " " guard_strs) body

let extract_preconditions config env (guards : guard list) =
  List.filter_map
    (fun g ->
      match g with
      | GExpr e -> Some (translate_precondition config env e)
      | GIn _ | GParam _ -> None)
    guards

let build_value_terms config env (params : param list) =
  let param_terms =
    List.map
      (fun (p : param) -> sanitize_ident (Ast.lower_name p.param_name))
      params
  in
  let param_set =
    List.filter_map
      (fun (p : param) ->
        match Collect.resolve_type env p.param_type dummy_loc with
        | Ok ty -> Some (sanitize_ident (Ast.lower_name p.param_name), ty)
        | Error _ -> None)
      params
  in
  let func_terms =
    Env.fold_terms
      (fun name entry acc ->
        match entry.Env.kind with
        | Env.KRule ty | Env.KClosure (ty, _) -> (
            let sname = sanitize_ident name in
            match[@warning "-4"] decompose_func_ty ty with
            | Some ([], _ret) ->
                (* Nullary rule: include current and primed *)
                sname :: (sname ^ "_prime") :: acc
            | Some ([ param_ty ], _ret) ->
                (* Unary rule: apply to each matching param *)
                let from_params =
                  List.filter_map
                    (fun (sp, resolved_ty) ->
                      if resolved_ty = param_ty then
                        Some
                          [
                            Printf.sprintf "(%s %s)" sname sp;
                            Printf.sprintf "(%s_prime %s)" sname sp;
                          ]
                      else None)
                    param_set
                in
                (* Also apply to domain elements for full coverage *)
                let from_elems =
                  match param_ty with
                  | TyDomain dname ->
                      let elems =
                        domain_elements dname (bound_for config dname)
                      in
                      (* Exclude elements that duplicate action param applications *)
                      let param_names =
                        List.filter_map
                          (fun (sp, resolved_ty) ->
                            if resolved_ty = param_ty then Some sp else None)
                          param_set
                      in
                      List.concat_map
                        (fun elem ->
                          if List.mem elem param_names then []
                          else
                            [
                              Printf.sprintf "(%s %s)" sname elem;
                              Printf.sprintf "(%s_prime %s)" sname elem;
                            ])
                        elems
                  | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString
                  | TyNothing | TyList _ | TyProduct _ | TySum _ | TyFunc _ ->
                      []
                in
                List.concat from_params @ from_elems @ acc
            | _ -> acc)
        | Env.KDomain | Env.KAlias _ | Env.KVar _ -> acc)
      env []
  in
  param_terms @ func_terms

(** Append (get-value ...) command to buffer if value_terms is non-empty *)
let append_get_value buf value_terms =
  match value_terms with
  | [] -> ()
  | terms ->
      Buffer.add_string buf
        (Printf.sprintf "(get-value (%s))\n" (String.concat " " terms))

type named_assertions = {
  buf : Buffer.t;
  mutable counter : int;
  mutable names : (string * string) list;
}
(** Named assertion accumulator for unsat-core diagnostics *)

let create_named_assertions buf = { buf; counter = 0; names = [] }

let add_named_assert na prefix human_text smt_expr =
  let aname = Printf.sprintf "%s_%d" prefix na.counter in
  na.counter <- na.counter + 1;
  na.names <- (aname, human_text) :: na.names;
  Buffer.add_string na.buf
    (Printf.sprintf "(assert (! %s :named %s))\n" smt_expr aname)

let get_assertion_names na = List.rev na.names

(** Add named type constraints for functions and action parameters *)
let add_type_constraints na config env (params : param list) =
  let type_exprs = collect_type_constraint_exprs config env in
  List.iter
    (fun (human, smt_expr) -> add_named_assert na "type" human smt_expr)
    type_exprs;
  List.iter
    (fun (p : param) ->
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok TyNat ->
          add_named_assert na "type"
            (Printf.sprintf "%s : Nat" (Ast.lower_name p.param_name))
            (Printf.sprintf "(>= %s 1)"
               (sanitize_ident (Ast.lower_name p.param_name)))
      | Ok TyNat0 ->
          add_named_assert na "type"
            (Printf.sprintf "%s : Nat0" (Ast.lower_name p.param_name))
            (Printf.sprintf "(>= %s 0)"
               (sanitize_ident (Ast.lower_name p.param_name)))
      | Ok
          ( TyBool | TyInt | TyReal | TyString | TyNothing | TyDomain _
          | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
      | Error _ ->
          ())
    params

(** Query 1: Contradiction detection for an action. Asserts all postconditions
    and checks satisfiability. UNSAT = contradiction found. *)
let declare_domain_membership config buf (params : param list) env =
  List.iter
    (fun (p : param) ->
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok (TyDomain name) ->
          let elems = domain_elements name (bound_for config name) in
          let sname = sanitize_ident (Ast.lower_name p.param_name) in
          let disj =
            List.map (fun e -> Printf.sprintf "(= %s %s)" sname e) elems
          in
          Buffer.add_string buf
            (Printf.sprintf "(assert (or %s))\n" (String.concat " " disj))
      | Ok
          ( TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing
          | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
      | Error _ ->
          ())
    params

let generate_contradiction_query config env action =
  let env = env_with_action_params env action.a_params in
  let buf = Buffer.create 1024 in
  let na = create_named_assertions buf in
  Buffer.add_string buf
    (Printf.sprintf "; Contradiction check: %s\n" action.a_label);
  Buffer.add_string buf "(set-option :produce-unsat-cores true)\n";
  Buffer.add_string buf
    (generate_preamble ~include_type_constraints:false config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  declare_domain_membership config buf action.a_params env;
  Buffer.add_string buf "\n; --- Type constraints ---\n";
  add_type_constraints na config env action.a_params;
  (* Named preconditions from guards *)
  let precond_exprs =
    List.filter_map
      (fun g -> match g with GExpr e -> Some e | GIn _ | GParam _ -> None)
      action.a_guards
  in
  List.iter
    (fun e ->
      add_named_assert na "precond" (Pretty.str_expr e)
        (translate_precondition config env e))
    precond_exprs;
  (* Named frame conditions *)
  let frame_exprs = collect_frame_exprs config env action.a_contexts in
  List.iter
    (fun (fname, smt_expr) ->
      add_named_assert na "frame"
        (Printf.sprintf "%s' = %s (frame)" fname fname)
        smt_expr)
    frame_exprs;
  (* Named postconditions — no guard injection in postconditions *)
  let post_config = { config with inject_guards = false } in
  Buffer.add_string buf "\n; --- Postconditions ---\n";
  List.iter
    (fun (p : expr located) ->
      add_named_assert na "postcond" (Pretty.str_expr p.value)
        (translate_proposition post_config env p.value))
    action.a_propositions;
  let value_terms = build_value_terms config env action.a_params in
  Buffer.add_string buf "(check-sat)\n";
  Buffer.add_string buf "(get-unsat-core)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "contradiction:%s" action.a_label;
    description =
      Printf.sprintf "Action '%s' postconditions are satisfiable" action.a_label;
    smt2 = Buffer.contents buf;
    kind = Contradiction;
    value_terms;
    invariant_text = "";
    assertion_names = get_assertion_names na;
  }

(** Query 2: Invariant preservation for a single (invariant, action) pair.
    Asserts all invariants in current state (to constrain pre-state), action
    transition, and negates only the single target invariant in the next state.
    SAT = violation found. *)
let generate_invariant_query config env ~all_invariants ~index
    (inv : expr located) action =
  let env = env_with_action_params env action.a_params in
  let buf = Buffer.create 1024 in
  let inv_text = Pretty.str_expr inv.value in
  Buffer.add_string buf
    (Printf.sprintf "; Invariant preservation: %s / %s\n" action.a_label
       inv_text);
  Buffer.add_string buf (generate_preamble ~constrain_primed:false config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  Buffer.add_string buf (declare_param_constraints env action.a_params);
  declare_domain_membership config buf action.a_params env;
  (* Assert all invariants hold in current state *)
  Buffer.add_string buf "\n; --- Invariants (current state) ---\n";
  let inv_current = conjoin_propositions config env all_invariants in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" inv_current);
  (* Assert preconditions *)
  let preconditions = extract_preconditions config env action.a_guards in
  List.iter
    (fun pc -> Buffer.add_string buf (Printf.sprintf "(assert %s)\n" pc))
    preconditions;
  (* Assert action postconditions (transition relation) — no guard injection *)
  let post_config = { config with inject_guards = false } in
  Buffer.add_string buf "\n; --- Action postconditions (transition) ---\n";
  let transition = conjoin_propositions post_config env action.a_propositions in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" transition);
  (* Assert frame conditions *)
  Buffer.add_string buf (generate_frame_conditions config env action.a_contexts);
  (* Assert single invariant violated in next state — guards injected *)
  Buffer.add_string buf "\n; --- Invariant violated in next state ---\n";
  let primed_inv = { inv with value = prime_expr inv.value } in
  let inv_primed = translate_proposition config env primed_inv.value in
  Buffer.add_string buf (Printf.sprintf "(assert (not %s))\n" inv_primed);
  let value_terms = build_value_terms config env action.a_params in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "invariant:%s:%d" action.a_label index;
    description =
      Printf.sprintf "Invariant preserved by action '%s'" action.a_label;
    smt2 = Buffer.contents buf;
    kind = InvariantPreservation;
    value_terms;
    invariant_text = inv_text;
    assertion_names = [];
  }

(** Query 3: Precondition satisfiability for an action. Asserts invariants +
    preconditions and checks satisfiability. UNSAT = dead operation. *)
let generate_precondition_query config env invariant_props action =
  let env = env_with_action_params env action.a_params in
  let buf = Buffer.create 1024 in
  let na = create_named_assertions buf in
  Buffer.add_string buf
    (Printf.sprintf "; Precondition satisfiability: %s\n" action.a_label);
  Buffer.add_string buf "(set-option :produce-unsat-cores true)\n";
  Buffer.add_string buf
    (generate_preamble ~include_type_constraints:false config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  declare_domain_membership config buf action.a_params env;
  Buffer.add_string buf "\n; --- Type constraints ---\n";
  add_type_constraints na config env action.a_params;
  (* Named invariants *)
  if invariant_props <> [] then begin
    Buffer.add_string buf "\n; --- Invariants ---\n";
    List.iter
      (fun (inv : expr located) ->
        add_named_assert na "inv"
          (Printf.sprintf "Invariant: %s" (Pretty.str_expr inv.value))
          (translate_proposition config env inv.value))
      invariant_props
  end;
  (* Named preconditions *)
  Buffer.add_string buf "\n; --- Preconditions ---\n";
  let precond_exprs =
    List.filter_map
      (fun g -> match g with GExpr e -> Some e | GIn _ | GParam _ -> None)
      action.a_guards
  in
  List.iter
    (fun e ->
      add_named_assert na "precond"
        (Printf.sprintf "Precondition: %s" (Pretty.str_expr e))
        (translate_precondition config env e))
    precond_exprs;
  Buffer.add_string buf "(check-sat)\n";
  Buffer.add_string buf "(get-unsat-core)\n";
  {
    name = Printf.sprintf "precondition:%s" action.a_label;
    description =
      Printf.sprintf "Action '%s' preconditions are satisfiable" action.a_label;
    smt2 = Buffer.contents buf;
    kind = PreconditionSat;
    value_terms = [];
    invariant_text = "";
    assertion_names = get_assertion_names na;
  }

(** Build SMT value terms for invariant consistency queries (no action params,
    no primed functions). Includes nullary rules and unary rules applied to
    domain elements. *)
let build_invariant_value_terms config env =
  Env.fold_terms
    (fun name entry acc ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KClosure (ty, _) -> (
          let sname = sanitize_ident name in
          match[@warning "-4"] decompose_func_ty ty with
          | Some ([], _ret) -> sname :: acc
          | Some ([ param_ty ], _ret) -> (
              match param_ty with
              | TyDomain dname ->
                  let elems = domain_elements dname (bound_for config dname) in
                  List.map
                    (fun elem -> Printf.sprintf "(%s %s)" sname elem)
                    elems
                  @ acc
              | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing
              | TyList _ | TyProduct _ | TySum _ | TyFunc _ ->
                  acc)
          | _ -> acc)
      | Env.KDomain | Env.KAlias _ | Env.KVar _ -> acc)
    env []

(** Query 0: Invariant consistency — checks that all invariants are jointly
    satisfiable. UNSAT = contradiction among invariants. *)
let generate_invariant_consistency_query config env invariant_props =
  let buf = Buffer.create 1024 in
  let na = create_named_assertions buf in
  Buffer.add_string buf "; Invariant consistency check\n";
  Buffer.add_string buf "(set-option :produce-unsat-cores true)\n";
  Buffer.add_string buf
    (generate_preamble ~constrain_primed:false ~include_type_constraints:false
       config env);
  Buffer.add_string buf "\n; --- Type constraints ---\n";
  let type_exprs =
    collect_type_constraint_exprs ~constrain_primed:false config env
  in
  List.iter
    (fun (human, smt_expr) -> add_named_assert na "type" human smt_expr)
    type_exprs;
  Buffer.add_string buf "\n; --- Invariants ---\n";
  List.iter
    (fun (inv : expr located) ->
      add_named_assert na "inv"
        (Printf.sprintf "Invariant: %s" (Pretty.str_expr inv.value))
        (translate_proposition config env inv.value))
    invariant_props;
  let value_terms = build_invariant_value_terms config env in
  Buffer.add_string buf "(check-sat)\n";
  Buffer.add_string buf "(get-unsat-core)\n";
  append_get_value buf value_terms;
  {
    name = "invariant-consistency";
    description = "Invariants are jointly satisfiable";
    smt2 = Buffer.contents buf;
    kind = InvariantConsistency;
    value_terms;
    invariant_text = "";
    assertion_names = get_assertion_names na;
  }

(** Query: Init consistency — checks that all init props + type constraints are
    jointly satisfiable. UNSAT = impossible initial state. *)
let generate_init_consistency_query config env init_props =
  let buf = Buffer.create 1024 in
  let na = create_named_assertions buf in
  Buffer.add_string buf "; Initial state consistency check\n";
  Buffer.add_string buf "(set-option :produce-unsat-cores true)\n";
  Buffer.add_string buf
    (generate_preamble ~constrain_primed:false ~include_type_constraints:false
       config env);
  Buffer.add_string buf "\n; --- Type constraints ---\n";
  let type_exprs =
    collect_type_constraint_exprs ~constrain_primed:false config env
  in
  List.iter
    (fun (human, smt_expr) -> add_named_assert na "type" human smt_expr)
    type_exprs;
  Buffer.add_string buf "\n; --- Initial state propositions ---\n";
  List.iter
    (fun (prop : expr located) ->
      add_named_assert na "init"
        (Printf.sprintf "Initially: %s" (Pretty.str_expr prop.value))
        (translate_proposition config env prop.value))
    init_props;
  let value_terms = build_invariant_value_terms config env in
  Buffer.add_string buf "(check-sat)\n";
  Buffer.add_string buf "(get-unsat-core)\n";
  append_get_value buf value_terms;
  {
    name = "init-consistency";
    description = "Initial state is satisfiable";
    smt2 = Buffer.contents buf;
    kind = InitConsistency;
    value_terms;
    invariant_text = "";
    assertion_names = get_assertion_names na;
  }

(** Query: Init satisfies invariant — for each invariant, checks init_props ∧
    ¬invariant. SAT = violation, UNSAT = ok. *)
let generate_init_invariant_query config env init_props ~index
    (inv : expr located) =
  let buf = Buffer.create 1024 in
  let inv_text = Pretty.str_expr inv.value in
  Buffer.add_string buf
    (Printf.sprintf "; Initial state satisfies invariant: %s\n" inv_text);
  Buffer.add_string buf (generate_preamble ~constrain_primed:false config env);
  Buffer.add_string buf
    (declare_type_constraints ~constrain_primed:false config env);
  (* Assert init props *)
  Buffer.add_string buf "\n; --- Initial state propositions ---\n";
  List.iter
    (fun (prop : expr located) ->
      let smt = translate_proposition config env prop.value in
      Buffer.add_string buf (Printf.sprintf "(assert %s)\n" smt))
    init_props;
  (* Negate the invariant *)
  Buffer.add_string buf "\n; --- Invariant negated ---\n";
  let inv_smt = translate_proposition config env inv.value in
  Buffer.add_string buf (Printf.sprintf "(assert (not %s))\n" inv_smt);
  let value_terms = build_invariant_value_terms config env in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "init-invariant:%d" index;
    description =
      Printf.sprintf "Invariant '%s' holds in initial state" inv_text;
    smt2 = Buffer.contents buf;
    kind = InitInvariant;
    value_terms;
    invariant_text = inv_text;
    assertion_names = [];
  }

(** Collect all rule names from env that should be renamed for BMC steps *)
let collect_rule_names env =
  Env.fold_terms
    (fun name entry acc ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KClosure (ty, _) -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with Some _ -> sname :: acc | None -> acc)
      | Env.KDomain | Env.KAlias _ | Env.KVar _ -> acc)
    env []

(** Rename all rule names in an SMT string for step [step]. Replaces
    [name_prime] → [name_s{step+1}] and [name] → [name_s{step}]. Sort
    substitutions by key length descending to avoid partial matches. *)
let rename_smt_for_step env smt step =
  let rule_names = collect_rule_names env in
  (* Build substitution pairs: longer keys first *)
  let subs =
    List.concat_map
      (fun name ->
        [
          (name ^ "_prime", Printf.sprintf "%s_s%d" name (step + 1));
          (name, Printf.sprintf "%s_s%d" name step);
        ])
      rule_names
  in
  let subs =
    List.sort
      (fun (a, _) (b, _) -> compare (String.length b) (String.length a))
      subs
  in
  List.fold_left (fun s (from, to_) -> replace_word ~from ~to_ s) smt subs

(** Declare k+1 copies of each rule function for BMC steps *)
let declare_step_functions config env steps =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "; --- Step-indexed function declarations ---\n";
  Env.iter_terms
    (fun name entry ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KClosure (ty, _) -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with
          | Some ([], ret) ->
              for i = 0 to steps do
                Buffer.add_string buf
                  (Printf.sprintf "(declare-const %s_s%d %s)\n" sname i
                     (sort_of_ty ret))
              done
          | Some (params, ret) ->
              let param_sorts =
                String.concat " " (List.map sort_of_ty params)
              in
              for i = 0 to steps do
                Buffer.add_string buf
                  (Printf.sprintf "(declare-fun %s_s%d (%s) %s)\n" sname i
                     param_sorts (sort_of_ty ret))
              done
          | None -> ())
      | Env.KDomain | Env.KAlias _ | Env.KVar _ -> ())
    env;
  (* Type constraints for each step *)
  Buffer.add_string buf "\n; --- Type constraints for all steps ---\n";
  let base_exprs =
    collect_type_constraint_exprs ~constrain_primed:false config env
  in
  for i = 0 to steps do
    List.iter
      (fun (_human, smt_expr) ->
        let renamed = rename_smt_for_step env smt_expr i in
        Buffer.add_string buf (Printf.sprintf "(assert %s)\n" renamed))
      base_exprs
  done;
  (* Closure axioms for each step — unprimed only, since step-indexing
     handles the next-state mapping via renaming *)
  let base_closure_axioms =
    generate_closure_axioms ~include_primed:false config env
  in
  if base_closure_axioms <> "" then begin
    Buffer.add_string buf "\n; --- Closure axioms for all steps ---\n";
    for i = 0 to steps do
      let renamed = rename_smt_for_step env base_closure_axioms i in
      Buffer.add_string buf renamed
    done
  end;
  Buffer.contents buf

(** Build SMT value terms for all BMC steps *)
let build_bmc_value_terms config env steps =
  let base_terms = build_invariant_value_terms config env in
  List.concat_map
    (fun i -> List.map (fun term -> rename_smt_for_step env term i) base_terms)
    (List.init (steps + 1) Fun.id)

(** Build the transition assertion for step i→i+1 as a disjunction over all
    actions. Each action's params are existentially quantified. *)
let build_step_transition config env actions step =
  let action_clauses =
    List.map
      (fun action ->
        (* Build existentially quantified clause for this action *)
        let param_bindings =
          List.map
            (fun (p : param) ->
              let sort =
                match resolve_param_sort env p.param_type with
                | Some s -> s
                | None ->
                    failwith
                      (Printf.sprintf
                         "SMT translation: cannot resolve sort for parameter \
                          '%s'"
                         (Ast.lower_name p.param_name))
              in
              Printf.sprintf "(%s %s)"
                (sanitize_ident (Ast.lower_name p.param_name))
                sort)
            action.a_params
        in
        (* Domain membership for action params *)
        let domain_conds =
          List.filter_map
            (fun (p : param) ->
              match Collect.resolve_type env p.param_type dummy_loc with
              | Ok (TyDomain name) ->
                  let elems = domain_elements name (bound_for config name) in
                  let sname = sanitize_ident (Ast.lower_name p.param_name) in
                  let disj =
                    List.map (fun e -> Printf.sprintf "(= %s %s)" sname e) elems
                  in
                  Some (Printf.sprintf "(or %s)" (String.concat " " disj))
              | Ok
                  ( TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString
                  | TyNothing | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
              | Error _ ->
                  None)
            action.a_params
        in
        (* Type constraints for action params *)
        let type_conds =
          List.filter_map
            (fun (p : param) ->
              match Collect.resolve_type env p.param_type dummy_loc with
              | Ok TyNat ->
                  Some
                    (Printf.sprintf "(>= %s 1)"
                       (sanitize_ident (Ast.lower_name p.param_name)))
              | Ok TyNat0 ->
                  Some
                    (Printf.sprintf "(>= %s 0)"
                       (sanitize_ident (Ast.lower_name p.param_name)))
              | Ok
                  ( TyBool | TyInt | TyReal | TyString | TyNothing | TyDomain _
                  | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
              | Error _ ->
                  None)
            action.a_params
        in
        (* Preconditions (from guards) — translated in base names *)
        let env_with_params = env_with_action_params env action.a_params in
        let precond_parts =
          extract_preconditions config env_with_params action.a_guards
        in
        (* Postconditions — no guard injection in postconditions *)
        let post_config = { config with inject_guards = false } in
        let postcond_parts =
          List.map
            (fun (p : expr located) ->
              translate_proposition post_config env_with_params p.value)
            action.a_propositions
        in
        (* Frame conditions *)
        let frame_parts =
          List.map snd (collect_frame_exprs config env action.a_contexts)
        in
        (* Rename: base name → _s{step}, prime → _s{step+1} *)
        let rename s = rename_smt_for_step env s step in
        let all_parts =
          domain_conds @ type_conds
          @ List.map rename precond_parts
          @ List.map rename postcond_parts
          @ List.map rename frame_parts
        in
        let all_parts_renamed = all_parts in
        let conj =
          match all_parts_renamed with
          | [] -> "true"
          | [ p ] -> p
          | ps -> Printf.sprintf "(and %s)" (String.concat " " ps)
        in
        match param_bindings with
        | [] -> conj
        | _ ->
            Printf.sprintf "(exists (%s) %s)"
              (String.concat " " param_bindings)
              conj)
      actions
  in
  match action_clauses with
  | [] -> "true"
  | [ c ] -> c
  | cs -> Printf.sprintf "(or %s)" (String.concat " " cs)

(** Generate a BMC query for a single invariant *)
let generate_bmc_query config env init_props actions ~inv_index
    (inv : expr located) ~steps =
  let buf = Buffer.create 2048 in
  let inv_text = Pretty.str_expr inv.value in
  Buffer.add_string buf
    (Printf.sprintf "; BMC check (%d steps): %s\n" steps inv_text);
  (* Domain sorts and composite types — same preamble but no function decls *)
  Buffer.add_string buf "; --- Domain sorts and elements ---\n";
  Buffer.add_string buf (declare_domain_sorts config env);
  Buffer.add_string buf "\n; --- Composite types ---\n";
  Buffer.add_string buf (declare_composite_types env);
  Buffer.add_string buf "\n";
  (* Declare step-indexed functions instead of regular ones *)
  Buffer.add_string buf (declare_step_functions config env steps);
  (* Assert init props constrain step 0 *)
  Buffer.add_string buf "\n; --- Initial state (step 0) ---\n";
  List.iter
    (fun (prop : expr located) ->
      let smt = translate_proposition config env prop.value in
      let renamed = rename_smt_for_step env smt 0 in
      Buffer.add_string buf (Printf.sprintf "(assert %s)\n" renamed))
    init_props;
  (* Assert transitions for each step i→i+1 *)
  for i = 0 to steps - 1 do
    Buffer.add_string buf
      (Printf.sprintf "\n; --- Transition step %d -> %d ---\n" i (i + 1));
    let transition = build_step_transition config env actions i in
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" transition)
  done;
  (* Assert invariant violated at some step *)
  Buffer.add_string buf "\n; --- Invariant violated at some step ---\n";
  let inv_smt = translate_proposition config env inv.value in
  let negated_steps =
    List.init (steps + 1) (fun i ->
        let renamed = rename_smt_for_step env inv_smt i in
        Printf.sprintf "(not %s)" renamed)
  in
  Buffer.add_string buf
    (Printf.sprintf "(assert (or %s))\n" (String.concat " " negated_steps));
  let value_terms = build_bmc_value_terms config env steps in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "bmc-invariant:%d" inv_index;
    description =
      Printf.sprintf "Invariant '%s' holds for %d steps from initial state"
        inv_text steps;
    smt2 = Buffer.contents buf;
    kind = BMCInvariant;
    value_terms;
    invariant_text = inv_text;
    assertion_names = [];
  }

(** Generate the "all actions disabled" assertion for a given step, using
    step-indexed function names. Each action's preconditions are universally
    quantified over parameters and negated. *)
let generate_all_actions_disabled config env actions step =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "\n; --- All actions disabled ---\n";
  List.iter
    (fun action ->
      let precond_exprs = extract_precondition_exprs action.a_guards in
      let param_bindings =
        List.map
          (fun (p : param) ->
            let sort =
              match resolve_param_sort env p.param_type with
              | Some s -> s
              | None ->
                  failwith
                    (Printf.sprintf
                       "SMT translation: cannot resolve sort for parameter '%s'"
                       (Ast.lower_name p.param_name))
            in
            Printf.sprintf "(%s %s)"
              (sanitize_ident (Ast.lower_name p.param_name))
              sort)
          action.a_params
      in
      let type_guards =
        List.filter_map
          (fun (p : param) ->
            match Collect.resolve_type env p.param_type dummy_loc with
            | Ok TyNat ->
                Some
                  (Printf.sprintf "(>= %s 1)"
                     (sanitize_ident (Ast.lower_name p.param_name)))
            | Ok TyNat0 ->
                Some
                  (Printf.sprintf "(>= %s 0)"
                     (sanitize_ident (Ast.lower_name p.param_name)))
            | Ok
                ( TyBool | TyInt | TyReal | TyString | TyNothing | TyDomain _
                | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
            | Error _ ->
                None)
          action.a_params
      in
      let env_with_params = env_with_action_params env action.a_params in
      let precond_strs =
        List.map
          (fun e ->
            rename_smt_for_step env
              (translate_precondition config env_with_params e)
              step)
          precond_exprs
      in
      let all_preconds =
        match precond_strs with
        | [ s ] -> s
        | ss -> Printf.sprintf "(and %s)" (String.concat " " ss)
      in
      let negated_body = Printf.sprintf "(not %s)" all_preconds in
      let body =
        match type_guards with
        | [] -> negated_body
        | _ ->
            Printf.sprintf "(=> (and %s) %s)"
              (String.concat " " type_guards)
              negated_body
      in
      match param_bindings with
      | [] ->
          Buffer.add_string buf
            (Printf.sprintf "; Action '%s' disabled\n(assert %s)\n"
               action.a_label negated_body)
      | _ ->
          Buffer.add_string buf
            (Printf.sprintf
               "; Action '%s' disabled\n(assert (forall (%s) %s))\n"
               action.a_label
               (String.concat " " param_bindings)
               body))
    actions;
  Buffer.contents buf

(** Query 5: BMC deadlock freedom — checks that no state reachable from the
    initial state within k steps has all actions disabled. SAT = reachable
    deadlock found, UNSAT = no deadlock within k steps. *)
let generate_bmc_deadlock_query config env init_props invariant_props actions
    ~steps =
  let buf = Buffer.create 2048 in
  Buffer.add_string buf
    (Printf.sprintf "; BMC deadlock check (%d steps)\n" steps);
  (* Domain sorts and composite types *)
  Buffer.add_string buf "; --- Domain sorts and elements ---\n";
  Buffer.add_string buf (declare_domain_sorts config env);
  Buffer.add_string buf "\n; --- Composite types ---\n";
  Buffer.add_string buf (declare_composite_types env);
  Buffer.add_string buf "\n";
  (* Declare step-indexed functions *)
  Buffer.add_string buf (declare_step_functions config env steps);
  (* Assert init props constrain step 0 *)
  Buffer.add_string buf "\n; --- Initial state (step 0) ---\n";
  List.iter
    (fun (prop : expr located) ->
      let smt = translate_proposition config env prop.value in
      let renamed = rename_smt_for_step env smt 0 in
      Buffer.add_string buf (Printf.sprintf "(assert %s)\n" renamed))
    init_props;
  (* Assert transitions for each step i→i+1 *)
  for i = 0 to steps - 1 do
    Buffer.add_string buf
      (Printf.sprintf "\n; --- Transition step %d -> %d ---\n" i (i + 1));
    let transition = build_step_transition config env actions i in
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" transition)
  done;
  (* Assert invariants hold at step k (deadlocked state must be valid) *)
  if invariant_props <> [] then begin
    Buffer.add_string buf
      (Printf.sprintf "\n; --- Invariants at step %d ---\n" steps);
    let inv_conj = conjoin_propositions config env invariant_props in
    let renamed = rename_smt_for_step env inv_conj steps in
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" renamed)
  end;
  (* Assert all actions disabled at step k *)
  Buffer.add_string buf (generate_all_actions_disabled config env actions steps);
  let value_terms = build_bmc_value_terms config env steps in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = "bmc-deadlock";
    description =
      Printf.sprintf "No reachable deadlock within %d steps from initial state"
        steps;
    smt2 = Buffer.contents buf;
    kind = BMCDeadlock;
    value_terms;
    invariant_text = "";
    assertion_names = [];
  }

type cond_info = {
  cond_arms : (expr * expr) list;
  cond_quantifiers : (param list * guard list) list;
      (** Enclosing quantifier bindings, outermost first *)
  cond_text : string;
}
(** Information about a cond expression found in the document *)

(** Recursively walk an expression collecting ECond nodes along with their
    enclosing quantifier context (params + guards). *)
let collect_conds_in_expr (e : expr) : cond_info list =
  let results = ref [] in
  let rec walk quant_ctx = function
    | ECond arms ->
        results :=
          {
            cond_arms = arms;
            cond_quantifiers = List.rev quant_ctx;
            cond_text = Pretty.str_expr (ECond arms);
          }
          :: !results;
        (* Also walk into arms and consequences for nested conds *)
        List.iter
          (fun (arm, cons) ->
            walk quant_ctx arm;
            walk quant_ctx cons)
          arms
    | EForall (mb, metas) | EExists (mb, metas) ->
        let ps, gs, body = Ast.unbind_quant mb metas in
        walk ((ps, gs) :: quant_ctx) body;
        List.iter
          (function
            | GExpr e -> walk quant_ctx e
            | GIn (_, e) -> walk quant_ctx e
            | GParam _ -> ())
          gs
    | EEach (mb, metas, _) ->
        let ps, gs, body = Ast.unbind_quant mb metas in
        walk ((ps, gs) :: quant_ctx) body;
        List.iter
          (function
            | GExpr e -> walk quant_ctx e
            | GIn (_, e) -> walk quant_ctx e
            | GParam _ -> ())
          gs
    | EBinop (_, e1, e2) ->
        walk quant_ctx e1;
        walk quant_ctx e2
    | EUnop (_, e1) -> walk quant_ctx e1
    | EApp (f, args) ->
        walk quant_ctx f;
        List.iter (walk quant_ctx) args
    | ETuple es -> List.iter (walk quant_ctx) es
    | EProj (e1, _) -> walk quant_ctx e1
    | EOverride (_, pairs) ->
        List.iter
          (fun (k, v) ->
            walk quant_ctx k;
            walk quant_ctx v)
          pairs
    | EInitially e1 -> walk quant_ctx e1
    | EVar _ | EDomain _ | EQualified _ | EPrimed _ | ELitBool _ | ELitNat _
    | ELitReal _ | ELitString _ ->
        ()
  in
  walk [] e;
  List.rev !results

(** Collect all cond expressions from classified chapters *)
let collect_conds chapters : cond_info list =
  List.concat_map
    (fun c ->
      let wrap_prop, props =
        match c with
        | Smt_doc.Invariant { head_bindings; propositions; checks } ->
            ( (fun (p : expr located) ->
                Smt_doc.bind_head_params head_bindings p),
              propositions @ checks )
        | Smt_doc.Action { params; guards; propositions; checks; _ } ->
            ( (fun (p : expr located) ->
                if params = [] && guards = [] then p
                else { p with value = Ast.make_forall params guards p.value }),
              propositions @ checks )
      in
      List.concat_map
        (fun (prop : expr located) ->
          collect_conds_in_expr (wrap_prop prop).value)
        props)
    chapters

(** Build value terms for a cond exhaustiveness query. Only includes functions
    that appear in the cond guard expressions, applied to domain elements from
    the enclosing quantifier bindings. *)
let build_cond_value_terms config env cond =
  let guard_exprs = List.map fst cond.cond_arms in
  let refs =
    List.concat_map collect_function_refs guard_exprs
    |> List.sort_uniq String.compare
  in
  (* Collect domain names from the enclosing quantifier params *)
  let quant_domains =
    List.concat_map
      (fun (params, _) ->
        List.filter_map
          (fun (p : param) ->
            match Collect.resolve_type env p.param_type dummy_loc with
            | Ok (TyDomain dname) -> Some dname
            | Ok
                ( TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString
                | TyNothing | TyList _ | TyProduct _ | TySum _ | TyFunc _ )
            | Error _ ->
                None)
          params)
      cond.cond_quantifiers
    |> List.sort_uniq String.compare
  in
  List.concat_map
    (fun name ->
      match[@warning "-4"] Env.lookup_term name env with
      | Some { kind = Env.KRule ty; _ }
      | Some { kind = Env.KClosure (ty, _); _ } -> (
          let sname = sanitize_ident name in
          match[@warning "-4"] decompose_func_ty ty with
          | Some ([], _) -> [ sname ]
          | Some ([ TyDomain dname ], _) when List.mem dname quant_domains ->
              let elems = domain_elements dname (bound_for config dname) in
              List.map (fun e -> Printf.sprintf "(%s %s)" sname e) elems
          | _ -> [])
      | _ -> [])
    refs

(** Generate exhaustiveness query for a single cond expression *)
let generate_exhaustiveness_query config env ~all_invariants:_ ~index cond =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "; Cond exhaustiveness check: %s\n" cond.cond_text);
  Buffer.add_string buf (generate_preamble ~constrain_primed:false config env);
  Buffer.add_string buf
    (declare_type_constraints ~constrain_primed:false config env);
  (* Build the disjunction of all arms *)
  let arm_exprs = List.map fst cond.cond_arms in
  let disj =
    match arm_exprs with
    | [ single ] -> single
    | first :: rest ->
        List.fold_right (fun arm acc -> EBinop (OpOr, arm, acc)) rest first
    | [] -> assert false
  in
  (* Wrap in surrounding quantifier bindings *)
  let wrapped =
    List.fold_right
      (fun (ps, gs) body -> Ast.make_forall ps gs body)
      cond.cond_quantifiers disj
  in
  (* Translate and negate *)
  let smt_exhaustive = translate_proposition config env wrapped in
  Buffer.add_string buf "\n; --- Exhaustiveness (negated) ---\n";
  Buffer.add_string buf (Printf.sprintf "(assert (not %s))\n" smt_exhaustive);
  let value_terms = build_cond_value_terms config env cond in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "cond-exhaustiveness:%d" index;
    description = Printf.sprintf "Cond arms are exhaustive: %s" cond.cond_text;
    smt2 = Buffer.contents buf;
    kind = CondExhaustiveness;
    value_terms;
    invariant_text = cond.cond_text;
    assertion_names = [];
  }

(** Entailment query for invariant chapters: assert all invariants, negate the
    check goal, test UNSAT. *)
let generate_invariant_entailment_query config env invariant_props ~index
    (goal : expr located) =
  let buf = Buffer.create 1024 in
  let goal_text = Pretty.str_expr goal.value in
  Buffer.add_string buf
    (Printf.sprintf "; Entailment check (invariant): %s\n" goal_text);
  Buffer.add_string buf (generate_preamble ~constrain_primed:false config env);
  (* Assert all invariants as assumptions *)
  if invariant_props <> [] then begin
    Buffer.add_string buf "\n; --- Invariants (assumptions) ---\n";
    let inv_conj = conjoin_propositions config env invariant_props in
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" inv_conj)
  end;
  (* Negate the check goal *)
  Buffer.add_string buf "\n; --- Negated check goal ---\n";
  let goal_smt = translate_proposition config env goal.value in
  Buffer.add_string buf (Printf.sprintf "(assert (not %s))\n" goal_smt);
  let value_terms = build_invariant_value_terms config env in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "entailment:invariant:%d" index;
    description = Printf.sprintf "Entailed: %s" goal_text;
    smt2 = Buffer.contents buf;
    kind = Entailment;
    value_terms;
    invariant_text = goal_text;
    assertion_names = [];
  }

(** Entailment query for action chapters: assert invariants + preconditions +
    postconditions + frame, negate the check goal, test UNSAT. *)
let generate_action_entailment_query config env ~all_invariants ~index
    (goal : expr located) action =
  let env = env_with_action_params env action.a_params in
  let buf = Buffer.create 1024 in
  let goal_text = Pretty.str_expr goal.value in
  Buffer.add_string buf
    (Printf.sprintf "; Entailment check (action %s): %s\n" action.a_label
       goal_text);
  Buffer.add_string buf (generate_preamble ~constrain_primed:false config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  Buffer.add_string buf (declare_param_constraints env action.a_params);
  declare_domain_membership config buf action.a_params env;
  (* Assert all invariants hold in current state *)
  if all_invariants <> [] then begin
    Buffer.add_string buf "\n; --- Invariants (current state) ---\n";
    let inv_current = conjoin_propositions config env all_invariants in
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" inv_current)
  end;
  (* Assert preconditions *)
  let preconditions = extract_preconditions config env action.a_guards in
  List.iter
    (fun pc -> Buffer.add_string buf (Printf.sprintf "(assert %s)\n" pc))
    preconditions;
  (* Assert action postconditions (transition relation) — no guard injection *)
  let post_config = { config with inject_guards = false } in
  Buffer.add_string buf "\n; --- Action postconditions (transition) ---\n";
  let transition = conjoin_propositions post_config env action.a_propositions in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" transition);
  (* Assert frame conditions *)
  Buffer.add_string buf (generate_frame_conditions config env action.a_contexts);
  (* Negate the check goal — no auto-priming, user writes primes explicitly *)
  Buffer.add_string buf "\n; --- Negated check goal ---\n";
  let goal_smt = translate_proposition config env goal.value in
  Buffer.add_string buf (Printf.sprintf "(assert (not %s))\n" goal_smt);
  let value_terms = build_value_terms config env action.a_params in
  Buffer.add_string buf "(check-sat)\n";
  append_get_value buf value_terms;
  {
    name = Printf.sprintf "entailment:%s:%d" action.a_label index;
    description =
      Printf.sprintf "Entailed by action '%s': %s" action.a_label goal_text;
    smt2 = Buffer.contents buf;
    kind = Entailment;
    value_terms;
    invariant_text = goal_text;
    assertion_names = [];
  }

(** Generate all verification queries for a document *)
let generate_queries config env (doc : document) =
  let chapters = classify_chapters doc in
  let invariants = collect_invariants chapters in
  let init_props = collect_initial_props chapters in
  let actions = collect_actions chapters in
  let queries = ref [] in
  let add f = queries := with_cond_aux f :: !queries in
  (* Invariant consistency query *)
  if invariants <> [] then
    add (fun () -> generate_invariant_consistency_query config env invariants);
  (* Init consistency query *)
  if init_props <> [] then
    add (fun () -> generate_init_consistency_query config env init_props);
  (* Init satisfies invariants queries *)
  if init_props <> [] && invariants <> [] then
    List.iteri
      (fun index inv ->
        add (fun () ->
            generate_init_invariant_query config env init_props ~index inv))
      invariants;
  (* Contradiction queries *)
  List.iter
    (fun action ->
      add (fun () -> generate_contradiction_query config env action))
    actions;
  (* Invariant preservation queries — one per (invariant, action) pair.
     Skip if invariant only touches extracontextual functions (trivially
     preserved by frame conditions). *)
  if invariants <> [] then
    List.iter
      (fun action ->
        List.iteri
          (fun index inv ->
            if invariant_touches_context env action.a_contexts [ inv ] then
              add (fun () ->
                  generate_invariant_query config env ~all_invariants:invariants
                    ~index inv action))
          invariants)
      actions;
  (* Precondition satisfiability queries *)
  List.iter
    (fun action ->
      add (fun () -> generate_precondition_query config env invariants action))
    actions;
  (* BMC deadlock freedom query — skip if any action has no preconditions
     (always enabled, so deadlock is impossible), or if no initial state *)
  let has_guarded_actions =
    actions <> [] && not (List.exists action_always_enabled actions)
  in
  if has_guarded_actions && init_props <> [] && config.steps > 0 then
    add (fun () ->
        generate_bmc_deadlock_query config env init_props invariants actions
          ~steps:config.steps);
  (* BMC queries — when init props, actions, and invariants all exist *)
  if init_props <> [] && actions <> [] && invariants <> [] && config.steps > 0
  then
    List.iteri
      (fun inv_index inv ->
        add (fun () ->
            generate_bmc_query config env init_props actions ~inv_index inv
              ~steps:config.steps))
      invariants;
  (* Cond exhaustiveness queries *)
  let conds = collect_conds chapters in
  List.iteri
    (fun index cond ->
      add (fun () ->
          generate_exhaustiveness_query config env ~all_invariants:invariants
            ~index cond))
    conds;
  (* Entailment queries for check blocks *)
  let all_checks = collect_checks chapters in
  List.iteri
    (fun index (check_expr, check_context) ->
      match check_context with
      | CheckInvariant chapter_props ->
          add (fun () ->
              generate_invariant_entailment_query config env chapter_props
                ~index check_expr)
      | CheckAction action ->
          add (fun () ->
              generate_action_entailment_query config env
                ~all_invariants:invariants ~index check_expr action))
    all_checks;
  List.rev !queries
