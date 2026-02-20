(** SMT-LIB2 translation for bounded model checking *)

open Ast
open Types

(** Configuration for bounded checking *)
type config = { bound : int }

(** A generated SMT query with metadata *)
type query = {
  name : string;
  description : string;
  smt2 : string;
  kind : query_kind;
}

and query_kind =
  | Contradiction  (** SAT = ok, UNSAT = contradiction found *)
  | InvariantPreservation
      (** SAT = violation (counterexample), UNSAT = preserved *)
  | PreconditionSat  (** SAT = ok, UNSAT = dead operation *)

(** SMT sort name for a Pantagruel type *)
let rec sort_of_ty = function
  | TyBool -> "Bool"
  | TyNat | TyNat0 | TyInt -> "Int"
  | TyReal -> "Real"
  | TyString -> "String"
  | TyNothing -> "Int" (* bottom type, never instantiated *)
  | TyDomain name -> name
  | TyList inner -> Printf.sprintf "(Array Int %s)" (sort_of_ty inner)
  | TyProduct ts ->
      let name = product_sort_name ts in
      name
  | TySum ts ->
      let name = sum_sort_name ts in
      name
  | TyFunc _ -> "Int" (* functions are declared separately, not as sorts *)

and product_sort_name ts =
  "Pair_" ^ String.concat "_" (List.map sort_base_name ts)

and sum_sort_name ts =
  "Sum_" ^ String.concat "_" (List.map sort_base_name ts)

and sort_base_name = function
  | TyBool -> "Bool"
  | TyNat | TyNat0 | TyInt -> "Int"
  | TyReal -> "Real"
  | TyString -> "String"
  | TyNothing -> "Nothing"
  | TyDomain name -> name
  | TyList inner -> "List_" ^ sort_base_name inner
  | TyProduct ts -> product_sort_name ts
  | TySum ts -> sum_sort_name ts
  | TyFunc _ -> "Func"

(** Generate domain element names *)
let domain_elements name bound =
  List.init bound (fun i -> Printf.sprintf "%s_%d" name i)

(** Sanitize an identifier for SMT-LIB2 (replace hyphens, question marks) *)
let sanitize_ident name =
  name
  |> String.to_seq
  |> Seq.map (fun c ->
         match c with
         | '-' -> '_'
         | '?' -> 'p'
         | _ -> c)
  |> String.of_seq

(** Generate sort declarations for user-defined domains *)
let declare_domain_sorts config env =
  let buf = Buffer.create 256 in
  Env.StringMap.iter
    (fun name entry ->
      match entry.Env.kind with
      | Env.KDomain ->
          let elems = domain_elements name config.bound in
          Buffer.add_string buf
            (Printf.sprintf "(declare-sort %s 0)\n" name);
          List.iter
            (fun elem ->
              Buffer.add_string buf
                (Printf.sprintf "(declare-const %s %s)\n" elem name))
            elems;
          if List.length elems > 1 then
            Buffer.add_string buf
              (Printf.sprintf "(assert (distinct %s))\n"
                 (String.concat " " elems))
      | _ -> ())
    env.Env.types;
  Buffer.contents buf

(** Collect all product/sum types used in the environment for datatype declarations *)
let collect_composite_types env =
  let products = Hashtbl.create 8 in
  let sums = Hashtbl.create 8 in
  let rec visit = function
    | TyProduct ts ->
        Hashtbl.replace products (product_sort_name ts) ts;
        List.iter visit ts
    | TySum ts ->
        Hashtbl.replace sums (sum_sort_name ts) ts;
        List.iter visit ts
    | TyList inner -> visit inner
    | TyFunc (params, ret) ->
        List.iter visit params;
        Option.iter visit ret
    | _ -> ()
  in
  Env.StringMap.iter
    (fun _ entry ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KVar ty | Env.KAlias ty -> visit ty
      | Env.KDomain -> ())
    env.Env.terms;
  Env.StringMap.iter
    (fun _ entry ->
      match entry.Env.kind with
      | Env.KAlias ty -> visit ty
      | _ -> ())
    env.Env.types;
  (products, sums)

(** Generate datatype declarations for product and sum types *)
let declare_composite_types env =
  let products, sums = collect_composite_types env in
  let buf = Buffer.create 256 in
  Hashtbl.iter
    (fun name ts ->
      let fields =
        List.mapi
          (fun i t ->
            Printf.sprintf "(fst_%d %s)" (i + 1) (sort_of_ty t))
          ts
      in
      Buffer.add_string buf
        (Printf.sprintf "(declare-datatype %s ((%s %s)))\n" name
           (Printf.sprintf "mk_%s" name)
           (String.concat " " fields)))
    products;
  Hashtbl.iter
    (fun name ts ->
      let constructors =
        List.mapi
          (fun i t ->
            Printf.sprintf "(mk_%s_%d (val_%s_%d %s))" name i name i
              (sort_of_ty t))
          ts
      in
      Buffer.add_string buf
        (Printf.sprintf "(declare-datatype %s (%s))\n" name
           (String.concat " " constructors)))
    sums;
  Buffer.contents buf

(** Extract the parameter types and return type from a rule type *)
let decompose_func_ty = function
  | TyFunc (params, Some ret) -> Some (params, ret)
  | _ -> None

(** Generate function declarations from the environment *)
let declare_functions env =
  let buf = Buffer.create 256 in
  Env.StringMap.iter
    (fun name entry ->
      match entry.Env.kind with
      | Env.KRule ty -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with
          | Some ([], ret) ->
              Buffer.add_string buf
                (Printf.sprintf "(declare-const %s %s)\n" sname
                   (sort_of_ty ret));
              Buffer.add_string buf
                (Printf.sprintf "(declare-const %s_prime %s)\n" sname
                   (sort_of_ty ret))
          | Some (params, ret) ->
              let param_sorts =
                String.concat " " (List.map sort_of_ty params)
              in
              Buffer.add_string buf
                (Printf.sprintf "(declare-fun %s (%s) %s)\n" sname param_sorts
                   (sort_of_ty ret));
              Buffer.add_string buf
                (Printf.sprintf "(declare-fun %s_prime (%s) %s)\n" sname
                   param_sorts (sort_of_ty ret))
          | None -> ())
      | _ -> ())
    env.Env.terms;
  Buffer.contents buf

(** Generate type constraints (e.g., Nat >= 1, Nat0 >= 0) for functions.
    When [constrain_primed] is false, primed functions are unconstrained. *)
let declare_type_constraints ?(constrain_primed = true) _config env =
  let buf = Buffer.create 256 in
  let add_nat_constraint sname params_sorts param_names ret_ty is_prime =
    let fname = if is_prime then sname ^ "_prime" else sname in
    match ret_ty with
    | TyNat ->
        if params_sorts = [] then
          Buffer.add_string buf
            (Printf.sprintf "(assert (>= %s 1))\n" fname)
        else begin
          (* For all domain elements, assert >= 1 *)
          Buffer.add_string buf
            (Printf.sprintf "(assert (forall (%s) (>= (%s %s) 1)))\n"
               (String.concat " "
                  (List.map2
                     (fun n s -> Printf.sprintf "(%s %s)" n s)
                     param_names params_sorts))
               fname
               (String.concat " " param_names))
        end
    | TyNat0 ->
        if params_sorts = [] then
          Buffer.add_string buf
            (Printf.sprintf "(assert (>= %s 0))\n" fname)
        else begin
          Buffer.add_string buf
            (Printf.sprintf "(assert (forall (%s) (>= (%s %s) 0)))\n"
               (String.concat " "
                  (List.map2
                     (fun n s -> Printf.sprintf "(%s %s)" n s)
                     param_names params_sorts))
               fname
               (String.concat " " param_names))
        end
    | _ -> ()
  in
  Env.StringMap.iter
    (fun name entry ->
      match entry.Env.kind with
      | Env.KRule ty -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with
          | Some (params, ret) ->
              let params_sorts = List.map sort_of_ty params in
              let param_names =
                List.mapi (fun i _ -> Printf.sprintf "x_%d" i) params
              in
              add_nat_constraint sname params_sorts param_names ret false;
              if constrain_primed then
                add_nat_constraint sname params_sorts param_names ret true
          | None -> ())
      | _ -> ())
    env.Env.terms;
  Buffer.contents buf

(** Generate the full preamble: sorts, functions, domain elements, constraints.
    When [constrain_primed] is false, primed functions are left unconstrained
    (used for invariant preservation queries where we want to check if the
    transition can violate type/invariant bounds). *)
let generate_preamble ?(constrain_primed = true) config env =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "; --- Domain sorts and elements ---\n";
  Buffer.add_string buf (declare_domain_sorts config env);
  Buffer.add_string buf "\n; --- Composite types ---\n";
  Buffer.add_string buf (declare_composite_types env);
  Buffer.add_string buf "\n; --- Function declarations ---\n";
  Buffer.add_string buf (declare_functions env);
  Buffer.add_string buf "\n; --- Type constraints ---\n";
  Buffer.add_string buf (declare_type_constraints ~constrain_primed config env);
  Buffer.contents buf

(** Translate an expression to SMT-LIB2 term string *)
let rec translate_expr config env (e : expr) =
  match e with
  | ELitBool true -> "true"
  | ELitBool false -> "false"
  | ELitNat n -> string_of_int n
  | ELitReal f -> Printf.sprintf "%.17g" f
  | ELitString s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | EVar name -> sanitize_ident name
  | EDomain name ->
      (* Domain as a set: shouldn't appear standalone in SMT, handled in context *)
      Printf.sprintf "<<domain:%s>>" name
  | EQualified (_mod, name) -> sanitize_ident name
  | EPrimed name -> sanitize_ident name ^ "_prime"
  | EApp (func, args) -> translate_app config env func args
  | ETuple exprs ->
      let ts = List.map (translate_expr config env) exprs in
      (* Infer component types to build the correct constructor name *)
      let component_sorts =
        List.map
          (fun sub_e ->
            match Check.infer_type { Check.env; loc = dummy_loc } sub_e with
            | Ok ty -> sort_base_name ty
            | Error _ -> "Int")
          exprs
      in
      let ctor = "mk_Pair_" ^ String.concat "_" component_sorts in
      Printf.sprintf "(%s %s)" ctor (String.concat " " ts)
  | EProj (e, idx) ->
      Printf.sprintf "(fst_%d %s)" idx (translate_expr config env e)
  | EBinop (op, e1, e2) -> translate_binop config env op e1 e2
  | EUnop (op, e) -> translate_unop config env op e
  | EForall (params, guards, body) ->
      translate_quantifier config env "forall" params guards body
  | EExists (params, guards, body) ->
      translate_quantifier config env "exists" params guards body
  | EOverride (name, pairs) -> translate_override config env name pairs

and translate_app config env func args =
  match func with
  | EOverride (name, pairs) ->
      (* f[k |-> v] applied to args: inline ite chain *)
      let sname = sanitize_ident name in
      let args_str = List.map (translate_expr config env) args in
      let applied_args = String.concat " " args_str in
      (* For arity-1 overrides, the single arg is the dispatch key *)
      let arg_str = List.hd args_str in
      let rec build_chain = function
        | [] -> Printf.sprintf "(%s %s)" sname applied_args
        | (k, v) :: rest ->
            Printf.sprintf "(ite (= %s %s) %s %s)" arg_str
              (translate_expr config env k)
              (translate_expr config env v)
              (build_chain rest)
      in
      build_chain pairs
  | _ ->
      let func_str =
        match func with
        | EVar name -> sanitize_ident name
        | EPrimed name -> sanitize_ident name ^ "_prime"
        | _ -> translate_expr config env func
      in
      let args_str = List.map (translate_expr config env) args in
      Printf.sprintf "(%s %s)" func_str (String.concat " " args_str)

and translate_binop config env op e1 e2 =
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
  | OpIn -> translate_in config env e1 e2
  | OpSubset ->
      (* forall x . (a x) => (b x) — simplified for now *)
      Printf.sprintf "(forall ((x_sub Int)) (=> %s %s))" t1 t2

and translate_in config _env elem set =
  match set with
  | EDomain name ->
      (* x in Domain → disjunction over domain elements *)
      let elems = domain_elements name config.bound in
      let elem_str = translate_expr config _env elem in
      let disj =
        List.map (fun e -> Printf.sprintf "(= %s %s)" elem_str e) elems
      in
      Printf.sprintf "(or %s)" (String.concat " " disj)
  | _ ->
      (* x in xs where xs is a list expression *)
      let elem_str = translate_expr config _env elem in
      let set_str = translate_expr config _env set in
      Printf.sprintf "(select %s %s)" set_str elem_str

and translate_unop config env op e =
  match op with
  | OpNot -> Printf.sprintf "(not %s)" (translate_expr config env e)
  | OpNeg -> Printf.sprintf "(- %s)" (translate_expr config env e)
  | OpCard -> translate_card config env e

and translate_card config env e =
  match e with
  | EDomain _ ->
      (* #Domain = bound (all elements exist) *)
      string_of_int config.bound
  | _ ->
      (* For list/set expressions, cardinality is not directly expressible
         without knowing the domain. Use an uninterpreted function as fallback. *)
      Printf.sprintf "(card %s)" (translate_expr config env e)

and translate_quantifier config env quant params guards body =
  (* Collect bindings and guard conditions *)
  let bindings =
    List.map
      (fun (p : param) ->
        let sort =
          match resolve_param_sort env p.param_type with
          | Some s -> s
          | None -> "Int"
        in
        Printf.sprintf "(%s %s)" (sanitize_ident p.param_name) sort)
      params
  in
  let guard_bindings, guard_conditions =
    List.fold_left
      (fun (binds, conds) g ->
        match g with
        | GParam p ->
            let sort =
              match resolve_param_sort env p.param_type with
              | Some s -> s
              | None -> "Int"
            in
            ( Printf.sprintf "(%s %s)" (sanitize_ident p.param_name) sort
              :: binds,
              conds )
        | GIn (name, list_expr) ->
            (* Bind name as element type; resolve from list expression type *)
            let elem_sort =
              match
                Check.infer_type { Check.env; loc = dummy_loc } list_expr
              with
              | Ok (TyList elem_ty) -> sort_of_ty elem_ty
              | _ -> "Int"
            in
            let guard_str =
              translate_in config env (EVar name) list_expr
            in
            ( Printf.sprintf "(%s %s)" (sanitize_ident name) elem_sort :: binds,
              guard_str :: conds )
        | GExpr e -> (binds, translate_expr config env e :: conds))
      ([], []) guards
  in
  let all_bindings = bindings @ List.rev guard_bindings in
  let body_str = translate_expr config env body in
  let conditions = List.rev guard_conditions in
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

and translate_override _config _env name _pairs =
  (* Standalone override (not applied) — can't be directly represented in
     SMT-LIB2 without higher-order functions. Applied overrides are handled
     in translate_app. *)
  Printf.sprintf "<<override:%s>>" (sanitize_ident name)

and resolve_param_sort env te =
  match Collect.resolve_type env te dummy_loc with
  | Ok ty -> Some (sort_of_ty ty)
  | Error _ -> None

(** Classify chapters into invariant chapters and action chapters *)
type chapter_class =
  | Invariant of expr located list  (** Non-action body propositions *)
  | Action of {
      label : string;
      params : param list;
      guards : guard list;
      context : string option;
      propositions : expr located list;
    }

let classify_chapter (chapter : chapter) =
  let action =
    List.find_map
      (fun (decl : declaration located) ->
        match decl.value with
        | DeclAction { label; params; guards; context } ->
            Some (label, params, guards, context)
        | _ -> None)
      chapter.head
  in
  match action with
  | Some (label, params, guards, context) ->
      Action
        {
          label;
          params;
          guards;
          context;
          propositions = chapter.body;
        }
  | None -> Invariant chapter.body

let classify_chapters (doc : document) =
  List.map classify_chapter doc.chapters

(** Collect all invariants from the document *)
let collect_invariants chapters =
  List.concat_map
    (fun c -> match c with Invariant props -> props | Action _ -> [])
    chapters

(** Collect all actions from the document *)
type action_info = {
  a_label : string;
  a_params : param list;
  a_guards : guard list;
  a_context : string option;
  a_propositions : expr located list;
}

let collect_actions chapters =
  List.filter_map
    (fun c ->
      match c with
      | Action { label; params; guards; context; propositions } ->
          Some
            {
              a_label = label;
              a_params = params;
              a_guards = guards;
              a_context = context;
              a_propositions = propositions;
            }
      | Invariant _ -> None)
    chapters

(** Translate a list of propositions into a conjunction *)
let conjoin_propositions config env props =
  match props with
  | [] -> "true"
  | [ p ] -> translate_expr config env p.value
  | _ ->
      let parts =
        List.map (fun (p : expr located) -> translate_expr config env p.value) props
      in
      Printf.sprintf "(and %s)" (String.concat " " parts)

(** Substitute primed names in an expression (for invariant checking in next state).
    Tracks locally-bound names (from quantifiers) to avoid priming them. *)
let rec prime_expr ?(bound = []) (e : expr) : expr =
  match e with
  | EVar name ->
      if List.mem name bound then e else EPrimed name
  | EApp (func, args) ->
      EApp (prime_expr ~bound func, List.map (prime_expr ~bound) args)
  | EBinop (op, e1, e2) ->
      EBinop (op, prime_expr ~bound e1, prime_expr ~bound e2)
  | EUnop (op, e) -> EUnop (op, prime_expr ~bound e)
  | ETuple es -> ETuple (List.map (prime_expr ~bound) es)
  | EProj (e, i) -> EProj (prime_expr ~bound e, i)
  | EForall (ps, gs, body) ->
      let bound' = List.map (fun (p : param) -> p.param_name) ps @ bound in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      EForall (ps, gs', prime_expr ~bound:bound'' body)
  | EExists (ps, gs, body) ->
      let bound' = List.map (fun (p : param) -> p.param_name) ps @ bound in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      EExists (ps, gs', prime_expr ~bound:bound'' body)
  | EOverride (name, pairs) ->
      EOverride
        ( name,
          List.map
            (fun (k, v) -> (prime_expr ~bound k, prime_expr ~bound v))
            pairs )
  | EPrimed _ | ELitBool _ | ELitNat _ | ELitReal _ | ELitString _
  | EDomain _ | EQualified _ ->
      e

and prime_guards ~bound gs =
  List.fold_left
    (fun (bound, acc) g ->
      match g with
      | GParam p -> (p.param_name :: bound, acc @ [ GParam p ])
      | GIn (name, e) ->
          (name :: bound, acc @ [ GIn (name, prime_expr ~bound e) ])
      | GExpr e -> (bound, acc @ [ GExpr (prime_expr ~bound e) ]))
    (bound, []) gs

(** Generate frame conditions: for every rule NOT in the action's context,
    assert f_prime = f. Contextual functions are left free — constrained
    only by the action's postconditions. *)
let generate_frame_conditions _config env context =
  match context with
  | None -> "" (* No context = no frame conditions *)
  | Some ctx_name -> (
      match Env.lookup_context ctx_name env with
      | None -> ""
      | Some context_members ->
          let buf = Buffer.create 256 in
          Buffer.add_string buf "; --- Frame conditions ---\n";
          Env.StringMap.iter
            (fun name entry ->
              match entry.Env.kind with
              | Env.KRule ty when not (List.mem name context_members) -> (
                  let sname = sanitize_ident name in
                  match decompose_func_ty ty with
                  | Some ([], _ret) ->
                      Buffer.add_string buf
                        (Printf.sprintf "(assert (= %s_prime %s))\n" sname
                           sname)
                  | Some (params, _ret) ->
                      let param_sorts = List.map sort_of_ty params in
                      let param_names =
                        List.mapi
                          (fun i _ -> Printf.sprintf "frame_x_%d" i)
                          params
                      in
                      let bindings =
                        List.map2
                          (fun n s -> Printf.sprintf "(%s %s)" n s)
                          param_names param_sorts
                      in
                      let args = String.concat " " param_names in
                      Buffer.add_string buf
                        (Printf.sprintf
                           "(assert (forall (%s) (= (%s_prime %s) (%s \
                            %s))))\n"
                           (String.concat " " bindings)
                           sname args sname args)
                  | None -> ())
              | _ -> ())
            env.Env.terms;
          Buffer.contents buf)

(** Extract preconditions from action guards (non-binding boolean conditions) *)
let extract_preconditions config env (guards : guard list) =
  List.filter_map
    (fun g ->
      match g with
      | GExpr e -> Some (translate_expr config env e)
      | _ -> None)
    guards

(** Generate parameter declarations for an action *)
let declare_action_params env (params : param list) =
  let buf = Buffer.create 128 in
  List.iter
    (fun (p : param) ->
      let sort =
        match resolve_param_sort env p.param_type with
        | Some s -> s
        | None -> "Int"
      in
      Buffer.add_string buf
        (Printf.sprintf "(declare-const %s %s)\n"
           (sanitize_ident p.param_name) sort))
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
               (sanitize_ident p.param_name))
      | Ok TyNat0 ->
          Buffer.add_string buf
            (Printf.sprintf "(assert (>= %s 0))\n"
               (sanitize_ident p.param_name))
      | _ -> ())
    params;
  Buffer.contents buf

(** Query 1: Contradiction detection for an action.
    Asserts all postconditions and checks satisfiability.
    UNSAT = contradiction found. *)
let declare_domain_membership config buf (params : param list) env =
  List.iter
    (fun (p : param) ->
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok (TyDomain name) ->
          let elems = domain_elements name config.bound in
          let sname = sanitize_ident p.param_name in
          let disj =
            List.map (fun e -> Printf.sprintf "(= %s %s)" sname e) elems
          in
          Buffer.add_string buf
            (Printf.sprintf "(assert (or %s))\n" (String.concat " " disj))
      | _ -> ())
    params

let generate_contradiction_query config env action =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "; Contradiction check: %s\n" action.a_label);
  Buffer.add_string buf (generate_preamble config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  Buffer.add_string buf (declare_param_constraints env action.a_params);
  declare_domain_membership config buf action.a_params env;
  (* Assert preconditions from guards *)
  let preconditions = extract_preconditions config env action.a_guards in
  List.iter
    (fun pc -> Buffer.add_string buf (Printf.sprintf "(assert %s)\n" pc))
    preconditions;
  (* Assert frame conditions *)
  Buffer.add_string buf (generate_frame_conditions config env action.a_context);
  (* Assert all postconditions *)
  Buffer.add_string buf "\n; --- Postconditions ---\n";
  let conj = conjoin_propositions config env action.a_propositions in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" conj);
  Buffer.add_string buf "(check-sat)\n";
  {
    name = Printf.sprintf "contradiction:%s" action.a_label;
    description =
      Printf.sprintf "Action '%s' postconditions are satisfiable" action.a_label;
    smt2 = Buffer.contents buf;
    kind = Contradiction;
  }

(** Query 2: Invariant preservation for an (invariant, action) pair.
    Asserts invariant in current state, action transition, and negated invariant
    in next state. SAT = violation found. *)
let generate_invariant_query config env invariant_props action =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "; Invariant preservation: %s\n" action.a_label);
  Buffer.add_string buf (generate_preamble ~constrain_primed:false config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  Buffer.add_string buf (declare_param_constraints env action.a_params);
  declare_domain_membership config buf action.a_params env;
  (* Assert invariant holds in current state *)
  Buffer.add_string buf "\n; --- Invariant (current state) ---\n";
  let inv_current = conjoin_propositions config env invariant_props in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" inv_current);
  (* Assert preconditions *)
  let preconditions = extract_preconditions config env action.a_guards in
  List.iter
    (fun pc -> Buffer.add_string buf (Printf.sprintf "(assert %s)\n" pc))
    preconditions;
  (* Assert action postconditions (transition relation) *)
  Buffer.add_string buf "\n; --- Action postconditions (transition) ---\n";
  let transition = conjoin_propositions config env action.a_propositions in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" transition);
  (* Assert frame conditions *)
  Buffer.add_string buf (generate_frame_conditions config env action.a_context);
  (* Assert invariant violated in next state *)
  Buffer.add_string buf "\n; --- Invariant violated in next state ---\n";
  let primed_invariants =
    List.map
      (fun (p : expr located) ->
        { p with value = prime_expr p.value })
      invariant_props
  in
  let inv_primed = conjoin_propositions config env primed_invariants in
  Buffer.add_string buf (Printf.sprintf "(assert (not %s))\n" inv_primed);
  Buffer.add_string buf "(check-sat)\n";
  {
    name = Printf.sprintf "invariant:%s" action.a_label;
    description =
      Printf.sprintf "Invariant preserved by action '%s'" action.a_label;
    smt2 = Buffer.contents buf;
    kind = InvariantPreservation;
  }

(** Query 3: Precondition satisfiability for an action.
    Asserts invariants + preconditions and checks satisfiability.
    UNSAT = dead operation. *)
let generate_precondition_query config env invariant_props action =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf "; Precondition satisfiability: %s\n" action.a_label);
  Buffer.add_string buf (generate_preamble config env);
  Buffer.add_string buf "\n; --- Action parameters ---\n";
  Buffer.add_string buf (declare_action_params env action.a_params);
  Buffer.add_string buf (declare_param_constraints env action.a_params);
  declare_domain_membership config buf action.a_params env;
  (* Assert invariants *)
  if invariant_props <> [] then begin
    Buffer.add_string buf "\n; --- Invariants ---\n";
    let inv = conjoin_propositions config env invariant_props in
    Buffer.add_string buf (Printf.sprintf "(assert %s)\n" inv)
  end;
  (* Assert preconditions *)
  Buffer.add_string buf "\n; --- Preconditions ---\n";
  let preconditions = extract_preconditions config env action.a_guards in
  List.iter
    (fun pc -> Buffer.add_string buf (Printf.sprintf "(assert %s)\n" pc))
    preconditions;
  Buffer.add_string buf "(check-sat)\n";
  {
    name = Printf.sprintf "precondition:%s" action.a_label;
    description =
      Printf.sprintf "Action '%s' preconditions are satisfiable" action.a_label;
    smt2 = Buffer.contents buf;
    kind = PreconditionSat;
  }

(** Collect all function names referenced in an expression *)
let rec collect_function_refs (e : expr) =
  match e with
  | EVar name -> [ name ]
  | EApp (func, args) ->
      collect_function_refs func @ List.concat_map collect_function_refs args
  | EPrimed name -> [ name ]
  | EBinop (_, e1, e2) ->
      collect_function_refs e1 @ collect_function_refs e2
  | EUnop (_, e) -> collect_function_refs e
  | EForall (_, gs, body) | EExists (_, gs, body) ->
      List.concat_map collect_guard_refs gs @ collect_function_refs body
  | ETuple es -> List.concat_map collect_function_refs es
  | EProj (e, _) -> collect_function_refs e
  | EOverride (name, pairs) ->
      name
      :: List.concat_map
           (fun (k, v) -> collect_function_refs k @ collect_function_refs v)
           pairs
  | ELitBool _ | ELitNat _ | ELitReal _ | ELitString _ | EDomain _
  | EQualified _ ->
      []

and collect_guard_refs = function
  | GParam _ -> []
  | GIn (_, e) -> collect_function_refs e
  | GExpr e -> collect_function_refs e

(** Check if invariant propositions touch any contextual functions.
    If they only reference extracontextual functions, the frame conditions
    guarantee preservation and we can skip the check. *)
let invariant_touches_context env context invariant_props =
  match context with
  | None -> true (* No context = check everything *)
  | Some ctx_name -> (
      match Env.lookup_context ctx_name env with
      | None -> true
      | Some context_members ->
          let refs =
            List.concat_map
              (fun (p : expr located) -> collect_function_refs p.value)
              invariant_props
          in
          List.exists (fun r -> List.mem r context_members) refs)

(** Generate all verification queries for a document *)
let generate_queries config env (doc : document) =
  let chapters = classify_chapters doc in
  let invariants = collect_invariants chapters in
  let actions = collect_actions chapters in
  let queries = ref [] in
  (* Contradiction queries *)
  List.iter
    (fun action ->
      queries :=
        generate_contradiction_query config env action :: !queries)
    actions;
  (* Invariant preservation queries — skip if invariant only touches
     extracontextual functions (trivially preserved by frame conditions) *)
  if invariants <> [] then
    List.iter
      (fun action ->
        if invariant_touches_context env action.a_context invariants then
          queries :=
            generate_invariant_query config env invariants action
            :: !queries)
      actions;
  (* Precondition satisfiability queries *)
  List.iter
    (fun action ->
      queries :=
        generate_precondition_query config env invariants action
        :: !queries)
    actions;
  List.rev !queries
