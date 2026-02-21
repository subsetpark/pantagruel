(** SMT-LIB2 translation for bounded model checking *)

open Ast
open Types

type config = { bound : int; steps : int; domain_bounds : int Env.StringMap.t }
(** Configuration for bounded checking. [steps] controls k-step BMC depth.
    [domain_bounds] maps domain names to per-domain minimum bounds (derived from
    nullary constant counts). *)

(** Compute per-domain minimum bounds by counting nullary constants. For each
    domain, the bound is max(default_bound, number_of_nullary_constants). *)
let compute_domain_bounds default_bound env =
  let counts =
    Env.StringMap.fold
      (fun _name entry acc ->
        match entry.Env.kind with
        | Env.KRule ty -> (
            match ty with
            | (TyFunc ([], Some (TyDomain dname)) | TyDomain dname)
              when Env.StringMap.find_opt dname env.Env.types
                   |> Option.map (fun e -> e.Env.kind = Env.KDomain)
                   |> Option.value ~default:false ->
                let cur =
                  Env.StringMap.find_opt dname acc |> Option.value ~default:0
                in
                Env.StringMap.add dname (cur + 1) acc
            | _ -> acc)
        | _ -> acc)
      env.Env.terms Env.StringMap.empty
  in
  Env.StringMap.filter_map
    (fun _dname count -> if count > default_bound then Some count else None)
    counts

(** Get the bound for a specific domain, using per-domain override if available.
*)
let bound_for config domain_name =
  match Env.StringMap.find_opt domain_name config.domain_bounds with
  | Some b -> b
  | None -> config.bound

type query = {
  name : string;
  description : string;
  smt2 : string;
  kind : query_kind;
  value_terms : string list;
  invariant_text : string;
  assertion_names : (string * string) list;
      (** Maps SMT assertion name to human-readable text *)
}
(** A generated SMT query with metadata *)

and query_kind =
  | Contradiction  (** SAT = ok, UNSAT = contradiction found *)
  | InvariantConsistency
      (** SAT = ok (invariants jointly satisfiable), UNSAT = contradiction *)
  | InvariantPreservation
      (** SAT = violation (counterexample), UNSAT = preserved *)
  | PreconditionSat  (** SAT = ok, UNSAT = dead operation *)
  | BMCDeadlock
      (** SAT = reachable deadlock found, UNSAT = no deadlock within k steps *)
  | InitConsistency
      (** SAT = ok (initial state possible), UNSAT = impossible initial state *)
  | InitInvariant
      (** SAT = violation (invariant not satisfied initially), UNSAT = ok *)
  | BMCInvariant
      (** SAT = reachable violation (concrete attack trace), UNSAT = safe up to
          k steps *)

(** SMT sort name for a Pantagruel type *)
let rec sort_of_ty = function
  | TyBool -> "Bool"
  | TyNat | TyNat0 | TyInt -> "Int"
  | TyReal -> "Real"
  | TyString -> "String"
  | TyNothing -> "Int" (* bottom type, never instantiated *)
  | TyDomain name -> name
  | TyList inner ->
      (* Model lists/sets as membership predicates: Array elem_sort Bool *)
      Printf.sprintf "(Array %s Bool)" (sort_of_ty inner)
  | TyProduct ts ->
      let name = product_sort_name ts in
      name
  | TySum ts ->
      let name = sum_sort_name ts in
      name
  | TyFunc _ -> "Int" (* functions are declared separately, not as sorts *)

and product_sort_name ts =
  "Pair_" ^ String.concat "_" (List.map sort_base_name ts)

and sum_sort_name ts = "Sum_" ^ String.concat "_" (List.map sort_base_name ts)

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
  name |> String.to_seq
  |> Seq.map (fun c -> match c with '-' -> '_' | '?' -> 'p' | _ -> c)
  |> String.of_seq

(** Generate sort declarations for user-defined domains *)
let declare_domain_sorts config env =
  let buf = Buffer.create 256 in
  Env.StringMap.iter
    (fun name entry ->
      match entry.Env.kind with
      | Env.KDomain ->
          let elems = domain_elements name (bound_for config name) in
          Buffer.add_string buf (Printf.sprintf "(declare-sort %s 0)\n" name);
          List.iter
            (fun elem ->
              Buffer.add_string buf
                (Printf.sprintf "(declare-const %s %s)\n" elem name))
            elems;
          if List.length elems > 1 then
            Buffer.add_string buf
              (Printf.sprintf "(assert (distinct %s))\n"
                 (String.concat " " elems));
          (* Closure axiom: every element of the sort is one of our constants *)
          let disj = List.map (fun e -> Printf.sprintf "(= _x_ %s)" e) elems in
          Buffer.add_string buf
            (Printf.sprintf "(assert (forall ((_x_ %s)) (or %s)))\n" name
               (String.concat " " disj))
      | _ -> ())
    env.Env.types;
  Buffer.contents buf

(** Collect all product/sum types used in the environment for datatype
    declarations *)
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
      | Env.KRule ty | Env.KVar ty | Env.KAlias ty | Env.KClosure (ty, _) ->
          visit ty
      | Env.KDomain -> ())
    env.Env.terms;
  Env.StringMap.iter
    (fun _ entry ->
      match entry.Env.kind with Env.KAlias ty -> visit ty | _ -> ())
    env.Env.types;
  (products, sums)

(** Topologically sort composite type entries so that dependencies are declared
    first. Each entry is (name, component_types). A type depends on another
    composite type if any of its components is that type. *)
let topo_sort_composites (entries : (string * ty list) list) =
  let names = Hashtbl.create 8 in
  List.iter (fun (n, _) -> Hashtbl.replace names n true) entries;
  (* Compute which composite names each entry depends on *)
  let rec collect_deps acc = function
    | TyProduct ts ->
        let n = product_sort_name ts in
        let acc = if Hashtbl.mem names n then n :: acc else acc in
        List.fold_left collect_deps acc ts
    | TySum ts ->
        let n = sum_sort_name ts in
        let acc = if Hashtbl.mem names n then n :: acc else acc in
        List.fold_left collect_deps acc ts
    | TyList inner -> collect_deps acc inner
    | _ -> acc
  in
  let visited = Hashtbl.create 8 in
  let result = ref [] in
  let rec visit name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.replace visited name true;
      (match List.assoc_opt name entries with
      | Some ts ->
          let deps = List.fold_left collect_deps [] ts in
          List.iter visit deps
      | None -> ());
      result := (name, List.assoc name entries) :: !result
    end
  in
  List.iter (fun (name, _) -> visit name) entries;
  List.rev !result

(** Generate datatype declarations for product and sum types *)
let declare_composite_types env =
  let products, sums = collect_composite_types env in
  let buf = Buffer.create 256 in
  let product_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) products [] in
  let sum_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) sums [] in
  let sorted = topo_sort_composites (product_list @ sum_list) in
  List.iter
    (fun (name, ts) ->
      if Hashtbl.mem products name then begin
        let fields =
          List.mapi
            (fun i t -> Printf.sprintf "(fst_%d %s)" (i + 1) (sort_of_ty t))
            ts
        in
        Buffer.add_string buf
          (Printf.sprintf "(declare-datatype %s ((%s %s)))\n" name
             (Printf.sprintf "mk_%s" name)
             (String.concat " " fields))
      end
      else
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
    sorted;
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
      | Env.KRule ty | Env.KClosure (ty, _) -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with
          | Some ([], ret) ->
              Buffer.add_string buf
                (Printf.sprintf "(declare-const %s %s)\n" sname (sort_of_ty ret));
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

(** Generate closure axioms for a single closure rule. Produces finite-unrolling
    axioms up to [bound-1] steps. [is_prime] controls whether to use primed
    function names. *)
let generate_closure_axiom config env ~is_prime closure_name target_name
    domain_name =
  let bound = bound_for config domain_name in
  let cname = sanitize_ident closure_name ^ if is_prime then "_prime" else "" in
  let tname = sanitize_ident target_name ^ if is_prime then "_prime" else "" in
  (* Determine target shape to generate the right step expression *)
  let target_entry = Env.lookup_term target_name env in
  let target_is_list =
    match target_entry with
    | Some { kind = Env.KRule (TyFunc (_, Some (TyList _))); _ } -> true
    | _ -> false
  in
  let step_expr x y =
    if target_is_list then Printf.sprintf "(select (%s %s) %s)" tname x y
    else
      let sum_sort = sum_sort_name [ TyDomain domain_name; TyNothing ] in
      Printf.sprintf "(= (%s %s) (mk_%s_0 %s))" tname x sum_sort y
  in
  (* Fully grounded axioms: enumerate all (x, y) pairs and expand
     intermediate chains over concrete domain elements.  This avoids
     quantifier alternation (forall-exists) that causes solver timeouts. *)
  let elems = domain_elements domain_name bound in
  let max_depth = max 1 (bound - 1) in
  let buf = Buffer.create 512 in
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          (* Build chain disjuncts for depths 1..max_depth *)
          let chains = ref [] in
          for depth = 1 to max_depth do
            if depth = 1 then chains := step_expr x y :: !chains
            else begin
              (* Enumerate all tuples of (depth-1) intermediate nodes *)
              let rec enum_intermediates k =
                if k = 0 then [ [] ]
                else
                  List.concat_map
                    (fun z ->
                      List.map
                        (fun rest -> z :: rest)
                        (enum_intermediates (k - 1)))
                    elems
              in
              let all_intermediates = enum_intermediates (depth - 1) in
              List.iter
                (fun intermediates ->
                  let nodes = [ x ] @ intermediates @ [ y ] in
                  let steps =
                    List.init depth (fun i ->
                        step_expr (List.nth nodes i) (List.nth nodes (i + 1)))
                  in
                  let chain =
                    match steps with
                    | [ s ] -> s
                    | _ -> Printf.sprintf "(and %s)" (String.concat " " steps)
                  in
                  chains := chain :: !chains)
                all_intermediates
            end
          done;
          let all_chains = List.rev !chains in
          let rhs =
            match all_chains with
            | [ single ] -> single
            | _ -> Printf.sprintf "(or %s)" (String.concat " " all_chains)
          in
          Buffer.add_string buf
            (Printf.sprintf "(assert (= (select (%s %s) %s) %s))\n" cname x y
               rhs))
        elems)
    elems;
  Buffer.contents buf

(** Generate all closure axioms for the environment *)
let generate_closure_axioms config env =
  let buf = Buffer.create 256 in
  Env.StringMap.iter
    (fun name entry ->
      match entry.Env.kind with
      | Env.KClosure (TyFunc ([ TyDomain dname ], _), target) ->
          Buffer.add_string buf
            (generate_closure_axiom config env ~is_prime:false name target dname);
          Buffer.add_string buf
            (generate_closure_axiom config env ~is_prime:true name target dname)
      | _ -> ())
    env.Env.terms;
  Buffer.contents buf

(** Collect type constraint expressions (e.g., Nat >= 1, Nat0 >= 0) for
    functions. Returns a list of (human_text, smt_expr) pairs. When
    [constrain_primed] is false, primed functions are unconstrained. *)
let collect_type_constraint_exprs ?(constrain_primed = true) _config env =
  let constraints = ref [] in
  let add_nat_constraint name sname params_sorts param_names ret_ty is_prime =
    let fname = if is_prime then sname ^ "_prime" else sname in
    let display_name = if is_prime then name ^ "'" else name in
    let type_name, bound =
      match ret_ty with
      | TyNat -> ("Nat", 1)
      | TyNat0 -> ("Nat0", 0)
      | _ -> ("", -1)
    in
    if bound >= 0 then begin
      let smt_expr =
        if params_sorts = [] then Printf.sprintf "(>= %s %d)" fname bound
        else
          Printf.sprintf "(forall (%s) (>= (%s %s) %d))"
            (String.concat " "
               (List.map2
                  (fun n s -> Printf.sprintf "(%s %s)" n s)
                  param_names params_sorts))
            fname
            (String.concat " " param_names)
            bound
      in
      let human = Printf.sprintf "%s : %s" display_name type_name in
      constraints := (human, smt_expr) :: !constraints
    end
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
              add_nat_constraint name sname params_sorts param_names ret false;
              if constrain_primed then
                add_nat_constraint name sname params_sorts param_names ret true
          | None -> ())
      | _ -> ())
    env.Env.terms;
  List.rev !constraints

(** Generate type constraints as plain assertions (for queries that don't need
    naming) *)
let declare_type_constraints ?(constrain_primed = true) config env =
  let buf = Buffer.create 256 in
  let exprs = collect_type_constraint_exprs ~constrain_primed config env in
  List.iter
    (fun (_human, smt_expr) ->
      Buffer.add_string buf (Printf.sprintf "(assert %s)\n" smt_expr))
    exprs;
  Buffer.contents buf

(** Generate the full preamble: sorts, functions, domain elements, constraints.
    When [constrain_primed] is false, primed functions are left unconstrained
    (used for invariant preservation queries where we want to check if the
    transition can violate type/invariant bounds). When
    [include_type_constraints] is false, type constraints are omitted (used when
    the caller will add them as named assertions instead). *)
let generate_preamble ?(constrain_primed = true)
    ?(include_type_constraints = true) config env =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "; --- Domain sorts and elements ---\n";
  Buffer.add_string buf (declare_domain_sorts config env);
  Buffer.add_string buf "\n; --- Composite types ---\n";
  Buffer.add_string buf (declare_composite_types env);
  Buffer.add_string buf "\n; --- Function declarations ---\n";
  Buffer.add_string buf (declare_functions env);
  let closure_axioms = generate_closure_axioms config env in
  if closure_axioms <> "" then begin
    Buffer.add_string buf "\n; --- Closure axioms ---\n";
    Buffer.add_string buf closure_axioms
  end;
  if include_type_constraints then begin
    Buffer.add_string buf "\n; --- Type constraints ---\n";
    Buffer.add_string buf
      (declare_type_constraints ~constrain_primed config env)
  end;
  Buffer.contents buf

(** Word-boundary-aware string substitution. Replaces occurrences of [from] in
    [s] with [to_], but only when [from] appears at a word boundary. Word
    boundary characters: '(', ')', ' ', '\n', '\t', and string edges. *)
let replace_word ~from ~to_ s =
  let from_len = String.length from in
  let s_len = String.length s in
  if from_len = 0 || s_len = 0 then s
  else
    let is_boundary i =
      i < 0 || i >= s_len
      || match s.[i] with '(' | ')' | ' ' | '\n' | '\t' -> true | _ -> false
    in
    let buf = Buffer.create (s_len + 64) in
    let i = ref 0 in
    while !i <= s_len - from_len do
      if
        String.sub s !i from_len = from
        && is_boundary (!i - 1)
        && is_boundary (!i + from_len)
      then begin
        Buffer.add_string buf to_;
        i := !i + from_len
      end
      else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    done;
    (* Copy remaining characters *)
    while !i < s_len do
      Buffer.add_char buf s.[!i];
      incr i
    done;
    Buffer.contents buf

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
          Ok (p.param_name, dname, None, [ (p.param_name, TyDomain dname) ])
      | _ ->
          Error "SMT translation: comprehension parameter must be a domain type"
      )
  | [] -> (
      (* Look for a leading GIn guard *)
      match guards with
      | GIn (name, list_expr) :: _rest -> (
          match Check.infer_type { Check.env; loc = dummy_loc } list_expr with
          | Ok (TyList (TyDomain dname)) ->
              Ok (name, dname, Some list_expr, [ (name, TyDomain dname) ])
          | _ ->
              Error
                "SMT translation: membership comprehension requires a domain \
                 list")
      | _ ->
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
            | _ -> None)
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
      (* Domain as a set value shouldn't appear standalone — OpIn, OpSubset,
         and OpCard all handle EDomain specially. If we reach here, it's an
         internal error in the translation. *)
      failwith
        (Printf.sprintf
           "SMT translation: EDomain '%s' appeared in standalone position" name)
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
  | EForall (params, guards, body) ->
      translate_quantifier config env "forall" params guards body
  | EEach (params, guards, body) ->
      translate_forall_comprehension config env params guards body
  | EExists (params, guards, body) ->
      translate_quantifier config env "exists" params guards body
  | EInitially e -> translate_expr config env e
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
  match op with
  (* OpIn and OpSubset handle EDomain specially — don't eagerly translate *)
  | OpIn -> translate_in config env e1 e2
  | OpSubset -> translate_subset config env e1 e2
  | _ -> (
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
  match set with
  | EDomain name ->
      (* x in Domain → disjunction over domain elements *)
      let elems = domain_elements name (bound_for config name) in
      let elem_str = translate_expr config env elem in
      let disj =
        List.map (fun e -> Printf.sprintf "(= %s %s)" elem_str e) elems
      in
      Printf.sprintf "(or %s)" (String.concat " " disj)
  | EEach (params, guards, body) -> (
      (* y in (each x: D | f x) → disjunction: (= y (f d0)) ∨ (= y (f d1)) ...
         y in (each x: D, g x | f x) → (g d0 ∧ = y (f d0)) ∨ ... *)
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
  match e2 with
  | EEach (params, guards, body) -> (
      (* xs subset (each x: D | f x) → for every elem of xs, elem is in the
         comprehension. Expand: for each domain elem d of the LHS element type,
         (select xs d) => (exists comprehension elem matching d). *)
      let expanded =
        expand_comprehension translate_expr config env params guards body
      in
      (* Infer LHS element type to get its domain *)
      match Check.infer_type { Check.env; loc = dummy_loc } e1 with
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
  | EDomain name ->
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
      match Check.infer_type { Check.env; loc = dummy_loc } e1 with
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
  match e with
  | EDomain name ->
      (* #Domain = bound (all elements exist) *)
      string_of_int (bound_for config name)
  | EEach (params, guards, body) -> (
      (* #(each x: D | f x) — count distinct values in the comprehension.
         Requires the range type to be a bounded domain. Expand over range
         domain elements, check if each is produced by any source element. *)
      let expanded =
        expand_comprehension translate_expr config env params guards body
      in
      (* Infer the body type to determine the range domain *)
      let param_bindings =
        List.filter_map
          (fun (p : param) ->
            match Collect.resolve_type env p.param_type dummy_loc with
            | Ok ty -> Some (p.param_name, ty)
            | Error _ -> None)
          params
      in
      let guard_bindings =
        List.filter_map
          (fun g ->
            match g with
            | GIn (name, list_expr) -> (
                match
                  Check.infer_type { Check.env; loc = dummy_loc } list_expr
                with
                | Ok (TyList elem_ty) -> Some (name, elem_ty)
                | _ -> None)
            | _ -> None)
          guards
      in
      let env_inner = Env.with_vars (param_bindings @ guard_bindings) env in
      match
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
      match Check.infer_type { Check.env; loc = dummy_loc } e with
      | Ok (TyList (TyDomain name)) ->
          (* Sum over domain elements: (+ (ite (select xs d0) 1 0) ...) *)
          let elems = domain_elements name (bound_for config name) in
          let terms =
            List.map
              (fun d -> Printf.sprintf "(ite (select %s %s) 1 0)" set_str d)
              elems
          in
          Printf.sprintf "(+ %s)" (String.concat " " terms)
      | Ok (TyList elem_ty) ->
          (* Non-domain list: try to resolve element type to a domain.
             If the element type is truly unbounded (e.g., Int), cardinality
             can't be computed in bounded model checking — emit 0 with a comment. *)
          let sort = sort_of_ty elem_ty in
          Printf.sprintf
            "; WARNING: cardinality of non-domain list (%s) is approximate\n\
             0 ; unbounded element type %s"
            set_str sort
      | _ ->
          (* Can't determine element type at all *)
          Printf.sprintf "; WARNING: unknown cardinality\n0")

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
  let param_bindings =
    List.filter_map
      (fun (p : param) ->
        match Collect.resolve_type env p.param_type dummy_loc with
        | Ok ty -> Some (p.param_name, ty)
        | Error _ -> None)
      params
  in
  let guard_bindings =
    List.filter_map
      (fun g ->
        match g with
        | GIn (name, list_expr) -> (
            match Check.infer_type { Check.env; loc = dummy_loc } list_expr with
            | Ok (TyList elem_ty) -> Some (name, elem_ty)
            | _ -> None)
        | _ -> None)
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

and translate_quantifier config env quant params guards body =
  (* Enrich env with formal parameter bindings so that type inference
     on guard expressions (e.g., GIn list exprs) can resolve them. *)
  let param_bindings =
    List.filter_map
      (fun (p : param) ->
        match Collect.resolve_type env p.param_type dummy_loc with
        | Ok ty -> Some (p.param_name, ty)
        | Error _ -> None)
      params
  in
  let env = Env.with_vars param_bindings env in
  (* Collect bindings, guard conditions, and type constraints for Nat/Nat0 *)
  let nat_constraint_of_param env (p : param) =
    match Collect.resolve_type env p.param_type dummy_loc with
    | Ok TyNat ->
        Some (Printf.sprintf "(>= %s 1)" (sanitize_ident p.param_name))
    | Ok TyNat0 ->
        Some (Printf.sprintf "(>= %s 0)" (sanitize_ident p.param_name))
    | _ -> None
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
                   p.param_name)
        in
        Printf.sprintf "(%s %s)" (sanitize_ident p.param_name) sort)
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
                       p.param_name)
            in
            let env =
              match Collect.resolve_type env p.param_type dummy_loc with
              | Ok ty -> Env.with_vars [ (p.param_name, ty) ] env
              | Error _ -> env
            in
            let conds =
              match nat_constraint_of_param env p with
              | Some c -> c :: conds
              | None -> conds
            in
            ( Printf.sprintf "(%s %s)" (sanitize_ident p.param_name) sort
              :: binds,
              conds,
              env )
        | GIn (name, list_expr) ->
            (* Bind name as element type; resolve from list expression type *)
            let elem_ty_opt =
              match
                Check.infer_type { Check.env; loc = dummy_loc } list_expr
              with
              | Ok (TyList elem_ty) -> Some elem_ty
              | _ -> None
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
            let guard_str = translate_in config env (EVar name) list_expr in
            ( Printf.sprintf "(%s %s)" (sanitize_ident name) elem_sort :: binds,
              guard_str :: conds,
              env )
        | GExpr e -> (binds, translate_expr config env e :: conds, env))
      ([], [], env) guards
  in
  let all_bindings = bindings @ List.rev guard_bindings in
  let body_str = translate_expr config env body in
  let conditions = param_type_conditions @ List.rev guard_conditions in
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
      Action { label; params; guards; context; propositions = chapter.body }
  | None -> Invariant chapter.body

let classify_chapters (doc : document) = List.map classify_chapter doc.chapters

(** Collect all invariants from the document (non-initially propositions) *)
let collect_invariants chapters =
  List.concat_map
    (fun c ->
      match c with
      | Invariant props ->
          List.filter
            (fun (p : expr located) ->
              match p.value with EInitially _ -> false | _ -> true)
            props
      | Action _ -> [])
    chapters

(** Collect all initial-state propositions, stripping the EInitially wrapper *)
let collect_initial_props chapters =
  List.concat_map
    (fun c ->
      match c with
      | Invariant props ->
          List.filter_map
            (fun (p : expr located) ->
              match p.value with
              | EInitially e -> Some { p with value = e }
              | _ -> None)
            props
      | Action _ -> [])
    chapters

type action_info = {
  a_label : string;
  a_params : param list;
  a_guards : guard list;
  a_context : string option;
  a_propositions : expr located list;
}
(** Collect all actions from the document *)

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
        List.map
          (fun (p : expr located) -> translate_expr config env p.value)
          props
      in
      Printf.sprintf "(and %s)" (String.concat " " parts)

(** Substitute primed names in an expression (for invariant checking in next
    state). Tracks locally-bound names (from quantifiers) to avoid priming them.
*)
let rec prime_expr ?(bound = []) (e : expr) : expr =
  match e with
  | EVar name -> if List.mem name bound then e else EPrimed name
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
  | EEach (ps, gs, body) ->
      let bound' = List.map (fun (p : param) -> p.param_name) ps @ bound in
      let bound'', gs' = prime_guards ~bound:bound' gs in
      EEach (ps, gs', prime_expr ~bound:bound'' body)
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

and prime_guards ~bound gs =
  List.fold_left
    (fun (bound, acc) g ->
      match g with
      | GParam p -> (p.param_name :: bound, acc @ [ GParam p ])
      | GIn (name, e) ->
          (name :: bound, acc @ [ GIn (name, prime_expr ~bound e) ])
      | GExpr e -> (bound, acc @ [ GExpr (prime_expr ~bound e) ]))
    (bound, []) gs

(** Generate frame condition expressions: for every rule NOT in the action's
    context, produce the SMT expression asserting f_prime = f. Returns a list of
    (function_name, smt_expr) pairs. *)
let collect_frame_exprs _config env context =
  match context with
  | None -> []
  | Some ctx_name -> (
      match Env.lookup_context ctx_name env with
      | None -> []
      | Some context_members ->
          Env.StringMap.fold
            (fun name entry acc ->
              match entry.Env.kind with
              | Env.KRule ty when not (List.mem name context_members) -> (
                  let sname = sanitize_ident name in
                  match decompose_func_ty ty with
                  | Some ([], _ret) ->
                      (name, Printf.sprintf "(= %s_prime %s)" sname sname)
                      :: acc
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
                      ( name,
                        Printf.sprintf "(forall (%s) (= (%s_prime %s) (%s %s)))"
                          (String.concat " " bindings)
                          sname args sname args )
                      :: acc
                  | None -> acc)
              | _ -> acc)
            env.Env.terms []
          |> List.rev)

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

(** Extract preconditions from action guards (non-binding boolean conditions) *)
let extract_preconditions config env (guards : guard list) =
  List.filter_map
    (fun g ->
      match g with GExpr e -> Some (translate_expr config env e) | _ -> None)
    guards

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
                 p.param_name)
      in
      Buffer.add_string buf
        (Printf.sprintf "(declare-const %s %s)\n"
           (sanitize_ident p.param_name)
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
               (sanitize_ident p.param_name))
      | Ok TyNat0 ->
          Buffer.add_string buf
            (Printf.sprintf "(assert (>= %s 0))\n"
               (sanitize_ident p.param_name))
      | _ -> ())
    params;
  Buffer.contents buf

(** Build SMT term strings to request values for in counterexamples. Includes
    action parameters, function applications to action params, and function
    applications to domain elements (for invariant queries where violations may
    occur on elements other than action params). *)
let build_value_terms config env (params : param list) =
  let param_terms =
    List.map (fun (p : param) -> sanitize_ident p.param_name) params
  in
  let param_set =
    List.filter_map
      (fun (p : param) ->
        match Collect.resolve_type env p.param_type dummy_loc with
        | Ok ty -> Some (sanitize_ident p.param_name, ty)
        | Error _ -> None)
      params
  in
  let func_terms =
    Env.StringMap.fold
      (fun name entry acc ->
        match entry.Env.kind with
        | Env.KRule ty | Env.KClosure (ty, _) -> (
            let sname = sanitize_ident name in
            match decompose_func_ty ty with
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
                  | _ -> []
                in
                List.concat from_params @ from_elems @ acc
            | _ -> acc)
        | _ -> acc)
      env.Env.terms []
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
            (Printf.sprintf "%s : Nat" p.param_name)
            (Printf.sprintf "(>= %s 1)" (sanitize_ident p.param_name))
      | Ok TyNat0 ->
          add_named_assert na "type"
            (Printf.sprintf "%s : Nat0" p.param_name)
            (Printf.sprintf "(>= %s 0)" (sanitize_ident p.param_name))
      | _ -> ())
    params

(** Query 1: Contradiction detection for an action. Asserts all postconditions
    and checks satisfiability. UNSAT = contradiction found. *)
let declare_domain_membership config buf (params : param list) env =
  List.iter
    (fun (p : param) ->
      match Collect.resolve_type env p.param_type dummy_loc with
      | Ok (TyDomain name) ->
          let elems = domain_elements name (bound_for config name) in
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
      (fun g -> match g with GExpr e -> Some e | _ -> None)
      action.a_guards
  in
  List.iter
    (fun e ->
      add_named_assert na "precond" (Pretty.str_expr e)
        (translate_expr config env e))
    precond_exprs;
  (* Named frame conditions *)
  let frame_exprs = collect_frame_exprs config env action.a_context in
  List.iter
    (fun (fname, smt_expr) ->
      add_named_assert na "frame"
        (Printf.sprintf "%s' = %s (frame)" fname fname)
        smt_expr)
    frame_exprs;
  (* Named postconditions *)
  Buffer.add_string buf "\n; --- Postconditions ---\n";
  List.iter
    (fun (p : expr located) ->
      add_named_assert na "postcond" (Pretty.str_expr p.value)
        (translate_expr config env p.value))
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
  (* Assert action postconditions (transition relation) *)
  Buffer.add_string buf "\n; --- Action postconditions (transition) ---\n";
  let transition = conjoin_propositions config env action.a_propositions in
  Buffer.add_string buf (Printf.sprintf "(assert %s)\n" transition);
  (* Assert frame conditions *)
  Buffer.add_string buf (generate_frame_conditions config env action.a_context);
  (* Assert single invariant violated in next state *)
  Buffer.add_string buf "\n; --- Invariant violated in next state ---\n";
  let primed_inv = { inv with value = prime_expr inv.value } in
  let inv_primed = translate_expr config env primed_inv.value in
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
          (translate_expr config env inv.value))
      invariant_props
  end;
  (* Named preconditions *)
  Buffer.add_string buf "\n; --- Preconditions ---\n";
  let precond_exprs =
    List.filter_map
      (fun g -> match g with GExpr e -> Some e | _ -> None)
      action.a_guards
  in
  List.iter
    (fun e ->
      add_named_assert na "precond"
        (Printf.sprintf "Precondition: %s" (Pretty.str_expr e))
        (translate_expr config env e))
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

(** Collect all function names referenced in an expression *)
let rec collect_function_refs (e : expr) =
  match e with
  | EVar name -> [ name ]
  | EApp (func, args) ->
      collect_function_refs func @ List.concat_map collect_function_refs args
  | EPrimed name -> [ name ]
  | EBinop (_, e1, e2) -> collect_function_refs e1 @ collect_function_refs e2
  | EUnop (_, e) -> collect_function_refs e
  | EForall (_, gs, body) | EExists (_, gs, body) | EEach (_, gs, body) ->
      List.concat_map collect_guard_refs gs @ collect_function_refs body
  | ETuple es -> List.concat_map collect_function_refs es
  | EProj (e, _) -> collect_function_refs e
  | EOverride (name, pairs) ->
      name
      :: List.concat_map
           (fun (k, v) -> collect_function_refs k @ collect_function_refs v)
           pairs
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

(** Build SMT value terms for invariant consistency queries (no action params,
    no primed functions). Includes nullary rules and unary rules applied to
    domain elements. *)
let build_invariant_value_terms config env =
  Env.StringMap.fold
    (fun name entry acc ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KClosure (ty, _) -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with
          | Some ([], _ret) -> sname :: acc
          | Some ([ param_ty ], _ret) -> (
              match param_ty with
              | TyDomain dname ->
                  let elems = domain_elements dname (bound_for config dname) in
                  List.map
                    (fun elem -> Printf.sprintf "(%s %s)" sname elem)
                    elems
                  @ acc
              | _ -> acc)
          | _ -> acc)
      | _ -> acc)
    env.Env.terms []

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
        (translate_expr config env inv.value))
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

(** Extract the GExpr preconditions from an action's guards as raw exprs *)
let extract_precondition_exprs (guards : guard list) =
  List.filter_map (fun g -> match g with GExpr e -> Some e | _ -> None) guards

(** Check if an action has no boolean preconditions (always enabled) *)
let action_always_enabled action =
  extract_precondition_exprs action.a_guards = []

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
        (translate_expr config env prop.value))
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
      let smt = translate_expr config env prop.value in
      Buffer.add_string buf (Printf.sprintf "(assert %s)\n" smt))
    init_props;
  (* Negate the invariant *)
  Buffer.add_string buf "\n; --- Invariant negated ---\n";
  let inv_smt = translate_expr config env inv.value in
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
  Env.StringMap.fold
    (fun name entry acc ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KClosure (ty, _) -> (
          let sname = sanitize_ident name in
          match decompose_func_ty ty with Some _ -> sname :: acc | None -> acc)
      | _ -> acc)
    env.Env.terms []

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
  Env.StringMap.iter
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
      | _ -> ())
    env.Env.terms;
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
  (* Closure axioms for each step *)
  let base_closure_axioms = generate_closure_axioms config env in
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
                         p.param_name)
              in
              Printf.sprintf "(%s %s)" (sanitize_ident p.param_name) sort)
            action.a_params
        in
        (* Domain membership for action params *)
        let domain_conds =
          List.filter_map
            (fun (p : param) ->
              match Collect.resolve_type env p.param_type dummy_loc with
              | Ok (TyDomain name) ->
                  let elems = domain_elements name (bound_for config name) in
                  let sname = sanitize_ident p.param_name in
                  let disj =
                    List.map (fun e -> Printf.sprintf "(= %s %s)" sname e) elems
                  in
                  Some (Printf.sprintf "(or %s)" (String.concat " " disj))
              | _ -> None)
            action.a_params
        in
        (* Type constraints for action params *)
        let type_conds =
          List.filter_map
            (fun (p : param) ->
              match Collect.resolve_type env p.param_type dummy_loc with
              | Ok TyNat ->
                  Some
                    (Printf.sprintf "(>= %s 1)" (sanitize_ident p.param_name))
              | Ok TyNat0 ->
                  Some
                    (Printf.sprintf "(>= %s 0)" (sanitize_ident p.param_name))
              | _ -> None)
            action.a_params
        in
        (* Preconditions (from guards) — translated in base names *)
        let precond_parts = extract_preconditions config env action.a_guards in
        (* Postconditions — translated with base name/prime *)
        let postcond_parts =
          List.map
            (fun (p : expr located) -> translate_expr config env p.value)
            action.a_propositions
        in
        (* Frame conditions *)
        let frame_parts =
          List.map snd (collect_frame_exprs config env action.a_context)
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
      let smt = translate_expr config env prop.value in
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
  let inv_smt = translate_expr config env inv.value in
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
                       p.param_name)
            in
            Printf.sprintf "(%s %s)" (sanitize_ident p.param_name) sort)
          action.a_params
      in
      let type_guards =
        List.filter_map
          (fun (p : param) ->
            match Collect.resolve_type env p.param_type dummy_loc with
            | Ok TyNat ->
                Some (Printf.sprintf "(>= %s 1)" (sanitize_ident p.param_name))
            | Ok TyNat0 ->
                Some (Printf.sprintf "(>= %s 0)" (sanitize_ident p.param_name))
            | _ -> None)
          action.a_params
      in
      let precond_strs =
        List.map
          (fun e -> rename_smt_for_step env (translate_expr config env e) step)
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
      let smt = translate_expr config env prop.value in
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

(** Generate all verification queries for a document *)
let generate_queries config env (doc : document) =
  let chapters = classify_chapters doc in
  let invariants = collect_invariants chapters in
  let init_props = collect_initial_props chapters in
  let actions = collect_actions chapters in
  let queries = ref [] in
  (* Invariant consistency query *)
  if invariants <> [] then
    queries :=
      generate_invariant_consistency_query config env invariants :: !queries;
  (* Init consistency query *)
  if init_props <> [] then
    queries := generate_init_consistency_query config env init_props :: !queries;
  (* Init satisfies invariants queries *)
  if init_props <> [] && invariants <> [] then
    List.iteri
      (fun index inv ->
        queries :=
          generate_init_invariant_query config env init_props ~index inv
          :: !queries)
      invariants;
  (* Contradiction queries *)
  List.iter
    (fun action ->
      queries := generate_contradiction_query config env action :: !queries)
    actions;
  (* Invariant preservation queries — one per (invariant, action) pair.
     Skip if invariant only touches extracontextual functions (trivially
     preserved by frame conditions). *)
  if invariants <> [] then
    List.iter
      (fun action ->
        List.iteri
          (fun index inv ->
            if invariant_touches_context env action.a_context [ inv ] then
              queries :=
                generate_invariant_query config env ~all_invariants:invariants
                  ~index inv action
                :: !queries)
          invariants)
      actions;
  (* Precondition satisfiability queries *)
  List.iter
    (fun action ->
      queries :=
        generate_precondition_query config env invariants action :: !queries)
    actions;
  (* BMC deadlock freedom query — skip if any action has no preconditions
     (always enabled, so deadlock is impossible), or if no initial state *)
  let has_guarded_actions =
    actions <> [] && not (List.exists action_always_enabled actions)
  in
  if has_guarded_actions && init_props <> [] && config.steps > 0 then
    queries :=
      generate_bmc_deadlock_query config env init_props invariants actions
        ~steps:config.steps
      :: !queries;
  (* BMC queries — when init props, actions, and invariants all exist *)
  if init_props <> [] && actions <> [] && invariants <> [] && config.steps > 0
  then
    List.iteri
      (fun inv_index inv ->
        queries :=
          generate_bmc_query config env init_props actions ~inv_index inv
            ~steps:config.steps
          :: !queries)
      invariants;
  List.rev !queries
