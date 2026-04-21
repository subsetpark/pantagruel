(** SMT-LIB2 preamble generation: sorts, functions, constraints *)

open Types
open Smt_types

(** Generate sort declarations for user-defined domains *)
let declare_domain_sorts config env =
  let buf = Buffer.create 256 in
  Env.iter_types
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
      | Env.KAlias _ | Env.KRule _ | Env.KVar _ | Env.KClosure _ -> ())
    env;
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
    | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing
    | TyDomain _ ->
        ()
  in
  Env.iter_terms
    (fun _ entry ->
      match entry.Env.kind with
      | Env.KRule ty | Env.KVar ty | Env.KAlias ty | Env.KClosure (ty, _) ->
          visit ty
      | Env.KDomain -> ())
    env;
  Env.iter_types
    (fun _ entry ->
      match entry.Env.kind with
      | Env.KAlias ty -> visit ty
      | Env.KDomain | Env.KRule _ | Env.KVar _ | Env.KClosure _ -> ())
    env;
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
    | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing
    | TyDomain _ | TyFunc _ ->
        acc
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
  | TyFunc (_, None)
  | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing | TyDomain _
  | TyList _ | TyProduct _ | TySum _ ->
      None

(** Generate function declarations from the environment *)
let declare_functions env =
  let buf = Buffer.create 256 in
  Env.iter_terms
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
      | Env.KDomain | Env.KAlias _ | Env.KVar _ -> ())
    env;
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
    match[@warning "-4"] target_entry with
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

(** Generate all closure axioms for the environment. When [~include_primed] is
    false, only base (unprimed) axioms are emitted. This is used by BMC
    step-indexing where primed names are handled via renaming and the last step
    has no step+1 declarations. *)
let generate_closure_axioms ?(include_primed = true) config env =
  let buf = Buffer.create 256 in
  Env.iter_terms
    (fun name entry ->
      match[@warning "-4"] entry.Env.kind with
      | Env.KClosure (TyFunc ([ TyDomain dname ], _), target) ->
          Buffer.add_string buf
            (generate_closure_axiom config env ~is_prime:false name target dname);
          if include_primed then
            Buffer.add_string buf
              (generate_closure_axiom config env ~is_prime:true name target
                 dname)
      | _ -> ())
    env;
  Buffer.contents buf

(** Collect type constraint expressions (e.g., Nat >= 1, Nat0 >= 0) for
    functions. Returns a list of (human_text, smt_expr) pairs. When
    [constrain_primed] is false, primed functions are unconstrained. *)
let collect_type_constraint_exprs ?(constrain_primed = true) _config env =
  let constraints = ref [] in
  let bound_of = function
    | TyNat -> Some ("Nat", 1)
    | TyNat0 -> Some ("Nat0", 0)
    | TyBool | TyInt | TyReal | TyString | TyNothing | TyDomain _ | TyList _
    | TyProduct _ | TySum _ | TyFunc _ ->
        None
  in
  let add_nat_constraint name sname params_sorts param_names ret_ty is_prime =
    let fname = if is_prime then sname ^ "_prime" else sname in
    let display_name = if is_prime then name ^ "'" else name in
    let applied =
      if params_sorts = [] then fname
      else Printf.sprintf "(%s %s)" fname (String.concat " " param_names)
    in
    let param_binders =
      List.map2
        (fun n s -> Printf.sprintf "(%s %s)" n s)
        param_names params_sorts
    in
    let wrap_forall binders body =
      if binders = [] then body
      else Printf.sprintf "(forall (%s) %s)" (String.concat " " binders) body
    in
    (match bound_of ret_ty with
    | Some (type_name, bound) ->
        let smt_expr =
          wrap_forall param_binders (Printf.sprintf "(>= %s %d)" applied bound)
        in
        let human = Printf.sprintf "%s : %s" display_name type_name in
        constraints := (human, smt_expr) :: !constraints
    | None -> ());
    match[@warning "-4"] ret_ty with
    | TyList elem_ty -> (
        match bound_of elem_ty with
        | Some (type_name, bound) ->
            let elem_binder = "k_elem" in
            let inner_binders =
              param_binders @ [ Printf.sprintf "(%s Int)" elem_binder ]
            in
            let smt_expr =
              wrap_forall inner_binders
                (Printf.sprintf "(=> (select %s %s) (>= %s %d))" applied
                   elem_binder elem_binder bound)
            in
            let human = Printf.sprintf "%s : [%s]" display_name type_name in
            constraints := (human, smt_expr) :: !constraints
        | None -> ())
    | _ -> ()
  in
  Env.iter_terms
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
      | Env.KDomain | Env.KAlias _ | Env.KVar _ | Env.KClosure _ -> ())
    env;
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

(** Resolve a parameter list to [(name, ty)] bindings, discarding any params
    whose types cannot be resolved. *)
let resolve_param_bindings env (params : Ast.param list) =
  List.filter_map
    (fun (p : Ast.param) ->
      match Collect.resolve_type env p.param_type Ast.dummy_loc with
      | Ok ty -> Some (Ast.lower_name p.param_name, ty)
      | Error _ -> None)
    params

(** Resolve a type expression to an SMT sort *)
let resolve_param_sort env te =
  match Collect.resolve_type env te Ast.dummy_loc with
  | Ok ty -> Some (sort_of_ty ty)
  | Error _ -> None
