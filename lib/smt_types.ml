(** SMT-LIB2 shared types, sort functions, and configuration *)

open Types

type config = {
  bound : int;
  steps : int;
  domain_bounds : int Env.StringMap.t;
  inject_guards : bool;
  quant_bound : string list;
      (** Accumulated quantifier-bound variable names from enclosing scopes.
          Used by [collect_body_guards] so that guard injection does not
          incorrectly prime variables bound by outer quantifiers. *)
}
(** Configuration for bounded checking. [steps] controls k-step BMC depth.
    [domain_bounds] maps domain names to per-domain minimum bounds (derived from
    nullary constant counts). [quant_bound] is internal traversal state — use
    [make_config] to construct. *)

let make_config ~bound ~steps ~domain_bounds ~inject_guards =
  { bound; steps; domain_bounds; inject_guards; quant_bound = [] }

(** Fresh uninterpreted constants for non-exhaustive cond else-branches. When a
    cond's last arm guard is not [true], we emit a fresh constant of the
    appropriate sort so the gap is genuinely unconstrained rather than silently
    assigned the last arm's consequence. *)
let cond_aux_counter = ref 0

let cond_aux_decls : string list ref = ref []

let reset_cond_aux () =
  cond_aux_counter := 0;
  cond_aux_decls := []

let fresh_cond_default sort =
  let n = !cond_aux_counter in
  incr cond_aux_counter;
  let name = Printf.sprintf "_cond_undef_%d" n in
  cond_aux_decls :=
    Printf.sprintf "(declare-const %s %s)" name sort :: !cond_aux_decls;
  name

let drain_cond_aux_decls () =
  let decls = List.rev !cond_aux_decls in
  cond_aux_decls := [];
  if decls = [] then ""
  else "\n; --- Cond default constants ---\n" ^ String.concat "\n" decls ^ "\n"

(** Splice [decls] into [smt2] right before the first [(assert ...)] line. Used
    to inject accumulated auxiliary declarations after the per-query translator
    has already produced the body text. *)
let splice_before_first_assert smt2 decls =
  if decls = "" then smt2
  else
    let lines = String.split_on_char '\n' smt2 in
    let rec split acc = function
      | [] -> (List.rev acc, [])
      | line :: rest
        when String.length line >= 7 && String.sub line 0 7 = "(assert" ->
          (List.rev acc, line :: rest)
      | line :: rest -> split (line :: acc) rest
    in
    let before, after = split [] lines in
    String.concat "\n" before ^ decls ^ String.concat "\n" after

(** Insert accumulated cond-default declarations into a finished SMT-LIB2
    string. Must be called after all [translate_*] calls for the query. *)
let insert_cond_aux_decls smt2 =
  splice_before_first_assert smt2 (drain_cond_aux_decls ())

(** Fresh uninterpreted constants for translation fallbacks. When a translation
    site cannot produce a faithful SMT term (e.g. cardinality of a list over an
    unbounded element type, or a standalone first-class override that has no
    direct SMT-LIB encoding), it emits a fresh constant of the appropriate sort
    and uses its name in place of the silent literal it would otherwise emit.
    The named constants make the approximation visible to downstream tooling (in
    particular, the structural checks in [test/smt_check.ml]). *)
let fallback_counter = ref 0

let fallback_decls : string list ref = ref []

let reset_fallbacks () =
  fallback_counter := 0;
  fallback_decls := []

let fresh_fallback ~kind ~sort =
  let n = !fallback_counter in
  incr fallback_counter;
  let name = Printf.sprintf "_%s_fallback_%d" kind n in
  fallback_decls :=
    Printf.sprintf "(declare-const %s %s)" name sort :: !fallback_decls;
  name

(** Query-local cache of list-search placeholder symbols. Identical
    [(func_s, arg_s)] keys reuse the same fresh constant so that [xs x = xs x]
    translates to a referentially stable equality. *)
let list_search_cache : (string * string, string) Hashtbl.t = Hashtbl.create 8

let reset_list_search_cache () = Hashtbl.clear list_search_cache

let intern_list_search_symbol ~func_s ~arg_s =
  match Hashtbl.find_opt list_search_cache (func_s, arg_s) with
  | Some name -> name
  | None ->
      let name = fresh_fallback ~kind:"list_search" ~sort:"Int" in
      Hashtbl.add list_search_cache (func_s, arg_s) name;
      name

(** Queue an additional [(assert ...)] alongside the most recently declared
    fallback constant — useful for soft constraints like non-negativity. *)
let add_fallback_assert assertion_body =
  fallback_decls :=
    Printf.sprintf "(assert %s)" assertion_body :: !fallback_decls

let drain_fallback_decls () =
  let decls = List.rev !fallback_decls in
  fallback_decls := [];
  if decls = [] then ""
  else "\n; --- Fallback constants ---\n" ^ String.concat "\n" decls ^ "\n"

(** Insert accumulated fallback declarations into a finished SMT-LIB2 string. *)
let insert_fallback_decls smt2 =
  splice_before_first_assert smt2 (drain_fallback_decls ())

(** Compute per-domain minimum bounds by counting nullary constants. For each
    domain, the bound is max(default_bound, number_of_nullary_constants). *)
let compute_domain_bounds default_bound env =
  let counts =
    Env.fold_terms
      (fun _name entry acc ->
        match entry.Env.kind with
        | Env.KRule ty -> (
            match ty with
            | (TyFunc ([], Some (TyDomain dname)) | TyDomain dname)
              when Env.lookup_type dname env
                   |> Option.map (fun e -> e.Env.kind = Env.KDomain)
                   |> Option.value ~default:false ->
                let cur =
                  Env.StringMap.find_opt dname acc |> Option.value ~default:0
                in
                Env.StringMap.add dname (cur + 1) acc
            | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing
            | TyDomain _ | TyList _ | TyProduct _ | TySum _ | TyFunc _ ->
                acc)
        | Env.KDomain | Env.KAlias _ | Env.KVar _ | Env.KClosure _ -> acc)
      env Env.StringMap.empty
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
  | CondExhaustiveness  (** SAT = non-exhaustive (counterexample), UNSAT = ok *)
  | Entailment  (** SAT = not entailed (counterexample), UNSAT = entailed *)

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
  |> Seq.map (fun c ->
      match c with '-' -> '_' | '?' -> 'p' | '!' -> 'b' | _ -> c)
  |> String.of_seq

(** SMT symbol name for a rule or closure reference. When the name has two or
    more arity overloads in [env], the symbol is mangled with an arity-tagged
    suffix ([foo$1], [foo$2]) so each overload gets a distinct SMT function
    symbol; single-arity rules keep their unmangled form to preserve existing
    snapshot output. The [$] separator is injective against [sanitize_ident]:
    Pantagruel lower identifiers permit only [a-zA-Z0-9-_?!] (and sanitize maps
    those into [a-zA-Z0-9_]), so a sanitized identifier can never contain [$].
    That guarantees an unrelated rule literally named [foo_1] cannot collide
    with [foo/1]'s mangled form. *)
let smt_rule_name env name arity =
  if Env.name_is_overloaded name env then
    sanitize_ident name ^ "$" ^ string_of_int arity
  else sanitize_ident name

(** Wrap a query generator: reset per-query auxiliary state (cond defaults and
    fallback constants), run the generator, and insert any accumulated
    declarations into the output. *)
let with_cond_aux f =
  reset_cond_aux ();
  reset_fallbacks ();
  reset_list_search_cache ();
  let q = f () in
  { q with smt2 = q.smt2 |> insert_cond_aux_decls |> insert_fallback_decls }
