(* @archlint.module core
   @archlint.domain pantagruel.smt-types *)

(** SMT-LIB2 shared types, sort functions, and configuration *)

open Types

type config = {
  bound : int;
  steps : int;
  domain_bounds : int Env.StringMap.t;
  inject_guards : bool;
  ground_quantifiers : bool;
      (** When true, quantifiers whose binders range over finite domain sorts
          are expanded ("grounded") into quantifier-free conjunctions /
          disjunctions over the enumerated domain elements, instead of being
          emitted as native SMT [forall]/[exists]. This trades a larger formula
          for a quantifier-free one, which the solver decides far more reliably
          (no dependence on quantifier-instantiation heuristics) for both sat-
          and unsat-seeking checks. *)
  quant_bound : string list;
      (** Accumulated quantifier-bound variable names from enclosing scopes.
          Used by [collect_body_guards] so that guard injection does not
          incorrectly prime variables bound by outer quantifiers. *)
}
(** Configuration for bounded checking. [steps] controls k-step BMC depth.
    [domain_bounds] maps domain names to per-domain minimum bounds (derived from
    nullary constant counts). [quant_bound] is internal traversal state — use
    [make_config] to construct. *)

(** Maximum number of grounded instances a single quantifier node may expand to.
    Above this, the node falls back to a native [forall]/[exists] (with a
    visible SMT comment) rather than expanding, bounding formula blow-up from
    high-arity / high-bound quantifiers. *)
let ground_instance_cap = 256

let make_config ~bound ~steps ~domain_bounds ~inject_guards
    ?(ground_quantifiers = true) () =
  {
    bound;
    steps;
    domain_bounds;
    inject_guards;
    ground_quantifiers;
    quant_bound = [];
  }

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

(** Splice [decls] after the last user-defined sort or datatype declaration.
    Auxiliary constants may use those sorts, so placing them before the first
    assertion is too early when domain axioms precede later sort declarations.
    If the query declares no user-defined types, fall back to inserting before
    the first assertion. *)
let splice_after_type_declarations smt2 decls =
  if decls = "" then smt2
  else
    let lines = String.split_on_char '\n' smt2 in
    let is_type_declaration line =
      List.exists
        (fun prefix ->
          let prefix_len = String.length prefix in
          String.length line >= prefix_len
          && String.sub line 0 prefix_len = prefix)
        [ "(declare-sort "; "(declare-datatype "; "(declare-datatypes " ]
    in
    let rec split_after_last_type before_rev last_split = function
      | [] -> last_split
      | line :: rest ->
          let before_rev = line :: before_rev in
          let last_split =
            if is_type_declaration line then Some (List.rev before_rev, rest)
            else last_split
          in
          split_after_last_type before_rev last_split rest
    in
    match split_after_last_type [] None lines with
    | None -> splice_before_first_assert smt2 decls
    | Some (before, after) ->
        String.concat "\n" before ^ decls ^ String.concat "\n" after

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

(** Hex-encode a Pantagruel identifier for use as one component of an SMT
    symbol. Encoding every byte makes the mapping injective: names such as [a-b]
    and [a_b] cannot collapse after punctuation rewriting. *)
let encode_ident name =
  let buf = Buffer.create (String.length name * 2) in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    name;
  Buffer.contents buf

let decode_ident encoded =
  let len = String.length encoded in
  if len mod 2 <> 0 then None
  else
    let buf = Buffer.create (len / 2) in
    let rec loop i =
      if i = len then Some (Buffer.contents buf)
      else
        match int_of_string_opt ("0x" ^ String.sub encoded i 2) with
        | Some code ->
            Buffer.add_char buf (Char.chr code);
            loop (i + 2)
        | None -> None
    in
    loop 0

(** User-originated SMT symbols live in disjoint namespaces. The [$] separators
    cannot occur in an encoded component. Names beginning with [_] are
    compiler-generated temporaries (source identifiers must begin with a
    letter), so they remain unchanged; already encoded names are also left
    untouched to make grounding/substitution idempotent. *)
let sanitize_ident name =
  if
    String.starts_with ~prefix:"pant$" name
    || (String.length name > 0 && name.[0] = '_')
  then name
  else "pant$v$" ^ encode_ident name

let smt_domain_name name = "pant$d$" ^ encode_ident name

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
  | TyDomain name -> smt_domain_name name
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
  "pant$t$product$" ^ String.concat "$" (List.map sort_base_name ts)

and sum_sort_name ts =
  "pant$t$sum$" ^ String.concat "$" (List.map sort_base_name ts)

and sort_base_name = function
  | TyBool -> "Bool"
  | TyNat | TyNat0 | TyInt -> "Int"
  | TyReal -> "Real"
  | TyString -> "String"
  | TyNothing -> "Nothing"
  | TyDomain name -> smt_domain_name name
  | TyList inner -> "pant$t$list$" ^ sort_base_name inner
  | TyProduct ts -> product_sort_name ts
  | TySum ts -> sum_sort_name ts
  | TyFunc _ -> "Func"

(** Generate domain element names *)
let domain_elements name bound =
  let encoded = encode_ident name in
  List.init bound (fun i -> Printf.sprintf "pant$e$%s$%d" encoded i)

(** SMT symbol name for a rule or closure reference. All rules use the [pant$r]
    namespace. When a name has two or more arity overloads in [env], the symbol
    also gets an arity-tagged suffix so each overload is distinct. The trailing
    [$] makes generated suffixes such as [_prime] and [_s0] unambiguous with
    respect to source names. *)
let smt_rule_name env name arity =
  let base = "pant$r$" ^ encode_ident name ^ "$" in
  if Env.name_is_overloaded name env then
    base ^ "arity$" ^ string_of_int arity ^ "$"
  else base

(** SMT symbol for an [EQualified] reference. When [(name, arity)] lives in the
    flat terms map (unambiguous import — exactly one origin), the qualified call
    shares a symbol with the unqualified one so that [all x | A::f x = f x]
    asserts the identity two users would expect. When it's reachable only via
    the qualified lookup (two or more modules export the same [(name, arity)]),
    each qualified call gets a distinct [pant$q]-prefixed symbol. Hex-encoded
    components and [$] separators make the representation injective. *)
let smt_qualified_rule_name env mod_name name arity =
  match Env.lookup_term_arity name arity env with
  | Some _ -> smt_rule_name env name arity
  | None ->
      "pant$q$" ^ encode_ident mod_name ^ "$" ^ encode_ident name ^ "$arity$"
      ^ string_of_int arity ^ "$"
