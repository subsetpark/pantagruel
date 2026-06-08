(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** SMT-LIB2 shared types, sort functions, and configuration. *)

type config = {
  bound : int;
  steps : int;
  domain_bounds : int Env.StringMap.t;
  inject_guards : bool;
  ground_quantifiers : bool;
  quant_bound : string list;
}

val ground_instance_cap : int

val make_config :
  bound:int ->
  steps:int ->
  domain_bounds:int Env.StringMap.t ->
  inject_guards:bool ->
  ?ground_quantifiers:bool ->
  unit ->
  config

val splice_before_first_assert : string -> string -> string
val compute_domain_bounds : int -> Env.t -> int Env.StringMap.t
val bound_for : config -> string -> int

type query = {
  name : string;
  description : string;
  smt2 : string;
  kind : query_kind;
  value_terms : string list;
  invariant_text : string;
  assertion_names : (string * string) list;
}

and query_kind =
  | Contradiction
  | InvariantConsistency
  | InvariantPreservation
  | PreconditionSat
  | BMCDeadlock
  | InitConsistency
  | InitInvariant
  | BMCInvariant
  | CondExhaustiveness
  | Entailment

val sort_of_ty : Types.ty -> string
val product_sort_name : Types.ty list -> string
val sum_sort_name : Types.ty list -> string
val sort_base_name : Types.ty -> string
val domain_elements : string -> int -> string list
val sanitize_ident : string -> string
val smt_rule_name : Env.t -> string -> int -> string
val smt_qualified_rule_name : Env.t -> string -> string -> int -> string
