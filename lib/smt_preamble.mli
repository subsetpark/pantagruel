(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** SMT-LIB2 preamble generation: sorts, functions, constraints, and shared
    translation helpers. *)

val declare_domain_sorts : Smt_types.config -> Env.t -> string
val declare_composite_types : Env.t -> string
val decompose_func_ty : Types.ty -> (Types.ty list * Types.ty) option
val declare_functions : Env.t -> string

val generate_closure_axioms :
  ?include_primed:bool -> Smt_types.config -> Env.t -> string

val collect_type_constraint_exprs :
  ?constrain_primed:bool -> Smt_types.config -> Env.t -> (string * string) list

val declare_type_constraints :
  ?constrain_primed:bool -> Smt_types.config -> Env.t -> string

val generate_preamble :
  ?constrain_primed:bool ->
  ?include_type_constraints:bool ->
  Smt_types.config ->
  Env.t ->
  string

val replace_word : from:string -> to_:string -> string -> string
val resolve_param_bindings : Env.t -> Ast.param list -> (string * Types.ty) list
val resolve_param_sort : Env.t -> Ast.type_expr -> string option
