(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** SMT expression transformations used by the SMT translator. *)

type comprehension_binding =
  | Domain of {
      name : string;
      domain : string;
      membership : Ast.expr option;
      bindings : (string * Types.ty) list;
    }
  | Numeric of {
      name : string;
      ty : Types.ty;
      bound : Ast.expr;
      inclusive : bool;
      bound_guard : Ast.guard;
      bindings : (string * Types.ty) list;
    }

val resolve_comprehension_binding :
  Env.t ->
  Ast.param list ->
  Ast.guard list ->
  (comprehension_binding, string) result

val resolve_numeric_comprehension_binding :
  Env.t ->
  Ast.param list ->
  (string * Types.ty * (string * Types.ty) list, string) result

val expand_comprehension :
  ?numeric_neutral:string ->
  (Smt_types.config -> Env.t -> Ast.expr -> string) ->
  Smt_types.config ->
  Env.t ->
  Ast.param list ->
  Ast.guard list ->
  Ast.expr ->
  (string option * string) list

val extract_upper_bound : Ast.param -> Ast.guard list -> Ast.expr option

val collect_body_guards :
  ?bound:string list -> Env.t -> Ast.expr -> Ast.expr list

val substitute_vars : (string * Ast.expr) list -> Ast.expr -> Ast.expr
val prime_expr : ?bound:string list -> Ast.expr -> Ast.expr
val unprime_expr : Ast.expr -> Ast.expr
