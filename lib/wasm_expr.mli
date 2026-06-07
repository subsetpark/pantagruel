(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure expression transformations used by the WASM bindings. *)

val bound_names_from_guards : Ast.guard list -> string list
val rename_expr : (string * string) list -> Ast.expr -> Ast.expr
val free_vars : Ast.expr -> string list
val subst_var : string -> Ast.expr -> Ast.expr -> Ast.expr
