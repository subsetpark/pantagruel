(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Rich Markdown rendering for Pantagruel documents. *)

val expression_to_markdown : Env.t -> Ast.expr -> string
val declaration_to_markdown : Env.t -> Ast.declaration -> string
val document_to_markdown : Env.t -> Ast.document -> string
