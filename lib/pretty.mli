(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pantagruel source pretty-printers. *)

val str_expr : Ast.expr -> string
val str_declaration : Ast.declaration -> string
val str_type_expr : Ast.type_expr -> string
val document_to_string : ?width:int -> Ast.document -> string
