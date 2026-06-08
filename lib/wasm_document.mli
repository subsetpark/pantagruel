(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure document and expression services used by the WASM bindings. *)

val parse_expr_string : string -> (Ast.expr, string) result
val parse_document_string : string -> string -> (Ast.document, string) result
val check_document_string : string -> string option
val format_module_error : Module.module_error -> string
val check_document_with_deps : string -> (string * string) list -> string option
