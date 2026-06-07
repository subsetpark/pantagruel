(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure formatting for CLI-facing module errors. *)

val format_module_error : Module.module_error -> string
