(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure recognition of Pantagruel module headers. *)

val parse_module_header_tokens :
  (unit -> Parser.token) -> (string, string) result
