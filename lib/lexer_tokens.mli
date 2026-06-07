(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure token classification and display helpers. *)

val keyword_or_lower_ident : string -> Parser.token
val string_of_token : Parser.token -> string
val describe_token : Parser.token -> string
val all_tokens : Parser.token list
