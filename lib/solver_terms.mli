(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure SMT solver term/name display helpers. *)

val prime_suffix : string
val has_prime_suffix : string -> bool
val strip_prime_suffix : string -> string
val add_prime_suffix : string -> string
val translate_value_sexp : Sexplib0.Sexp.t -> string
val translate_value : string -> string
val translate_display_name : string -> string

type value_group = Before | After | ActionParam

val classify_term : string -> value_group
val unprime_term : string -> string option
