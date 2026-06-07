(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure keys used by doc comment storage. *)

val key : int -> int -> int * int
val key_at_pos : Lexing.position -> int * int
