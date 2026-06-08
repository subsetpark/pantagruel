(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure title extraction for generated HTML documents. *)

val module_name_from_first_line : string -> string option
(** [module_name_from_first_line line] returns the module name when [line] is a
    Pantagruel module declaration of the form ["module NAME."]. *)
