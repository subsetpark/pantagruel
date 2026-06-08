(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure SMT solver term/name display helpers. *)

val prime_suffix : string
(** Literal suffix used for post-state SMT names: ["_prime"]. *)

val has_prime_suffix : string -> bool
(** [has_prime_suffix s] is [true] when [s] ends with [prime_suffix]. *)

val strip_prime_suffix : string -> string
(** [strip_prime_suffix s] removes [prime_suffix].
    @raise Invalid_argument if [s] does not end with [prime_suffix]. *)

val add_prime_suffix : string -> string
(** [add_prime_suffix s] appends [prime_suffix] to [s]. *)

val translate_value_sexp : Sexplib0.Sexp.t -> string
(** [translate_value_sexp sexp] renders solver values for display, including Z3
    negation forms such as [(- 1)] and internal domain atoms such as
    [User!val!0]. *)

val translate_value : string -> string
(** [translate_value value] parses and renders a raw solver value string with
    [translate_value_sexp], returning the original string if parsing fails. *)

val translate_display_name : string -> string
(** [translate_display_name term] renders SMT term names as Pantagruel-facing
    names by converting [_prime] to a trailing quote and removing surrounding
    parentheses from applied terms. *)

(** Classification for model values: [Before] is pre-state rule data, [After] is
    post-state rule data, and [ActionParam] is an action parameter or other
    unprimed bare value. *)
type value_group = Before | After | ActionParam

val classify_term : string -> value_group
(** [classify_term term] classifies bare or parenthesized solver-generated terms
    using [prime_suffix]. Parenthesized terms are expected to be well-formed
    applied S-expressions such as [(balance a)] or [(balance_prime a)];
    malformed parenthesized strings are treated as non-list terms. *)

val unprime_term : string -> string option
(** [unprime_term term] returns the unprimed counterpart of a primed bare or
    applied term, or [None] when [term] is not a prime form. This complements
    [classify_term] by exposing the base identifier used for before/after value
    comparison. *)
