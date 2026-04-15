(** Structural sanity checks on emitted SMT-LIB2.

    Layer 1 of the SMT-translator test strategy: walks the sexp form of a
    generated query and reports failures of the structural invariants the
    translator is supposed to maintain. Pure OCaml, no solver required. *)

(** A failure-kind tag, used in the per-fixture allowlist
    ([test/regression/expected_failures.txt]). *)
type failure_kind =
  | Parse_error
  | Duplicate_binder
  | Vacuous_binder
  | Fallback_emission

val failure_kind_tag : failure_kind -> string
(** Stable string tag matching the allowlist file format. *)

val failure_kind_of_tag : string -> failure_kind option
(** Inverse of [failure_kind_tag]; returns [None] for unknown tags. *)

type failure = { kind : failure_kind; message : string }
(** A concrete failure with enough context to debug. *)

val format_failure : failure -> string

val parse_smt2 : string -> (Sexplib0.Sexp.t list, string) result
(** Parse an SMT-LIB2 string into its sexp form. Wraps [Parsexp.Many]. *)

val check_query : string -> failure list
(** Run all structural checks on a single SMT-LIB2 string. Failures are
    accumulated; the function does not short-circuit on the first error. *)

val check_queries : string list -> failure list
(** Run [check_query] on a list of query SMT-LIB2 strings, accumulating all
    failures. *)
