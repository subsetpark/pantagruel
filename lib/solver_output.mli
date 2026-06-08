(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure SMT solver output parsing and display formatting. *)

(** Result parsed from raw SMT solver stdout. *)
type solver_result =
  | Sat of (string * string) list
      (** Solver returned [sat]. Carries parsed [(variable_name, value)] pairs
          from a get-value response, or an empty list if no model values were
          emitted. *)
  | Unsat of string list
      (** Solver returned [unsat]. Carries unsat-core assertion identifiers, or
          an empty list when no core was emitted. *)
  | Unknown of string
      (** Solver returned [unknown]. Carries the solver-provided
          [:reason-unknown] explanation when present, otherwise ["unknown"]. *)
  | SolverError of string
      (** Solver invocation, runtime, or parse failure. Carries the diagnostic
          message or unrecognized raw output. *)

val parse_sexps : string -> Sexplib0.Sexp.t list
(** [parse_sexps output] parses raw solver stdout as zero or more S-expressions.
    It returns [[]] on malformed input instead of raising. *)

val parse_get_value_sexp : Sexplib0.Sexp.t -> (string * string) list
(** [parse_get_value_sexp sexp] parses an already-parsed get-value payload of
    the form [((term value) ...)]. Malformed pairs are ignored and non-list
    inputs return [[]]. *)

val parse_get_value : string -> (string * string) list
(** [parse_get_value output] parses raw get-value output by calling
    [parse_sexps] and then [parse_get_value_sexp] when exactly one S-expression
    is present. It returns [[]] on parse failure or unexpected shape. *)

val parse_unsat_core_sexp : Sexplib0.Sexp.t -> string list
(** [parse_unsat_core_sexp sexp] parses an already-parsed unsat-core payload of
    the form [(name ...)]. Non-atom entries are ignored and non-list inputs
    return [[]]. *)

val parse_unsat_core : string -> string list
(** [parse_unsat_core output] parses raw unsat-core output by calling
    [parse_sexps] and then [parse_unsat_core_sexp] on the first parsed
    S-expression. It returns [[]] on parse failure or empty input. *)

val format_counterexample : (string * string) list -> string
(** [format_counterexample values] formats parsed model values into grouped
    before/action/after display text. The input must already be parsed
    [(term, value)] pairs. *)

val format_bmc_counterexample : (string * string) list -> string
(** [format_bmc_counterexample values] formats parsed bounded-model-checking
    model values by step using solver-generated term names. *)

val extract_reason_unknown : Sexplib0.Sexp.t -> string option
(** [extract_reason_unknown sexp] extracts a non-empty reason from an
    already-parsed [(:reason-unknown reason)] response. It returns [None] for
    other shapes. *)

val parse_solver_output : string -> solver_result
(** [parse_solver_output output] parses complete raw solver stdout into a
    [solver_result]. It does not raise for malformed solver output; unsupported
    or unparseable output is returned as [SolverError]. *)
