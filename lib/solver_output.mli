(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure SMT solver output parsing and display formatting. *)

type solver_result =
  | Sat of (string * string) list
  | Unsat of string list
  | Unknown of string
  | SolverError of string

val parse_sexps : string -> Sexplib0.Sexp.t list
val parse_get_value_sexp : Sexplib0.Sexp.t -> (string * string) list
val parse_get_value : string -> (string * string) list
val parse_unsat_core_sexp : Sexplib0.Sexp.t -> string list
val parse_unsat_core : string -> string list
val format_counterexample : (string * string) list -> string
val format_bmc_counterexample : (string * string) list -> string
val extract_reason_unknown : Sexplib0.Sexp.t -> string option
val parse_solver_output : string -> solver_result
