(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Pure interpretation of solver results in query context. *)

type verification_result = {
  query : Smt.query;
  result : Solver_output.solver_result;
  passed : bool;
  message : string;
}

val format_unsat_core : string list -> (string * string) list -> string
val extract_label : Smt.query -> int -> string

val interpret_result :
  Smt.query -> Solver_output.solver_result -> verification_result

val extract_holds_clause : string -> string
val parse_preservation_info : string -> (string * int) option
val parse_bmc_index : string -> int option
val correlate_results : verification_result list -> verification_result list
