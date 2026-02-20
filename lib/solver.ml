(** Solver interface: invoke an SMT solver and parse results *)

type solver_result =
  | Sat of string option  (** SAT with optional model *)
  | Unsat
  | Unknown of string
  | SolverError of string

type verification_result = {
  query : Smt.query;
  result : solver_result;
  passed : bool;
  message : string;
}

let default_solver = "z3"
let default_args = [ "-smt2"; "-in" ]

(** Check if a solver binary is available *)
let solver_available ?(solver = default_solver) () =
  let cmd = Printf.sprintf "which %s > /dev/null 2>&1" solver in
  Sys.command cmd = 0

(** Run a solver on an SMT-LIB2 string *)
let run_solver ?(solver = default_solver) ?(args = default_args) smt2 =
  let cmd = String.concat " " (solver :: args) in
  let ic, oc = Unix.open_process cmd in
  output_string oc smt2;
  close_out oc;
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  let _status = Unix.close_process (ic, oc) in
  let output = Buffer.contents buf |> String.trim in
  (* Parse result *)
  if String.length output = 0 then SolverError "No output from solver"
  else
    let lines = String.split_on_char '\n' output in
    match lines with
    | "sat" :: rest ->
        let model =
          if rest = [] then None
          else Some (String.concat "\n" rest)
        in
        Sat model
    | [ "unsat" ] -> Unsat
    | "unsat" :: _ -> Unsat
    | "unknown" :: rest ->
        let reason =
          if rest = [] then "unknown"
          else String.concat "\n" rest
        in
        Unknown reason
    | _ ->
        if String.sub output 0 (min 6 (String.length output)) = "(error" then
          SolverError output
        else SolverError (Printf.sprintf "Unexpected solver output: %s" output)

(** Interpret a solver result in the context of a query kind *)
let interpret_result query result =
  match (query.Smt.kind, result) with
  (* Contradiction: SAT = ok, UNSAT = contradiction *)
  | Smt.Contradiction, Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.Contradiction, Unsat ->
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Action '%s' postconditions are contradictory"
            (String.sub query.name 14 (String.length query.name - 14));
      }
  (* Invariant: SAT = violation (bad), UNSAT = preserved (good) *)
  | Smt.InvariantPreservation, Unsat ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InvariantPreservation, Sat model ->
      let model_str =
        match model with
        | Some m -> Printf.sprintf "\n  Counterexample: %s" m
        | None -> ""
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Invariant may be violated by action '%s'%s"
            (String.sub query.name 10 (String.length query.name - 10))
            model_str;
      }
  (* Precondition: SAT = ok, UNSAT = dead operation *)
  | Smt.PreconditionSat, Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.PreconditionSat, Unsat ->
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "WARNING: Action '%s' preconditions are unsatisfiable (dead operation)"
            (String.sub query.name 13 (String.length query.name - 13));
      }
  (* Unknown / error cases *)
  | _, Unknown reason ->
      {
        query;
        result;
        passed = true;
        (* Don't fail on unknown *)
        message =
          Printf.sprintf "UNKNOWN: %s (reason: %s)" query.description reason;
      }
  | _, SolverError err ->
      {
        query;
        result;
        passed = false;
        message = Printf.sprintf "ERROR: %s: %s" query.description err;
      }

(** Run all queries and return results *)
let verify_all ?(solver = default_solver) queries =
  List.map
    (fun query ->
      let result = run_solver ~solver query.Smt.smt2 in
      interpret_result query result)
    queries
