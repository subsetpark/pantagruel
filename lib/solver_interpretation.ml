(* @archlint.module core
   @archlint.domain pantagruel.solver-interpretation *)

(** Pure interpretation of solver results in query context. *)

type verification_result = {
  query : Smt.query;
  result : Solver_output.solver_result;
  passed : bool;
  message : string;
}

(** Format unsat core names into human-readable output. Looks up each core name
    in the query's assertion_names map. *)
let format_unsat_core core_names assertion_names =
  match core_names with
  | [] -> ""
  | _ -> (
      let resolved =
        List.filter_map
          (fun name -> List.assoc_opt name assertion_names)
          core_names
      in
      match resolved with
      | [] -> ""
      | texts ->
          "\n  Conflicting constraints:\n"
          ^ String.concat "\n" (List.map (fun t -> "    " ^ t) texts))

(** Extract action label from query name (after the colon prefix) *)
let extract_label query prefix_len =
  String.sub query.Smt.name prefix_len
    (String.length query.Smt.name - prefix_len)

(** Interpret a solver result in the context of a query kind *)
let interpret_result query result =
  match (query.Smt.kind, result) with
  | Smt.InvariantConsistency, Solver_output.Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InvariantConsistency, Solver_output.Unsat core ->
      let core_text = format_unsat_core core query.assertion_names in
      let detail =
        if core_text = "" then
          "\n  No state can satisfy all invariants simultaneously."
        else core_text
      in
      {
        query;
        result;
        passed = false;
        message = "FAIL: Invariants are contradictory" ^ detail;
      }
  | Smt.Contradiction, Solver_output.Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.Contradiction, Solver_output.Unsat core ->
      let core_text = format_unsat_core core query.assertion_names in
      let detail =
        if core_text = "" then
          "\n  No state transition can satisfy all constraints simultaneously."
        else core_text
      in
      let label = extract_label query 14 in
      let has_prefix pfx =
        List.exists
          (fun name ->
            String.length name > String.length pfx
            && String.sub name 0 (String.length pfx) = pfx)
          core
      in
      let headline =
        if has_prefix "postcond" then
          Printf.sprintf "FAIL: Action '%s' postconditions are contradictory"
            label
        else Printf.sprintf "FAIL: Action '%s' is contradictory" label
      in
      { query; result; passed = false; message = headline ^ detail }
  | Smt.InvariantPreservation, Solver_output.Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InvariantPreservation, Solver_output.Sat values ->
      let action_label =
        let after_prefix =
          String.sub query.name 10 (String.length query.name - 10)
        in
        match String.rindex_opt after_prefix ':' with
        | Some i -> String.sub after_prefix 0 i
        | None -> after_prefix
      in
      let inv_desc =
        if query.invariant_text <> "" then
          Printf.sprintf " '%s'" query.invariant_text
        else ""
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Invariant%s may be violated by action '%s'%s"
            inv_desc action_label
            (Solver_output.format_counterexample values);
      }
  | Smt.PreconditionSat, Solver_output.Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.PreconditionSat, Solver_output.Unsat core ->
      let core_text = format_unsat_core core query.assertion_names in
      let detail =
        if core_text = "" then
          "\n  Preconditions are unsatisfiable given the invariants."
        else core_text
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "WARNING: Action '%s' is unreachable%s"
            (extract_label query 13) detail;
      }
  | Smt.BMCDeadlock, Solver_output.Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.BMCDeadlock, Solver_output.Sat values ->
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Reachable deadlock - no action is enabled%s"
            (Solver_output.format_bmc_counterexample values);
      }
  | Smt.InitConsistency, Solver_output.Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InitConsistency, Solver_output.Unsat core ->
      let core_text = format_unsat_core core query.assertion_names in
      let detail =
        if core_text = "" then
          "\n  No state can satisfy all initial-state constraints."
        else core_text
      in
      {
        query;
        result;
        passed = false;
        message = "FAIL: Initial state is impossible" ^ detail;
      }
  | Smt.InitInvariant, Solver_output.Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InitInvariant, Solver_output.Sat values ->
      let inv_desc =
        if query.invariant_text <> "" then
          Printf.sprintf " '%s'" query.invariant_text
        else ""
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Invariant%s not satisfied in initial state%s"
            inv_desc
            (Solver_output.format_counterexample values);
      }
  | Smt.BMCInvariant, Solver_output.Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.BMCInvariant, Solver_output.Sat values ->
      let inv_desc =
        if query.invariant_text <> "" then
          Printf.sprintf " '%s'" query.invariant_text
        else ""
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf
            "FAIL: Invariant%s violated in reachable state from initial state%s"
            inv_desc
            (Solver_output.format_bmc_counterexample values);
      }
  | Smt.CondExhaustiveness, Solver_output.Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.CondExhaustiveness, Solver_output.Sat values ->
      let cond_desc =
        if query.invariant_text <> "" then
          Printf.sprintf " '%s'" query.invariant_text
        else ""
      in
      let cx =
        match values with
        | [] -> ""
        | _ -> (
            let parse_applied term =
              if String.length term >= 2 && term.[0] = '(' then
                let inner = String.sub term 1 (String.length term - 2) in
                match String.index_opt inner ' ' with
                | Some i ->
                    let fname = String.sub inner 0 i in
                    let arg =
                      String.sub inner (i + 1) (String.length inner - i - 1)
                    in
                    Some (fname, arg)
                | None -> None
              else None
            in
            let by_elem = Hashtbl.create 16 in
            List.iter
              (fun (term, value) ->
                match parse_applied term with
                | Some (fname, elem) ->
                    let prev =
                      Option.value ~default:[] (Hashtbl.find_opt by_elem elem)
                    in
                    Hashtbl.replace by_elem elem ((fname, value) :: prev)
                | None -> ())
              values;
            let best_elem =
              Hashtbl.fold
                (fun _elem fvs best ->
                  let max_val =
                    List.fold_left
                      (fun acc (_, v) ->
                        match int_of_string_opt v with
                        | Some n -> max acc n
                        | None -> acc)
                      min_int fvs
                  in
                  match best with
                  | Some (_, prev_max) when prev_max >= max_val -> best
                  | _ -> Some (fvs, max_val))
                by_elem None
            in
            match best_elem with
            | None -> ""
            | Some (fvs, _) ->
                let lines =
                  List.rev_map
                    (fun (fname, value) ->
                      Printf.sprintf "    %s = %s"
                        (Solver_terms.translate_display_name fname)
                        (Solver_terms.translate_value value))
                    fvs
                in
                "\n  Counterexample:\n" ^ String.concat "\n" lines)
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Cond expression%s may not be exhaustive%s"
            cond_desc cx;
      }
  | Smt.Entailment, Solver_output.Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.Entailment, Solver_output.Sat values ->
      let goal_desc =
        if query.invariant_text <> "" then
          Printf.sprintf " '%s'" query.invariant_text
        else ""
      in
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Not entailed:%s%s" goal_desc
            (Solver_output.format_counterexample values);
      }
  | _, Solver_output.Unknown reason ->
      {
        query;
        result;
        passed = true;
        message =
          Printf.sprintf "UNKNOWN: %s (reason: %s)" query.description reason;
      }
  | _, Solver_output.SolverError err ->
      {
        query;
        result;
        passed = false;
        message = Printf.sprintf "ERROR: %s: %s" query.description err;
      }

(** Extract "holds for N steps from initial state" from a BMC description *)
let extract_holds_clause desc =
  let key = "holds for " in
  let key_len = String.length key in
  let desc_len = String.length desc in
  let rec find i =
    if i > desc_len - key_len then desc
    else if String.sub desc i key_len = key then String.sub desc i (desc_len - i)
    else find (i + 1)
  in
  find 0

(** Parse invariant index from preservation query name
    "invariant:<action>:<index>" *)
let parse_preservation_info name =
  if String.length name > 10 && String.sub name 0 10 = "invariant:" then
    let rest = String.sub name 10 (String.length name - 10) in
    match String.rindex_opt rest ':' with
    | Some i -> (
        let action = String.sub rest 0 i in
        let idx_str = String.sub rest (i + 1) (String.length rest - i - 1) in
        try Some (action, int_of_string idx_str) with Failure _ -> None)
    | None -> None
  else None

(** Parse invariant index from BMC query name "bmc-invariant:<index>" *)
let parse_bmc_index name =
  if String.length name > 14 && String.sub name 0 14 = "bmc-invariant:" then
    try Some (int_of_string (String.sub name 14 (String.length name - 14)))
    with Failure _ -> None
  else None

(** Correlate BMC results with invariant preservation results. When a
    preservation check fails but BMC passes, it's a false alarm (WARN). When
    both fail, BMC provides the concrete reachable trace. Standalone BMC results
    are removed from the output. *)
let correlate_results results =
  let bmc_map = Hashtbl.create 8 in
  List.iter
    (fun (r : verification_result) ->
      match parse_bmc_index r.query.Smt.name with
      | Some idx -> Hashtbl.replace bmc_map idx r
      | None -> ())
    results;
  if Hashtbl.length bmc_map = 0 then results
  else
    List.filter_map
      (fun (r : verification_result) ->
        match parse_bmc_index r.query.Smt.name with
        | Some _ -> None
        | None -> (
            match parse_preservation_info r.query.Smt.name with
            | Some (action_label, idx) when not r.passed -> (
                match Hashtbl.find_opt bmc_map idx with
                | Some bmc_r -> (
                    let inv_desc =
                      if r.query.invariant_text <> "" then
                        Printf.sprintf " '%s'" r.query.invariant_text
                      else ""
                    in
                    let values =
                      match r.result with
                      | Solver_output.Sat v -> v
                      | Unsat _ | Unknown _ | SolverError _ -> []
                    in
                    match bmc_r.result with
                    | Unsat _ ->
                        let holds =
                          extract_holds_clause bmc_r.query.description
                        in
                        let cx = Solver_output.format_counterexample values in
                        let cx_str =
                          if cx = "" then ""
                          else "\n  Counterexample (possibly unreachable):" ^ cx
                        in
                        Some
                          {
                            r with
                            passed = true;
                            message =
                              Printf.sprintf
                                "WARN: Invariant%s not preserved by action \
                                 '%s' (but %s)%s"
                                inv_desc action_label holds cx_str;
                          }
                    | Sat bmc_values ->
                        let cx = Solver_output.format_counterexample values in
                        Some
                          {
                            r with
                            message =
                              Printf.sprintf
                                "FAIL: Invariant%s violated by action '%s' \
                                 (reachable from initial state)%s\n\
                                \  Reachable trace:%s"
                                inv_desc action_label cx
                                (Solver_output.format_bmc_counterexample
                                   bmc_values);
                          }
                    | Unknown _ | SolverError _ -> Some r)
                | None -> Some r)
            | Some _ | None -> Some r))
      results
