(** Solver interface: invoke an SMT solver and parse results *)

module Sexp = Sexplib0.Sexp

type solver_result =
  | Sat of (string * string) list  (** SAT with parsed value assignments *)
  | Unsat of string list  (** UNSAT with unsat core names (empty if no core) *)
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
let default_timeout = 30.0
let prime_suffix = "_prime"
let has_prime_suffix s = String.ends_with ~suffix:prime_suffix s

let strip_prime_suffix s =
  String.sub s 0 (String.length s - String.length prime_suffix)

let add_prime_suffix s = s ^ prime_suffix

(** Check if a solver binary is available *)
let solver_available ?(solver = default_solver) () =
  let is_executable path =
    try
      Unix.access path [ Unix.X_OK ];
      true
    with Unix.Unix_error _ -> false
  in
  if String.contains solver '/' then is_executable solver
  else
    match Sys.getenv_opt "PATH" with
    | None -> false
    | Some path ->
        String.split_on_char ':' path
        |> List.exists (fun dir -> is_executable (Filename.concat dir solver))

(** Try to parse a string as a list of s-expressions. Returns an empty list on
    parse failure. *)
let parse_sexps s =
  match Parsexp.Many.parse_string s with Ok sexps -> sexps | Error _ -> []

(** Convert an s-expression back to its string representation. Used to produce
    the string keys/values that downstream display code expects. *)
let sexp_to_string = Sexp.to_string

(** Parse a (get-value ...) response. Input is a single sexp: ((term1 value1)
    (term2 value2) ...) Returns a list of (term, value) string pairs. *)
let parse_get_value_sexp (sexp : Sexp.t) =
  match sexp with
  | List pairs ->
      List.filter_map
        (fun (pair : Sexp.t) ->
          match pair with
          | List [ term; value ] ->
              Some (sexp_to_string term, sexp_to_string value)
          | _ -> None)
        pairs
  | _ -> []

(** Parse a (get-value ...) response from a raw string. Kept for test
    compatibility. *)
let parse_get_value output =
  match parse_sexps output with
  | [ sexp ] -> parse_get_value_sexp sexp
  | _ -> []

(** Extract atom names from an unsat core sexp: (name1 name2 ...) *)
let parse_unsat_core_sexp (sexp : Sexp.t) =
  match sexp with
  | List atoms ->
      List.filter_map
        (fun (a : Sexp.t) -> match a with Atom s -> Some s | _ -> None)
        atoms
  | _ -> []

(** Parse an unsat core response from a raw string. Kept for test compatibility.
*)
let parse_unsat_core output =
  match parse_sexps output with
  | sexp :: _ -> parse_unsat_core_sexp sexp
  | [] -> []

(** Translate an s-expression value to a display string.
    - z3 negation: (- N) -> "-N"
    - z3 internal domain names: Domain!val!N -> Domain_N *)
let translate_value_sexp (sexp : Sexp.t) =
  match sexp with
  | List [ Atom "-"; Atom n ] -> "-" ^ n
  | Atom s -> (
      match String.split_on_char '!' s with
      | domain :: "val" :: [ n ] -> domain ^ "_" ^ n
      | _ -> s)
  | _ -> sexp_to_string sexp

(** Translate a raw string value for display. Kept for backward compatibility
    with format_counterexample. *)
let translate_value value =
  match Parsexp.Single.parse_string value with
  | Ok sexp -> translate_value_sexp sexp
  | Error _ -> value

(** Translate SMT names back to Pantagruel-friendly display names.
    - Replaces _prime suffix with ' (e.g., "balance_prime" -> "balance'")
    - Strips parens from applied terms for readability *)
let translate_display_name term =
  (* Replace _prime with ' *)
  let replace_prime s =
    match String.split_on_char ' ' s with
    | [] -> s
    | parts ->
        String.concat " "
          (List.map
             (fun p ->
               if has_prime_suffix p then strip_prime_suffix p ^ "'" else p)
             parts)
  in
  let name = replace_prime term in
  (* Strip outer parens for display: "(balance a)" -> "balance a" *)
  if
    String.length name >= 2
    && name.[0] = '('
    && name.[String.length name - 1] = ')'
  then String.sub name 1 (String.length name - 2)
  else name

(** Classify a value term into a group *)
type value_group = Before | After | ActionParam

let classify_term term =
  if String.length term >= 2 && term.[0] = '(' then
    (* Applied term like "(balance a)" or "(balance_prime a)" *)
    let inner = String.sub term 1 (String.length term - 2) in
    let fname =
      match String.index_opt inner ' ' with
      | Some i -> String.sub inner 0 i
      | None -> inner
    in
    if has_prime_suffix fname then After else Before
  else if has_prime_suffix term then After
  else ActionParam

(** Find the "unprime" counterpart of a _prime term *)
let unprime_term term =
  if String.length term >= 2 && term.[0] = '(' then
    let inner = String.sub term 1 (String.length term - 2) in
    match String.index_opt inner ' ' with
    | Some i ->
        let fname = String.sub inner 0 i in
        let rest = String.sub inner i (String.length inner - i) in
        if has_prime_suffix fname then
          Some ("(" ^ strip_prime_suffix fname ^ rest ^ ")")
        else None
    | None ->
        if has_prime_suffix inner then
          Some ("(" ^ strip_prime_suffix inner ^ ")")
        else None
  else if has_prime_suffix term then Some (strip_prime_suffix term)
  else None

(** Format parsed value pairs into a grouped counterexample block. Groups values
    into Before/Action/After and filters unchanged values. *)
let format_counterexample values =
  match values with
  | [] -> ""
  | _ ->
      (* Build a map from term -> value for quick lookup *)
      let value_map = List.to_seq values |> Hashtbl.of_seq in
      (* Filter out unchanged values: if f_prime x = f x, omit both *)
      let is_unchanged term value =
        match classify_term term with
        | After -> (
            match unprime_term term with
            | Some base -> (
                match Hashtbl.find_opt value_map base with
                | Some base_value -> base_value = value
                | None -> false)
            | None -> false)
        | Before -> (
            (* Check if there's a primed version with same value *)
            let inner =
              if String.length term >= 2 && term.[0] = '(' then
                String.sub term 1 (String.length term - 2)
              else term
            in
            let fname =
              match String.index_opt inner ' ' with
              | Some i -> String.sub inner 0 i
              | None -> inner
            in
            let rest =
              match String.index_opt inner ' ' with
              | Some i -> String.sub inner i (String.length inner - i)
              | None -> ""
            in
            let primed =
              if rest = "" then add_prime_suffix fname
              else "(" ^ add_prime_suffix fname ^ rest ^ ")"
            in
            match Hashtbl.find_opt value_map primed with
            | Some primed_value -> primed_value = value
            | None -> false)
        | ActionParam -> false
      in
      let filtered =
        List.filter (fun (t, v) -> not (is_unchanged t v)) values
      in
      (* Deduplicate: if action param a = Account_0, then (balance Account_0)
         is redundant with (balance a). Build set of param-value aliases. *)
      let param_values =
        List.filter_map
          (fun (t, v) ->
            if classify_term t = ActionParam then Some (translate_value v)
            else None)
          values
      in
      let is_redundant_elem_term term =
        (* Check if term is an applied form like "(f Domain_N)" where
           Domain_N is the resolved value of some action parameter *)
        if String.length term >= 2 && term.[0] = '(' then
          let inner = String.sub term 1 (String.length term - 2) in
          match String.index_opt inner ' ' with
          | Some i ->
              let args =
                String.sub inner (i + 1) (String.length inner - i - 1)
              in
              List.mem args param_values
          | None -> false
        else false
      in
      let filtered =
        List.filter (fun (t, _) -> not (is_redundant_elem_term t)) filtered
      in
      (* Group into categories *)
      let action_params, before, after =
        List.fold_left
          (fun (ap, b, a) (term, value) ->
            let line =
              Printf.sprintf "    %s = %s"
                (translate_display_name term)
                (translate_value value)
            in
            match classify_term term with
            | ActionParam -> (line :: ap, b, a)
            | Before -> (ap, line :: b, a)
            | After -> (ap, b, line :: a))
          ([], [], []) filtered
      in
      let sections = ref [] in
      if after <> [] then
        sections := ("  After:" :: List.rev after) :: !sections;
      if action_params <> [] then
        sections := ("  Action:" :: List.rev action_params) :: !sections;
      if before <> [] then
        sections := ("  Before:" :: List.rev before) :: !sections;
      "\n" ^ String.concat "\n" (List.concat !sections)

(** Read all data from a file descriptor with a total deadline. Returns [None]
    on timeout, [Some output] on completion. *)
let read_with_timeout fd timeout =
  let buf = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then None
    else
      match Unix.select [ fd ] [] [] remaining with
      | [], _, _ -> None
      | _ ->
          let n = Unix.read fd chunk 0 (Bytes.length chunk) in
          if n = 0 then Some (Buffer.contents buf |> String.trim)
          else (
            Buffer.add_subbytes buf chunk 0 n;
            loop ())
  in
  loop ()

(** Parse raw solver output into a [solver_result]. *)
let parse_solver_output output =
  if String.length output = 0 then SolverError "No output from solver"
  else
    let sexps = parse_sexps output in
    match sexps with
    | Atom "sat" :: rest ->
        let values =
          List.find_map
            (fun (s : Sexp.t) ->
              match s with List _ -> Some (parse_get_value_sexp s) | _ -> None)
            rest
          |> Option.value ~default:[]
        in
        Sat values
    | Atom "unsat" :: rest ->
        let core =
          List.find_map
            (fun (s : Sexp.t) ->
              match s with
              | List (Atom "error" :: _) -> None
              | List _ -> Some (parse_unsat_core_sexp s)
              | _ -> None)
            rest
          |> Option.value ~default:[]
        in
        Unsat core
    | Atom "unknown" :: rest ->
        let reason = String.concat " " (List.map sexp_to_string rest) in
        Unknown (if reason = "" then "unknown" else reason)
    | List (Atom "error" :: _) :: _ -> SolverError output
    | _ -> SolverError (Printf.sprintf "Unexpected solver output: %s" output)

(** Run a solver on an SMT-LIB2 string *)
let run_solver ?(solver = default_solver) ?(args = default_args)
    ?(timeout = default_timeout) smt2 =
  let argv = Array.of_list (solver :: args) in
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process solver argv stdin_read stdout_write Unix.stderr
  in
  Unix.close stdin_read;
  Unix.close stdout_write;
  (* Write SMT input and close stdin so the solver sees EOF *)
  let input_bytes = Bytes.of_string smt2 in
  let len = Bytes.length input_bytes in
  let rec write_all off =
    if off < len then
      let n = Unix.write stdin_write input_bytes off (len - off) in
      write_all (off + n)
  in
  (try write_all 0 with Unix.Unix_error _ -> ());
  Unix.close stdin_write;
  (* Read output with timeout *)
  let result =
    match read_with_timeout stdout_read timeout with
    | None ->
        (* Timeout: kill the solver process *)
        (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        Unix.close stdout_read;
        SolverError (Printf.sprintf "Solver timed out after %.0fs" timeout)
    | Some output -> (
        Unix.close stdout_read;
        let _pid, status = Unix.waitpid [] pid in
        match status with
        | Unix.WEXITED 0 -> parse_solver_output output
        | Unix.WEXITED code ->
            if String.length output > 0 then parse_solver_output output
            else SolverError (Printf.sprintf "Solver exited with code %d" code)
        | Unix.WSIGNALED sig_num ->
            SolverError (Printf.sprintf "Solver killed by signal %d" sig_num)
        | Unix.WSTOPPED sig_num ->
            SolverError (Printf.sprintf "Solver stopped by signal %d" sig_num))
  in
  result

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
  (* Invariant consistency: SAT = ok, UNSAT = contradiction *)
  | Smt.InvariantConsistency, Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InvariantConsistency, Unsat core ->
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
  (* Contradiction: SAT = ok, UNSAT = contradiction *)
  | Smt.Contradiction, Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.Contradiction, Unsat core ->
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
  (* Invariant: SAT = violation (bad), UNSAT = preserved (good) *)
  | Smt.InvariantPreservation, Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.InvariantPreservation, Sat values ->
      let action_label =
        (* name is "invariant:<action>:<index>" — extract action label *)
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
            (format_counterexample values);
      }
  (* Precondition: SAT = ok, UNSAT = dead operation *)
  | Smt.PreconditionSat, Sat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.PreconditionSat, Unsat core ->
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
  (* Deadlock freedom: SAT = deadlock (bad), UNSAT = ok *)
  | Smt.DeadlockFreedom, Unsat _ ->
      {
        query;
        result;
        passed = true;
        message = Printf.sprintf "OK: %s" query.description;
      }
  | Smt.DeadlockFreedom, Sat values ->
      {
        query;
        result;
        passed = false;
        message =
          Printf.sprintf "FAIL: Potential deadlock — no action is enabled%s"
            (format_counterexample values);
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
let verify_all ?(solver = default_solver) ?(timeout = default_timeout) queries =
  List.map
    (fun query ->
      let result = run_solver ~solver ~timeout query.Smt.smt2 in
      interpret_result query result)
    queries
