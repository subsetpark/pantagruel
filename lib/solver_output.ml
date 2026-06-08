(* @archlint.module core
   @archlint.domain pantagruel.solver-output *)

(** Pure SMT solver output parsing and display formatting. *)

module Sexp = Sexplib0.Sexp

type solver_result =
  | Sat of (string * string) list  (** SAT with parsed value assignments *)
  | Unsat of string list  (** UNSAT with unsat core names (empty if no core) *)
  | Unknown of string
  | SolverError of string

let sexp_to_string = Sexp.to_string

(** Try to parse a string as a list of s-expressions. Returns an empty list on
    parse failure. *)
let parse_sexps s =
  match Parsexp.Many.parse_string s with Ok sexps -> sexps | Error _ -> []

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
          | Atom _ | List _ -> None)
        pairs
  | Atom _ -> []

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
        (fun (a : Sexp.t) -> match a with Atom s -> Some s | List _ -> None)
        atoms
  | Atom _ -> []

(** Parse an unsat core response from a raw string. Kept for test compatibility.
*)
let parse_unsat_core output =
  match parse_sexps output with
  | sexp :: _ -> parse_unsat_core_sexp sexp
  | [] -> []

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
        match Solver_terms.classify_term term with
        | After -> (
            match Solver_terms.unprime_term term with
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
              if rest = "" then Solver_terms.add_prime_suffix fname
              else "(" ^ Solver_terms.add_prime_suffix fname ^ rest ^ ")"
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
            if Solver_terms.classify_term t = ActionParam then
              Some (Solver_terms.translate_value v)
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
                (Solver_terms.translate_display_name term)
                (Solver_terms.translate_value value)
            in
            match Solver_terms.classify_term term with
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

(** Format parsed value pairs into a BMC counterexample grouped by step. Parses
    _sN suffix from value term names to determine step index. *)
let format_bmc_counterexample values =
  match values with
  | [] -> ""
  | _ ->
      let parse_step term =
        let fname =
          if String.length term >= 2 && term.[0] = '(' then
            let inner = String.sub term 1 (String.length term - 2) in
            match String.index_opt inner ' ' with
            | Some i -> String.sub inner 0 i
            | None -> inner
          else term
        in
        match String.rindex_opt fname '_' with
        | Some i when i + 1 < String.length fname && fname.[i + 1] = 's' -> (
            let num_str =
              String.sub fname (i + 2) (String.length fname - i - 2)
            in
            try Some (int_of_string num_str) with Failure _ -> None)
        | _ -> None
      in
      let display_name term =
        let strip_step s =
          match String.rindex_opt s '_' with
          | Some i when i + 1 < String.length s && s.[i + 1] = 's' -> (
              let num_str = String.sub s (i + 2) (String.length s - i - 2) in
              try
                ignore (int_of_string num_str);
                String.sub s 0 i
              with Failure _ -> s)
          | _ -> s
        in
        if String.length term >= 2 && term.[0] = '(' then
          let inner = String.sub term 1 (String.length term - 2) in
          match String.index_opt inner ' ' with
          | Some i ->
              let fname = String.sub inner 0 i in
              let rest = String.sub inner i (String.length inner - i) in
              strip_step fname ^ rest
          | None -> strip_step inner
        else strip_step term
      in
      let step_map = Hashtbl.create 16 in
      List.iter
        (fun (term, value) ->
          match parse_step term with
          | Some step ->
              let existing =
                match Hashtbl.find_opt step_map step with
                | Some l -> l
                | None -> []
              in
              Hashtbl.replace step_map step
                ((display_name term, Solver_terms.translate_value value)
                :: existing)
          | None -> ())
        values;
      let steps =
        Hashtbl.fold (fun k v acc -> (k, v) :: acc) step_map []
        |> List.sort (fun (a, _) (b, _) -> compare a b)
      in
      let buf = Buffer.create 256 in
      List.iter
        (fun (step, entries) ->
          Buffer.add_string buf (Printf.sprintf "\n  Step %d:\n" step);
          List.iter
            (fun (name, value) ->
              Buffer.add_string buf (Printf.sprintf "    %s = %s\n" name value))
            (List.rev entries))
        steps;
      Buffer.contents buf

(** Extract the reason-unknown string from a (get-info :reason-unknown) response
    sexp: (:reason-unknown "..."). Returns None if not such a sexp or if the
    reason is empty. *)
let extract_reason_unknown (sexp : Sexp.t) =
  match sexp with
  | List [ Atom ":reason-unknown"; Atom reason ] when String.trim reason <> ""
    ->
      Some (String.trim reason)
  | List _ | Atom _ -> None

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
              match s with
              | List (Atom "error" :: _) | List (Atom ":reason-unknown" :: _) ->
                  None
              | List _ -> Some (parse_get_value_sexp s)
              | Atom _ -> None)
            rest
          |> Option.value ~default:[]
        in
        Sat values
    | Atom "unsat" :: rest ->
        let core =
          List.find_map
            (fun (s : Sexp.t) ->
              match s with
              | List (Atom "error" :: _) | List (Atom ":reason-unknown" :: _) ->
                  None
              | List _ -> Some (parse_unsat_core_sexp s)
              | Atom _ -> None)
            rest
          |> Option.value ~default:[]
        in
        Unsat core
    | Atom "unknown" :: rest ->
        let reason = List.find_map extract_reason_unknown rest in
        Unknown (Option.value reason ~default:"unknown")
    | List (Atom "error" :: _) :: _ -> SolverError output
    | List _ :: _ | Atom _ :: _ | [] ->
        SolverError (Printf.sprintf "Unexpected solver output: %s" output)
