(* @archlint.module core
   @archlint.domain pantagruel.solver-terms *)

(** Pure SMT solver term/name display helpers. *)

module Sexp = Sexplib0.Sexp

let prime_suffix = "_prime"
let has_prime_suffix s = String.ends_with ~suffix:prime_suffix s

let strip_prime_suffix s =
  if not (has_prime_suffix s) then
    invalid_arg "strip_prime_suffix: missing prime suffix";
  String.sub s 0 (String.length s - String.length prime_suffix)

let add_prime_suffix s = s ^ prime_suffix

(** Convert an s-expression back to its string representation. Used to produce
    the string keys/values that downstream display code expects. *)
let sexp_to_string = Sexp.to_string

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
  | List _ -> sexp_to_string sexp

(** Translate a raw string value for display. Kept for backward compatibility
    with format_counterexample. *)
let translate_value value =
  match Parsexp.Single.parse_string value with
  | Ok sexp -> translate_value_sexp sexp
  | Error _ -> value

(** Translate SMT names back to Pantagruel-friendly display names.
    - Replaces _prime suffix with ' (for example, "balance_prime" -> "balance'")
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
  if
    String.length term >= 2
    && term.[0] = '('
    && term.[String.length term - 1] = ')'
  then
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

(** Find the "unprime" counterpart of a _prime term. Parenthesized inputs are
    expected to be well-formed solver-generated applied terms, matching
    [classify_term]'s list-shape handling. Malformed inputs are treated as
    non-list terms and may return [None]. *)
let unprime_term term =
  if
    String.length term >= 2
    && term.[0] = '('
    && term.[String.length term - 1] = ')'
  then
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
