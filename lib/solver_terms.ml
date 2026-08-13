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

let strip_prefix ~prefix s =
  if String.starts_with ~prefix s then
    Some
      (String.sub s (String.length prefix)
         (String.length s - String.length prefix))
  else None

let split_component s =
  match String.index_opt s '$' with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> (s, "")

let decode_component encoded =
  Smt_types.decode_ident encoded |> Option.value ~default:encoded

(** Decode one generated SMT atom back to its Pantagruel spelling. Unknown and
    solver-owned atoms pass through unchanged. *)
let decode_symbol atom =
  let base, primed =
    if has_prime_suffix atom then (strip_prime_suffix atom, true)
    else (atom, false)
  in
  let decoded =
    match strip_prefix ~prefix:"pant$v$" base with
    | Some encoded -> decode_component encoded
    | None -> (
        match strip_prefix ~prefix:"pant$r$" base with
        | Some rest ->
            let encoded, _suffix = split_component rest in
            decode_component encoded
        | None -> (
            match strip_prefix ~prefix:"pant$q$" base with
            | Some rest ->
                let encoded_module, rest = split_component rest in
                let encoded_name, _suffix = split_component rest in
                decode_component encoded_module
                ^ "::"
                ^ decode_component encoded_name
            | None -> base))
  in
  if primed then decoded ^ "'" else decoded

let decode_domain_value atom =
  match strip_prefix ~prefix:"pant$e$" atom with
  | Some rest ->
      let encoded, index = split_component rest in
      decode_component encoded ^ "_" ^ index
  | None -> (
      match String.split_on_char '!' atom with
      | encoded_sort :: "val" :: [ index ] -> (
          match strip_prefix ~prefix:"pant$d$" encoded_sort with
          | Some encoded -> decode_component encoded ^ "_" ^ index
          | None -> encoded_sort ^ "_" ^ index)
      | _ -> atom)

(** Convert an s-expression back to its string representation. Used to produce
    the string keys/values that downstream display code expects. *)
let sexp_to_string = Sexp.to_string

(** Translate an s-expression value to a display string.
    - z3 negation: (- N) -> "-N"
    - z3 internal domain names: Domain!val!N -> Domain_N *)
let translate_value_sexp (sexp : Sexp.t) =
  match sexp with
  | List [ Atom "-"; Atom n ] -> "-" ^ n
  | Atom s -> decode_domain_value s
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
  let rec decode_sexp = function
    | Sexp.Atom atom -> Sexp.Atom (decode_symbol (decode_domain_value atom))
    | Sexp.List items -> Sexp.List (List.map decode_sexp items)
  in
  let name =
    match Parsexp.Single.parse_string term with
    | Ok sexp -> sexp_to_string (decode_sexp sexp)
    | Error _ -> decode_symbol term
  in
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
