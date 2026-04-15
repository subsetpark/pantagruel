(** Structural sanity checks on emitted SMT-LIB2. See [smt_check.mli]. *)

module Sexp = Sexplib0.Sexp

type failure_kind =
  | Parse_error
  | Duplicate_binder
  | Vacuous_binder
  | Fallback_emission

let failure_kind_tag = function
  | Parse_error -> "parse_error"
  | Duplicate_binder -> "duplicate_binder"
  | Vacuous_binder -> "vacuous_binder"
  | Fallback_emission -> "fallback_emission"

let failure_kind_of_tag = function
  | "parse_error" -> Some Parse_error
  | "duplicate_binder" -> Some Duplicate_binder
  | "vacuous_binder" -> Some Vacuous_binder
  | "fallback_emission" -> Some Fallback_emission
  | _ -> None

type failure = { kind : failure_kind; message : string }

let format_failure { kind; message } =
  Printf.sprintf "[%s] %s" (failure_kind_tag kind) message

(* ------------------------------------------------------------------ *)
(* Sexp parsing — wraps Parsexp with a uniform error shape.            *)
(* ------------------------------------------------------------------ *)

let parse_smt2 (smt2 : string) : (Sexp.t list, string) result =
  match Parsexp.Many.parse_string smt2 with
  | Ok sexps -> Ok sexps
  | Error err -> Error (Parsexp.Parse_error.message err)

(* ------------------------------------------------------------------ *)
(* SMT-LIB2 keyword set — atoms in this set are not free variables.    *)
(* The set is conservative: anything the translator emits as a fixed   *)
(* operator/keyword goes here. Identifiers from the user's spec or     *)
(* declared functions are NOT in this set, so they show up as free.    *)
(* ------------------------------------------------------------------ *)

module StringSet = Set.Make (String)

let smt_keywords =
  StringSet.of_list
    [
      (* logical *)
      "and";
      "or";
      "not";
      "=>";
      "xor";
      "ite";
      "true";
      "false";
      "=";
      (* arithmetic *)
      "+";
      "-";
      "*";
      "/";
      "div";
      "mod";
      "abs";
      (* comparisons *)
      "<";
      ">";
      "<=";
      ">=";
      (* arrays *)
      "select";
      "store";
      "as";
      "const";
      (* binders *)
      "forall";
      "exists";
      "let";
      "match";
      (* commands (don't appear inside terms but harmless to include) *)
      "assert";
      "check-sat";
      "declare-const";
      "declare-fun";
      "declare-sort";
      "define-fun";
      "set-option";
      "set-info";
      "set-logic";
      "get-value";
      "get-unsat-core";
      "get-model";
      "push";
      "pop";
      "echo";
      "exit";
      (* annotations *)
      "!";
      ":named";
      (* sorts that may appear as atoms *)
      "Bool";
      "Int";
      "Real";
      "String";
      "Array";
      (* datatypes *)
      "declare-datatype";
      "declare-datatypes";
    ]

let is_numeric_literal s =
  let len = String.length s in
  if len = 0 then false
  else
    let i = if s.[0] = '-' && len > 1 then 1 else 0 in
    let rec loop saw_dot j =
      if j >= len then j > i
      else
        match s.[j] with
        | '0' .. '9' -> loop saw_dot (j + 1)
        | '.' when not saw_dot -> loop true (j + 1)
        | _ -> false
    in
    loop false i

let is_string_literal s =
  String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'

let is_keyword_like s = String.length s >= 1 && s.[0] = ':'
(* SMT keyword form like :named *)

(* ------------------------------------------------------------------ *)
(* Sexp walking — extract free atoms while respecting binders.          *)
(* ------------------------------------------------------------------ *)

(** Extract binder names from an SMT-LIB binding list of the shape
    [((x T) (y U) ...)]. Returns names in declaration order (with duplicates if
    present, so the caller can detect them). *)
let binder_names (bindings : Sexp.t) : string list =
  match bindings with
  | Sexp.List binders ->
      List.filter_map
        (fun b ->
          match[@warning "-4"] b with
          | Sexp.List (Sexp.Atom name :: _) -> Some name
          | _ -> None)
        binders
  | Sexp.Atom _ -> []

(** [free_atoms sexp] returns the set of atoms in [sexp] that are not bound by
    enclosing [forall] / [exists] / [let] forms, and that look like user
    identifiers (not keywords, not numeric literals, not string literals, not
    :keywords). *)
let free_atoms (sexp : Sexp.t) : StringSet.t =
  let rec go bound acc = function
    | Sexp.Atom a ->
        if
          StringSet.mem a smt_keywords
          || is_numeric_literal a || is_string_literal a || is_keyword_like a
          || StringSet.mem a bound
        then acc
        else StringSet.add a acc
    | Sexp.List items -> (
        match[@warning "-4"] items with
        | [ Sexp.Atom ("forall" | "exists"); bindings; body ] ->
            let names = binder_names bindings in
            let bound' =
              List.fold_left (fun s n -> StringSet.add n s) bound names
            in
            (* Descend into the binding sorts too — those are atoms but
               typically refer to declared sorts (like a Domain name). *)
            let acc = go bound acc bindings in
            go bound' acc body
        | [ Sexp.Atom "let"; bindings; body ] ->
            (* (let ((x e1) (y e2)) body) — bindings are scoped only in body *)
            let names = binder_names bindings in
            let bound' =
              List.fold_left (fun s n -> StringSet.add n s) bound names
            in
            (* Visit each let-bound expression with the *outer* scope. *)
            let acc =
              match[@warning "-4"] bindings with
              | Sexp.List binders ->
                  List.fold_left
                    (fun acc b ->
                      match[@warning "-4"] b with
                      | Sexp.List [ Sexp.Atom _; expr ] -> go bound acc expr
                      | _ -> acc)
                    acc binders
              | _ -> acc
            in
            go bound' acc body
        | _ -> List.fold_left (go bound) acc items)
  in
  go StringSet.empty StringSet.empty sexp

(** Iterate over every [(forall ...)] / [(exists ...)] form, calling [visit]
    with [(quant_kind, bindings_sexp, body_sexp)]. Recurses through the entire
    sexp tree including inside the body. *)
let iter_quantifiers (sexp : Sexp.t)
    (visit : string -> Sexp.t -> Sexp.t -> unit) =
  let rec go = function
    | Sexp.Atom _ -> ()
    | Sexp.List items ->
        (match[@warning "-4"] items with
        | [ Sexp.Atom (("forall" | "exists") as q); bindings; body ] ->
            visit q bindings body
        | _ -> ());
        List.iter go items
  in
  go sexp

(* ------------------------------------------------------------------ *)
(* Individual checks.                                                   *)
(* ------------------------------------------------------------------ *)

(** Quantifier binders must be unique within their introducing form. *)
let check_no_duplicate_binders (sexps : Sexp.t list) : failure list =
  let failures = ref [] in
  List.iter
    (fun sexp ->
      iter_quantifiers sexp (fun q bindings _body ->
          let names = binder_names bindings in
          let seen = Hashtbl.create 8 in
          List.iter
            (fun name ->
              if Hashtbl.mem seen name then
                failures :=
                  {
                    kind = Duplicate_binder;
                    message =
                      Printf.sprintf
                        "%s introduces duplicate binder %S in (%s (%s) ...)" q
                        name q (Sexp.to_string bindings);
                  }
                  :: !failures
              else Hashtbl.add seen name ())
            names))
    sexps;
  List.rev !failures

(** Every quantifier binder must appear free somewhere in its body. Body here
    includes any guard antecedent emitted as [(=> (and guards...) inner)] —
    guards live structurally inside the body, so [free_atoms body] picks them up
    automatically. *)
let check_no_vacuous_binders (sexps : Sexp.t list) : failure list =
  let failures = ref [] in
  List.iter
    (fun sexp ->
      iter_quantifiers sexp (fun q bindings body ->
          let names = binder_names bindings in
          let body_free = free_atoms body in
          List.iter
            (fun name ->
              if not (StringSet.mem name body_free) then
                failures :=
                  {
                    kind = Vacuous_binder;
                    message =
                      Printf.sprintf
                        "%s binds %S but it does not appear free in body" q name;
                  }
                  :: !failures)
            names))
    sexps;
  List.rev !failures

(** Detect uses of the translator's named-fallback constants. The naming
    convention is [_<kind>_fallback_N]. We scan all atoms in all sexps. *)
let check_no_fallback_emissions (sexps : Sexp.t list) : failure list =
  (* Atoms emitted by [Smt_types.fresh_fallback] have the shape
     [_<kind>_fallback_<N>]. Extract the kind by splitting on `_`. *)
  let suffix_marker = "_fallback_" in
  let fallback_kind_of_atom s =
    let len = String.length s in
    if len < 1 + String.length suffix_marker + 1 then None
    else if s.[0] <> '_' then None
    else
      try
        let i = String.index_from s 1 '_' in
        let kind = String.sub s 1 (i - 1) in
        let rest = String.sub s i (len - i) in
        if
          String.length rest > String.length suffix_marker
          && String.sub rest 0 (String.length suffix_marker) = suffix_marker
        then Some kind
        else None
      with Not_found -> None
  in
  let seen : (string * string, unit) Hashtbl.t = Hashtbl.create 4 in
  let rec walk = function
    | Sexp.Atom a -> (
        match fallback_kind_of_atom a with
        | Some kind ->
            if not (Hashtbl.mem seen (kind, a)) then
              Hashtbl.add seen (kind, a) ()
        | None -> ())
    | Sexp.List items -> List.iter walk items
  in
  List.iter walk sexps;
  Hashtbl.fold
    (fun (kind, atom) () acc ->
      {
        kind = Fallback_emission;
        message =
          Printf.sprintf "kind=%s constant=%s (translator approximation)" kind
            atom;
      }
      :: acc)
    seen []

(* ------------------------------------------------------------------ *)
(* Public entry point.                                                  *)
(* ------------------------------------------------------------------ *)

let check_query (smt2 : string) : failure list =
  match parse_smt2 smt2 with
  | Error msg ->
      [ { kind = Parse_error; message = Printf.sprintf "parsexp: %s" msg } ]
  | Ok sexps ->
      check_no_duplicate_binders sexps
      @ check_no_vacuous_binders sexps
      @ check_no_fallback_emissions sexps

let check_queries (smt2s : string list) : failure list =
  List.concat_map check_query smt2s
