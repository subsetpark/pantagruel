(* @archlint.module state
   @archlint.domain pantagruel.smt-types *)

(** Stateful SMT auxiliary symbol allocation. *)

(** Fresh uninterpreted constants for non-exhaustive cond else-branches. When a
    cond's last arm guard is not [true], we emit a fresh constant of the
    appropriate sort so the gap is genuinely unconstrained rather than silently
    assigned the last arm's consequence. *)
let cond_aux_counter = ref 0

let cond_aux_decls : string list ref = ref []

let reset_cond_aux () =
  cond_aux_counter := 0;
  cond_aux_decls := []

let fresh_cond_default sort =
  let n = !cond_aux_counter in
  incr cond_aux_counter;
  let name = Printf.sprintf "_cond_undef_%d" n in
  cond_aux_decls :=
    Printf.sprintf "(declare-const %s %s)" name sort :: !cond_aux_decls;
  name

let drain_cond_aux_decls () =
  let decls = List.rev !cond_aux_decls in
  cond_aux_decls := [];
  if decls = [] then ""
  else "\n; --- Cond default constants ---\n" ^ String.concat "\n" decls ^ "\n"

(** Insert accumulated cond-default declarations into a finished SMT-LIB2
    string. Must be called after all [translate_*] calls for the query. *)
let insert_cond_aux_decls smt2 =
  Smt_types.splice_before_first_assert smt2 (drain_cond_aux_decls ())

(** Fresh uninterpreted constants for translation fallbacks. When a translation
    site cannot produce a faithful SMT term (e.g. cardinality of a list over an
    unbounded element type, or a standalone first-class override that has no
    direct SMT-LIB encoding), it emits a fresh constant of the appropriate sort
    and uses its name in place of the silent literal it would otherwise emit.
    The named constants make the approximation visible to downstream tooling (in
    particular, the structural checks in [test/smt_check.ml]). *)
let fallback_counter = ref 0

let fallback_decls : string list ref = ref []

let reset_fallbacks () =
  fallback_counter := 0;
  fallback_decls := []

let fresh_fallback ~kind ~sort =
  let n = !fallback_counter in
  incr fallback_counter;
  let name = Printf.sprintf "_%s_fallback_%d" kind n in
  fallback_decls :=
    Printf.sprintf "(declare-const %s %s)" name sort :: !fallback_decls;
  name

let fallback_cache : (string * string * string, string) Hashtbl.t =
  Hashtbl.create 16

let reset_fallback_cache () = Hashtbl.clear fallback_cache

let intern_fallback_symbol ~kind ~sort ~key =
  match Hashtbl.find_opt fallback_cache (kind, sort, key) with
  | Some name -> name
  | None ->
      let name = fresh_fallback ~kind ~sort in
      Hashtbl.add fallback_cache (kind, sort, key) name;
      name

(** Query-local cache of list-search placeholder symbols. Identical
    [(func_s, arg_s)] keys reuse the same fresh constant so that [xs x = xs x]
    translates to a referentially stable equality. *)
let reset_list_search_cache () = reset_fallback_cache ()

let intern_list_search_symbol ~func_s ~arg_s =
  intern_fallback_symbol ~kind:"list_search" ~sort:"Int"
    ~key:(func_s ^ "\x00" ^ arg_s)

let intern_list_index_symbol ~func_s ~arg_s ~sort =
  intern_fallback_symbol ~kind:"list_index" ~sort ~key:(func_s ^ "\x00" ^ arg_s)

let intern_card_symbol ~expr_s =
  intern_fallback_symbol ~kind:"card" ~sort:"Int" ~key:expr_s

(** Queue an additional [(assert ...)] alongside the most recently declared
    fallback constant — useful for soft constraints like non-negativity. *)
let add_fallback_assert assertion_body =
  fallback_decls :=
    Printf.sprintf "(assert %s)" assertion_body :: !fallback_decls

let drain_fallback_decls () =
  let decls = List.rev !fallback_decls in
  fallback_decls := [];
  if decls = [] then ""
  else "\n; --- Fallback constants ---\n" ^ String.concat "\n" decls ^ "\n"

(** Insert accumulated fallback declarations into a finished SMT-LIB2 string. *)
let insert_fallback_decls smt2 =
  Smt_types.splice_before_first_assert smt2 (drain_fallback_decls ())

(** Wrap a query generator: reset per-query auxiliary state (cond defaults and
    fallback constants), run the generator, and insert any accumulated
    declarations into the output. *)
let with_cond_aux (f : unit -> Smt_types.query) =
  reset_cond_aux ();
  reset_fallbacks ();
  reset_list_search_cache ();
  let q = f () in
  { q with smt2 = q.smt2 |> insert_cond_aux_decls |> insert_fallback_decls }
