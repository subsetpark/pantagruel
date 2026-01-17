(** Type environment for Pantagruel *)

open Types

(** What kind of binding is this? *)
type entry_kind =
  | KDomain                 (** Domain definition *)
  | KAlias of ty            (** Type alias, already expanded *)
  | KProc of ty             (** Procedure with its type *)
  | KVar of ty              (** Variable (param, quantifier-bound) *)
[@@deriving show]

(** An entry in the environment *)
type entry = {
  kind: entry_kind;
  loc: Ast.loc;
  module_origin: string option;  (** None = local, Some m = imported from m *)
}
[@@deriving show]

module StringMap = Map.Make(String)

(** The type environment *)
type t = {
  (** Type namespace: domains and aliases *)
  types: entry StringMap.t;

  (** Term namespace: procedures and variables *)
  terms: entry StringMap.t;

  (** Current module name *)
  current_module: string;

  (** Void procedure in current chapter (for prime checking) *)
  void_proc: string option;

  (** Variables bound in current scope (for prime rejection) *)
  local_vars: string list;
}

let empty module_name = {
  types = StringMap.empty;
  terms = StringMap.empty;
  current_module = module_name;
  void_proc = None;
  local_vars = [];
}

(** Add a domain to the type namespace *)
let add_domain name loc env =
  let entry = { kind = KDomain; loc; module_origin = None } in
  { env with types = StringMap.add name entry env.types }

(** Add a type alias to the type namespace *)
let add_alias name ty loc env =
  let entry = { kind = KAlias ty; loc; module_origin = None } in
  { env with types = StringMap.add name entry env.types }

(** Add a procedure to the term namespace *)
let add_proc name ty loc env =
  let entry = { kind = KProc ty; loc; module_origin = None } in
  { env with terms = StringMap.add name entry env.terms }

(** Add a variable to the term namespace (also tracks as local var) *)
let add_var name ty env =
  let entry = { kind = KVar ty; loc = Ast.dummy_loc; module_origin = None } in
  { env with
    terms = StringMap.add name entry env.terms;
    local_vars = name :: env.local_vars }

(** Set the Void procedure context for a chapter *)
let with_void_proc name env =
  { env with void_proc = Some name }

(** Clear the Void procedure context *)
let clear_void_proc env =
  { env with void_proc = None }

(** Check if a name is a locally-bound variable *)
let is_local_var name env =
  List.mem name env.local_vars

(** Check if we're in a Void procedure context *)
let in_void_context env =
  Option.is_some env.void_proc

(** Lookup a type by name *)
let lookup_type name env =
  StringMap.find_opt name env.types

(** Lookup a term by name *)
let lookup_term name env =
  StringMap.find_opt name env.terms

(** Create a child environment with additional variable bindings *)
let with_vars vars env =
  List.fold_left (fun env (name, ty) -> add_var name ty env) env vars

(** Reset local vars (for entering new scope) *)
let reset_local_vars env =
  { env with local_vars = [] }

(** Get all exported names (for module system) *)
let exports env =
  let type_names = StringMap.bindings env.types |> List.map fst in
  let term_names =
    StringMap.bindings env.terms
    |> List.filter (fun (_, e) -> match e.kind with KVar _ -> false | _ -> true)
    |> List.map fst
  in
  (type_names, term_names)

(** Merge another environment's exports into this one *)
let merge_imports env other origin_module =
  let add_with_origin map (name, entry) =
    let entry' = { entry with module_origin = Some origin_module } in
    StringMap.add name entry' map
  in
  let types' = List.fold_left add_with_origin env.types (StringMap.bindings other.types) in
  let terms' =
    other.terms
    |> StringMap.bindings
    |> List.filter (fun (_, e) -> match e.kind with KVar _ -> false | _ -> true)
    |> List.fold_left add_with_origin env.terms
  in
  { env with types = types'; terms = terms' }
