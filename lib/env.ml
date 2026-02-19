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
  decl_chapter: int;  (** Chapter where declared; -1 for imports/builtins *)
}
[@@deriving show]

module StringMap = Map.Make(String)

(** The type environment *)
type t = {
  (** Type namespace: domains and aliases *)
  types: entry StringMap.t;

  (** Term namespace: procedures and variables *)
  terms: entry StringMap.t;

  (** Import index: name → [(module, entry)] for types *)
  imported_types: (string * entry) list StringMap.t;

  (** Import index: name → [(module, entry)] for terms *)
  imported_terms: (string * entry) list StringMap.t;

  (** Current module name *)
  current_module: string;

  (** Context declarations: context name → member function names *)
  contexts: string list StringMap.t;

  (** Void procedure in current chapter (for prime checking) *)
  void_proc: string option;

  (** Active context for current void proc (for primed-in-context enforcement) *)
  proc_context: string option;

  (** Variables bound in current scope (for prime rejection) *)
  local_vars: string list;
}

let empty module_name = {
  types = StringMap.empty;
  terms = StringMap.empty;
  imported_types = StringMap.empty;
  imported_terms = StringMap.empty;
  current_module = module_name;
  contexts = StringMap.empty;
  void_proc = None;
  proc_context = None;
  local_vars = [];
}

(** Add a domain to the type namespace *)
let add_domain name loc ~chapter env =
  let entry = { kind = KDomain; loc; module_origin = None; decl_chapter = chapter } in
  { env with types = StringMap.add name entry env.types }

(** Add a type alias to the type namespace *)
let add_alias name ty loc ~chapter env =
  let entry = { kind = KAlias ty; loc; module_origin = None; decl_chapter = chapter } in
  { env with types = StringMap.add name entry env.types }

(** Add a procedure to the term namespace *)
let add_proc name ty loc ~chapter env =
  let entry = { kind = KProc ty; loc; module_origin = None; decl_chapter = chapter } in
  { env with terms = StringMap.add name entry env.terms }

(** Add a variable to the term namespace (also tracks as local var) *)
let add_var name ty env =
  let entry = { kind = KVar ty; loc = Ast.dummy_loc; module_origin = None; decl_chapter = -1 } in
  { env with
    terms = StringMap.add name entry env.terms;
    local_vars = name :: env.local_vars }

(** Set the Void procedure context for a chapter *)
let with_void_proc name env =
  { env with void_proc = Some name }

(** Clear the Void procedure context *)
let clear_void_proc env =
  { env with void_proc = None }

(** Add a context declaration (initially empty members) *)
let add_context name members env =
  { env with contexts = StringMap.add name members env.contexts }

(** Add a procedure to a context's member list *)
let add_proc_to_context ctx_name proc_name env =
  match StringMap.find_opt ctx_name env.contexts with
  | Some members ->
      { env with contexts = StringMap.add ctx_name (members @ [proc_name]) env.contexts }
  | None -> env

(** Lookup a context by name *)
let lookup_context name env =
  StringMap.find_opt name env.contexts

(** Set the active proc context *)
let with_proc_context ctx env =
  { env with proc_context = ctx }

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

(** Get all exported names (for module system) *)
let exports env =
  let type_names = StringMap.bindings env.types |> List.map fst in
  let term_names =
    StringMap.bindings env.terms
    |> List.filter (fun (_, e) -> match e.kind with KVar _ -> false | _ -> true)
    |> List.map fst
  in
  (type_names, term_names)

(** Add another environment's exports to the import index, then rebuild flat maps
    with only unambiguous imports *)
let add_import env other origin_module =
  let add_to_index index other_map =
    StringMap.fold (fun name entry index ->
      match entry.kind with
      | KVar _ -> index
      | _ ->
        let existing = match StringMap.find_opt name index with
          | Some lst -> lst | None -> [] in
        (* Avoid adding duplicates from the same module *)
        if List.exists (fun (m, _) -> m = origin_module) existing then index
        else StringMap.add name ((origin_module, entry) :: existing) index
    ) other_map index
  in
  let imported_types = add_to_index env.imported_types other.types in
  let imported_terms = add_to_index env.imported_terms other.terms in
  let flat_of_index index =
    StringMap.fold (fun name entries acc ->
      match entries with
      | [(origin, entry)] ->
          StringMap.add name { entry with module_origin = Some origin; decl_chapter = -1 } acc
      | _ -> acc
    ) index StringMap.empty
  in
  (* Merge contexts from imported module *)
  let merged_contexts =
    StringMap.fold (fun name members acc ->
      StringMap.add name members acc
    ) other.contexts env.contexts
  in
  { env with imported_types; imported_terms;
    types = flat_of_index imported_types;
    terms = flat_of_index imported_terms;
    contexts = merged_contexts }

(** Lookup a type by module and name in the import index *)
let lookup_qualified_type mod_name name env =
  match StringMap.find_opt name env.imported_types with
  | Some entries ->
      (match List.find_opt (fun (m, _) -> m = mod_name) entries with
       | Some (_, entry) -> Some entry
       | None -> None)
  | None -> None

(** Lookup a term by module and name in the import index *)
let lookup_qualified_term mod_name name env =
  match StringMap.find_opt name env.imported_terms with
  | Some entries ->
      (match List.find_opt (fun (m, _) -> m = mod_name) entries with
       | Some (_, entry) -> Some entry
       | None -> None)
  | None -> None

(** Check if a type name is ambiguous across imports.
    Returns Some [module_names] if ambiguous, None otherwise *)
let ambiguous_type_modules name env =
  match StringMap.find_opt name env.imported_types with
  | Some entries when List.length entries > 1 ->
      Some (List.map fst entries)
  | _ -> None

(** Check if a term name is ambiguous across imports.
    Returns Some [module_names] if ambiguous, None otherwise *)
let ambiguous_term_modules name env =
  match StringMap.find_opt name env.imported_terms with
  | Some entries when List.length entries > 1 ->
      Some (List.map fst entries)
  | _ -> None

(** Filter environment for visibility in a chapter head.
    Declaration in chapter N is visible in heads of chapters M >= N.
    Imports (decl_chapter = -1) are always visible. *)
let visible_in_head chapter_idx env =
  let filter_map m =
    StringMap.filter (fun _ entry -> entry.decl_chapter <= chapter_idx) m
  in
  { env with types = filter_map env.types; terms = filter_map env.terms }

(** Filter environment for visibility in a chapter body.
    Declaration in chapter N is visible in bodies of chapters M >= N-1.
    Imports (decl_chapter = -1) are always visible. *)
let visible_in_body chapter_idx env =
  let filter_map m =
    StringMap.filter (fun _ entry -> entry.decl_chapter <= chapter_idx + 1) m
  in
  { env with types = filter_map env.types; terms = filter_map env.terms }
