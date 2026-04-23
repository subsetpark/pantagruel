(** Type environment for Pantagruel *)

open Types

(** What kind of binding is this? *)
type entry_kind =
  | KDomain  (** Domain definition *)
  | KAlias of ty  (** Type alias, already expanded *)
  | KRule of ty  (** Rule with its type *)
  | KVar of ty  (** Variable (param, quantifier-bound) *)
  | KClosure of ty * string  (** Closure rule: type * target rule name *)
[@@deriving show]

type entry = {
  kind : entry_kind;
  loc : Ast.loc;
  module_origin : string option;  (** None = local, Some m = imported from m *)
  decl_chapter : int;  (** Chapter where declared; -1 for imports/builtins *)
}
[@@deriving show]
(** An entry in the environment *)

module StringMap = Map.Make (String)

(** Arity-keyed map for the term namespace. Keyed by [(name, arity)] so rules
    can be overloaded by arity under positional coherence. Single-arity rules
    continue to have one entry per name. *)
module TermKey = struct
  type t = string * int

  let compare = Stdlib.compare
end

module TermMap = Map.Make (TermKey)

type t = {
  types : entry StringMap.t;  (** Type namespace: domains and aliases *)
  terms : entry TermMap.t;
      (** Term namespace: rules (KRule) and closures (KClosure) only. Variables
          live in [vars]. Keyed by [(name, arity)] to allow arity-overloading
          under positional coherence. Separating the two namespaces lets
          application heads / primed / override positions resolve against
          [terms] without being shadowed by same-named parameters — the six-of-
          seven syntactic positions in which a lower identifier can appear are
          rule-only; only bare-atom references are ambiguous, and those resolve
          through [lookup_bare] with var-first-then-nullary-rule fallback. *)
  vars : entry StringMap.t;
      (** Variable namespace: KVar only. Parameters, quantifier binders, and
          shorthand-bound values. Separated from [terms] so a param named the
          same as a same-named rule no longer overwrites the rule in lookup. *)
  imported_types : (string * entry) list StringMap.t;
      (** Import index: name -> [(module, entry)] for types *)
  imported_terms : (string * entry) list TermMap.t;
      (** Import index: (name, arity) -> [(module, entry)] for terms (rules).
          Imports never include variables ([add_import] filters [KVar]). Arity-
          keyed so module A exporting f/1 and f/2 imports cleanly; two modules
          exporting the same (name, arity) remain an ambiguous collision. *)
  current_module : string;  (** Current module name *)
  contexts : string list StringMap.t;
      (** Context declarations: context name -> member function names *)
  rule_guards : (Ast.param list * Ast.guard list) TermMap.t;
      (** Declaration guards keyed by (rule name, arity): formal params +
          guards. Keying by arity mirrors the term namespace so per-overload
          guards stay distinct. *)
  action : string option;  (** Action in current chapter (for prime checking) *)
  action_contexts : string list;
      (** Active contexts for current action (for primed-in-context enforcement)
      *)
  local_vars : string list;
      (** Variables bound in current scope (for prime rejection) *)
}
(** The type environment *)

let empty module_name =
  {
    types = StringMap.empty;
    terms = TermMap.empty;
    vars = StringMap.empty;
    imported_types = StringMap.empty;
    imported_terms = TermMap.empty;
    current_module = module_name;
    contexts = StringMap.empty;
    rule_guards = TermMap.empty;
    action = None;
    action_contexts = [];
    local_vars = [];
  }

(** Callback for reporting that an [add_var] call shadowed a nullary rule of the
    same name. Installed by [check.ml] so the existing warning pipeline picks up
    the event without creating a circular dependency between [env.ml] and
    [check.ml]. Default is no-op so tests that use the env directly don't
    require setting it. *)
let shadow_reporter : (string -> ty -> ty -> Ast.loc -> Ast.loc -> unit) ref =
  ref (fun _ _ _ _ _ -> ())

(** Extract the arity implied by a KRule / KClosure entry's type. For any
    TyFunc, arity is the length of the param list. Non-TyFunc kinds shouldn't
    land in [terms] but we default to 0 defensively. *)
let entry_arity (e : entry) : int =
  match[@warning "-4"] e.kind with
  | KRule (TyFunc (params, _)) | KClosure (TyFunc (params, _), _) ->
      List.length params
  | _ -> 0

(** Add a raw entry to the type namespace *)
let add_type_entry name entry env =
  { env with types = StringMap.add name entry env.types }

(** Add a raw entry to the term namespace. Arity is inferred from the entry's
    type (param list of a KRule / KClosure TyFunc). *)
let add_term_entry name entry env =
  let arity = entry_arity entry in
  { env with terms = TermMap.add (name, arity) entry env.terms }

(** Add a domain to the type namespace *)
let add_domain name loc ~chapter env =
  let entry =
    { kind = KDomain; loc; module_origin = None; decl_chapter = chapter }
  in
  { env with types = StringMap.add name entry env.types }

(** Add a type alias to the type namespace *)
let add_alias name ty loc ~chapter env =
  let entry =
    { kind = KAlias ty; loc; module_origin = None; decl_chapter = chapter }
  in
  { env with types = StringMap.add name entry env.types }

(** Compute the arity implied by a rule/closure type. *)
let arity_of_ty (ty : ty) : int =
  match[@warning "-4"] ty with
  | TyFunc (params, _) -> List.length params
  | _ -> 0

(** Add a rule to the term namespace, keyed by [(name, arity)]. *)
let add_rule name ty loc ~chapter env =
  let entry =
    { kind = KRule ty; loc; module_origin = None; decl_chapter = chapter }
  in
  { env with terms = TermMap.add (name, arity_of_ty ty) entry env.terms }

(** Add a closure rule to the term namespace, keyed by [(name, arity)]. *)
let add_closure name ty target loc ~chapter env =
  let entry =
    {
      kind = KClosure (ty, target);
      loc;
      module_origin = None;
      decl_chapter = chapter;
    }
  in
  { env with terms = TermMap.add (name, arity_of_ty ty) entry env.terms }

(** Collect every overload of [name] in the term namespace, sorted by ascending
    arity. Returns [] when no declaration of [name] exists. *)
let overloads_of name env =
  TermMap.fold
    (fun (n, arity) entry acc ->
      if n = name then (arity, entry) :: acc else acc)
    env.terms []
  |> List.sort (fun (a, _) (b, _) -> compare a b)

(** Whether [name] has two or more arity overloads. Used by SMT emission to
    decide whether to mangle the output symbol with an arity suffix. *)
let name_is_overloaded name env = List.length (overloads_of name env) >= 2

(** Add a variable to the var namespace (also tracks as local var). If [name]
    matches a nullary declaration in [terms] (rule or closure), fire the shadow
    reporter so the check layer can emit a warning — nullary rules/closures
    auto-apply in bare-atom position, so a same-named variable genuinely
    eclipses a binding the user could otherwise reach by bare reference. Non-
    nullary collisions are not shadowing (syntactic position disambiguates) and
    don't warn. *)
let add_var name ty env =
  (match[@warning "-4"] TermMap.find_opt (name, 0) env.terms with
  | Some { kind = KRule (TyFunc ([], Some ret)); loc = rule_loc; _ }
  | Some { kind = KClosure (TyFunc ([], Some ret), _); loc = rule_loc; _ } ->
      !shadow_reporter name ret ty rule_loc Ast.dummy_loc
  | _ -> ());
  let entry =
    {
      kind = KVar ty;
      loc = Ast.dummy_loc;
      module_origin = None;
      decl_chapter = -1;
    }
  in
  {
    env with
    vars = StringMap.add name entry env.vars;
    local_vars = name :: env.local_vars;
  }

(** Set the action for a chapter *)
let with_action name env = { env with action = Some name }

(** Clear the action *)
let clear_action env = { env with action = None }

(** Add a context declaration (initially empty members) *)
let add_context name members env =
  { env with contexts = StringMap.add name members env.contexts }

(** Add a rule to a context's member list *)
let add_rule_to_context ctx_name rule_name env =
  match StringMap.find_opt ctx_name env.contexts with
  | Some members ->
      {
        env with
        contexts = StringMap.add ctx_name (members @ [ rule_name ]) env.contexts;
      }
  | None -> env

(** Lookup a context by name *)
let lookup_context name env = StringMap.find_opt name env.contexts

(** Lookup rule guards by name. Compatibility shim — returns the first
    overload's guards. Arity-aware callers should use [lookup_rule_guards_arity]
    once Patch 3/4 migrates them. *)
let lookup_rule_guards name env =
  TermMap.fold
    (fun (n, _) guards acc ->
      match acc with
      | Some _ -> acc
      | None -> if n = name then Some guards else None)
    env.rule_guards None

(** Lookup rule guards by [(name, arity)]. *)
let lookup_rule_guards_arity name arity env =
  TermMap.find_opt (name, arity) env.rule_guards

(** Store declaration guards for a rule. Arity is derived from the param list.
*)
let add_rule_guards name params guards env =
  if guards = [] then env
  else
    let arity = List.length params in
    {
      env with
      rule_guards = TermMap.add (name, arity) (params, guards) env.rule_guards;
    }

(** Set the active action contexts *)
let with_action_contexts ctxs env = { env with action_contexts = ctxs }

(** Check if a name is a locally-bound variable *)
let is_local_var name env = List.mem name env.local_vars

(** Check if we're in an action context *)
let in_action_context env = Option.is_some env.action

(** Lookup a type by name *)
let lookup_type name env = StringMap.find_opt name env.types

(** Lookup a term (rule or closure) by name. Compatibility shim — returns the
    first overload by ascending arity. Arity-aware callers should use
    [lookup_term_arity] to pick a specific overload. *)
let lookup_term name env =
  match overloads_of name env with [] -> None | (_, e) :: _ -> Some e

(** Lookup a term (rule or closure) by name AND arity. Returns exactly the
    overload whose declared arity matches. *)
let lookup_term_arity name arity env = TermMap.find_opt (name, arity) env.terms

(** Lookup a variable by name. *)
let lookup_var name env = StringMap.find_opt name env.vars

(** Lookup for bare-atom references in value position: try variables first, then
    fall back to term entries. Patch 1 shim: accepts any arity. Patch 3 will
    tighten this to nullary-only to preserve Pantagruel's no-first-class-
    functions discipline in the overload world. *)
let lookup_bare name env =
  match StringMap.find_opt name env.vars with
  | Some _ as e -> e
  | None -> lookup_term name env

(** Fold over the terms namespace. Shim over the arity-keyed storage that keeps
    the old [(name -> entry -> acc -> acc)] callback signature. Each overload is
    visited as a separate (name, entry) pair; callers that don't know about
    overloads will see the same name repeated. *)
let fold_terms f env init =
  TermMap.fold (fun (n, _) entry acc -> f n entry acc) env.terms init

(** Iterate over the terms namespace (rules and closures). *)
let iter_terms f env = TermMap.iter (fun (n, _) entry -> f n entry) env.terms

(** Fold over every binding in both [terms] and [vars]. Used where callers need
    the union (e.g., sort collection for SMT preamble). Fold order: [terms]
    first, then [vars]. *)
let fold_all_terms f env init =
  let acc =
    TermMap.fold (fun (n, _) entry acc -> f n entry acc) env.terms init
  in
  StringMap.fold f env.vars acc

(** Fold over the types namespace *)
let fold_types f env init = StringMap.fold f env.types init

(** Iterate over the types namespace *)
let iter_types f env = StringMap.iter f env.types

(** Get bindings of the terms namespace. Each overload is its own (name, entry)
    pair; single-arity rules continue to have one entry per name. *)
let bindings_terms env =
  TermMap.fold (fun (n, _) entry acc -> (n, entry) :: acc) env.terms []
  |> List.rev

(** Get bindings of the types namespace *)
let bindings_types env = StringMap.bindings env.types

(** Get the active action contexts *)
let action_contexts env = env.action_contexts

(** Get the current module name *)
let current_module env = env.current_module

(** Initialize environment for a new module: set module name, clear action,
    local_vars, and any scoped variables (which belong to a prior chapter body
    and should not leak). *)
let with_module_init mod_name env =
  {
    env with
    current_module = mod_name;
    action = None;
    local_vars = [];
    vars = StringMap.empty;
  }

(** Create a child environment with additional variable bindings *)
let with_vars vars env =
  List.fold_left (fun env (name, ty) -> add_var name ty env) env vars

(** Get all exported names (for module system). Variables are never exported —
    only rules, closures, domains, and aliases cross module boundaries — so
    [vars] is ignored here. Overloaded names appear once in the term list. *)
let exports env =
  let type_names = StringMap.bindings env.types |> List.map fst in
  let term_names =
    TermMap.fold (fun (n, _) _ acc -> n :: acc) env.terms []
    |> List.sort_uniq String.compare
  in
  (type_names, term_names)

(** Add another environment's exports to the import index, then rebuild flat
    maps with only unambiguous imports. Term imports are keyed by
    [(name, arity)] so overload families from a single module merge into the
    importer's term namespace cleanly, while same-(name, arity) exports from two
    modules remain ambiguous. *)
let add_import env other origin_module =
  let add_to_type_index index other_map =
    StringMap.fold
      (fun name entry index ->
        match entry.kind with
        | KVar _ -> index
        | KDomain | KAlias _ | KRule _ | KClosure _ ->
            let existing =
              match StringMap.find_opt name index with
              | Some lst -> lst
              | None -> []
            in
            if List.exists (fun (m, _) -> m = origin_module) existing then index
            else StringMap.add name ((origin_module, entry) :: existing) index)
      other_map index
  in
  let add_to_term_index index other_map =
    TermMap.fold
      (fun key entry index ->
        match entry.kind with
        | KVar _ -> index
        | KDomain | KAlias _ | KRule _ | KClosure _ ->
            let existing =
              match TermMap.find_opt key index with
              | Some lst -> lst
              | None -> []
            in
            if List.exists (fun (m, _) -> m = origin_module) existing then index
            else TermMap.add key ((origin_module, entry) :: existing) index)
      other_map index
  in
  let imported_types = add_to_type_index env.imported_types other.types in
  let imported_terms = add_to_term_index env.imported_terms other.terms in
  let flat_of_type_index index =
    StringMap.fold
      (fun name entries acc ->
        match entries with
        | [ (origin, entry) ] ->
            StringMap.add name
              { entry with module_origin = Some origin; decl_chapter = -1 }
              acc
        | _ -> acc)
      index StringMap.empty
  in
  let flat_of_term_index index =
    TermMap.fold
      (fun key entries acc ->
        match entries with
        | [ (origin, entry) ] ->
            TermMap.add key
              { entry with module_origin = Some origin; decl_chapter = -1 }
              acc
        | _ -> acc)
      index TermMap.empty
  in
  let merged_contexts =
    StringMap.fold
      (fun name members acc -> StringMap.add name members acc)
      other.contexts env.contexts
  in
  let merged_rule_guards =
    TermMap.fold
      (fun key guards acc -> TermMap.add key guards acc)
      other.rule_guards env.rule_guards
  in
  {
    env with
    imported_types;
    imported_terms;
    types = flat_of_type_index imported_types;
    terms = flat_of_term_index imported_terms;
    contexts = merged_contexts;
    rule_guards = merged_rule_guards;
  }

(** Lookup a type by module and name in the import index *)
let lookup_qualified_type mod_name name env =
  match StringMap.find_opt name env.imported_types with
  | Some entries -> (
      match List.find_opt (fun (m, _) -> m = mod_name) entries with
      | Some (_, entry) -> Some entry
      | None -> None)
  | None -> None

(** Lookup a term by module and name in the import index. Compatibility shim —
    returns the first overload of [name] exported by [mod_name]. *)
let lookup_qualified_term mod_name name env =
  TermMap.fold
    (fun (n, _) entries acc ->
      match acc with
      | Some _ -> acc
      | None ->
          if n = name then
            match List.find_opt (fun (m, _) -> m = mod_name) entries with
            | Some (_, entry) -> Some entry
            | None -> None
          else None)
    env.imported_terms None

(** Check if a type name is ambiguous across imports. Returns Some
    [module_names] if ambiguous, None otherwise *)
let ambiguous_type_modules name env =
  match StringMap.find_opt name env.imported_types with
  | Some entries when List.length entries > 1 -> Some (List.map fst entries)
  | _ -> None

(** Check if a term name is ambiguous across imports. Returns Some
    [module_names] if ambiguous, None otherwise. With arity-keyed imports, a
    name is ambiguous when *any* arity overload is declared in multiple modules
    — collected across arities and deduplicated. *)
let ambiguous_term_modules name env =
  let all_entries =
    TermMap.fold
      (fun (n, _) entries acc -> if n = name then entries @ acc else acc)
      env.imported_terms []
  in
  let modules = List.map fst all_entries |> List.sort_uniq String.compare in
  if List.length modules > 1 then Some modules else None

(** Filter environment for visibility in a chapter head. Declaration in chapter
    N is visible in heads of chapters M >= N. Imports (decl_chapter = -1) are
    always visible. *)
let visible_in_head chapter_idx env =
  let filter_string_map m =
    StringMap.filter (fun _ entry -> entry.decl_chapter <= chapter_idx) m
  in
  let filter_term_map m =
    TermMap.filter (fun _ entry -> entry.decl_chapter <= chapter_idx) m
  in
  {
    env with
    types = filter_string_map env.types;
    terms = filter_term_map env.terms;
    vars = filter_string_map env.vars;
  }

(** Filter environment for visibility in a chapter body. Declaration in chapter
    N is visible in bodies of chapters M >= N-1. Imports (decl_chapter = -1) are
    always visible. *)
let visible_in_body chapter_idx env =
  let filter_string_map m =
    StringMap.filter (fun _ entry -> entry.decl_chapter <= chapter_idx + 1) m
  in
  let filter_term_map m =
    TermMap.filter (fun _ entry -> entry.decl_chapter <= chapter_idx + 1) m
  in
  {
    env with
    types = filter_string_map env.types;
    terms = filter_term_map env.terms;
    vars = filter_string_map env.vars;
  }
