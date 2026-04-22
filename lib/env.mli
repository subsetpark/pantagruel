(** Type environment for Pantagruel *)

(** What kind of binding is this? *)
type entry_kind =
  | KDomain  (** Domain definition *)
  | KAlias of Types.ty  (** Type alias, already expanded *)
  | KRule of Types.ty  (** Rule with its type *)
  | KVar of Types.ty  (** Variable (param, quantifier-bound) *)
  | KClosure of Types.ty * string  (** Closure rule: type * target rule name *)
[@@deriving show]

type entry = {
  kind : entry_kind;
  loc : Ast.loc;
  module_origin : string option;
  decl_chapter : int;
}
[@@deriving show]

module StringMap : Map.S with type key = string

type t

val empty : string -> t
val add_type_entry : string -> entry -> t -> t
val add_term_entry : string -> entry -> t -> t
val add_domain : string -> Ast.loc -> chapter:int -> t -> t
val add_alias : string -> Types.ty -> Ast.loc -> chapter:int -> t -> t
val add_rule : string -> Types.ty -> Ast.loc -> chapter:int -> t -> t

val add_closure :
  string -> Types.ty -> string -> Ast.loc -> chapter:int -> t -> t

val add_var : string -> Types.ty -> t -> t
val with_action : string -> t -> t
val clear_action : t -> t
val add_context : string -> string list -> t -> t
val add_rule_to_context : string -> string -> t -> t
val lookup_context : string -> t -> string list option
val lookup_rule_guards : string -> t -> (Ast.param list * Ast.guard list) option
val add_rule_guards : string -> Ast.param list -> Ast.guard list -> t -> t
val with_action_contexts : string list -> t -> t
val is_local_var : string -> t -> bool
val in_action_context : t -> bool
val lookup_type : string -> t -> entry option

val lookup_term : string -> t -> entry option
(** Lookup a rule or closure (no variable fallback). *)

val lookup_var : string -> t -> entry option
(** Lookup a variable only. *)

val lookup_bare : string -> t -> entry option
(** Bare-atom (value-position) lookup: variables first, then rules. The one site
    where rule and variable namespaces may legitimately collide. *)

val fold_terms : (string -> entry -> 'a -> 'a) -> t -> 'a -> 'a
val iter_terms : (string -> entry -> unit) -> t -> unit

val fold_all_terms : (string -> entry -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over both rules/closures ([terms]) and variables ([vars]). *)

val fold_types : (string -> entry -> 'a -> 'a) -> t -> 'a -> 'a
val iter_types : (string -> entry -> unit) -> t -> unit
val bindings_terms : t -> (string * entry) list
val bindings_types : t -> (string * entry) list

val shadow_reporter :
  (string -> Types.ty -> Types.ty -> Ast.loc -> Ast.loc -> unit) ref
(** Callback invoked by [add_var] when a variable is added with the same name as
    an existing nullary declaration (rule or closure). Both kinds auto-apply in
    bare-atom position, so a same-named variable eclipses them identically.
    Args: name, declaration's return type, var's type, declaration's loc, var's
    add loc. The check layer installs a reporter that pushes a warning into its
    type-warning accumulator; default is a no-op for tests that use env in
    isolation. *)

val action_contexts : t -> string list
val current_module : t -> string
val with_module_init : string -> t -> t
val with_vars : (string * Types.ty) list -> t -> t
val exports : t -> string list * string list
val add_import : t -> t -> string -> t
val lookup_qualified_type : string -> string -> t -> entry option
val lookup_qualified_term : string -> string -> t -> entry option
val ambiguous_type_modules : string -> t -> string list option
val ambiguous_term_modules : string -> t -> string list option
val visible_in_head : int -> t -> t
val visible_in_body : int -> t -> t
