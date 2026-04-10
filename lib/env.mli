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

type t = {
  types : entry StringMap.t;
  terms : entry StringMap.t;
  imported_types : (string * entry) list StringMap.t;
  imported_terms : (string * entry) list StringMap.t;
  current_module : string;
  contexts : string list StringMap.t;
  rule_guards : (Ast.param list * Ast.guard list) StringMap.t;
  action : string option;
  action_contexts : string list;
  local_vars : string list;
}

val empty : string -> t
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
val with_vars : (string * Types.ty) list -> t -> t
val exports : t -> string list * string list
val add_import : t -> t -> string -> t
val lookup_qualified_type : string -> string -> t -> entry option
val lookup_qualified_term : string -> string -> t -> entry option
val ambiguous_type_modules : string -> t -> string list option
val ambiguous_term_modules : string -> t -> string list option
val visible_in_head : int -> t -> t
val visible_in_body : int -> t -> t
