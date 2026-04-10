(** Pass 1: Collect declarations from all chapters *)

type collect_error =
  | DuplicateDomain of string * Ast.loc * Ast.loc
  | DuplicateRule of string * Ast.loc * Ast.loc
  | UndefinedType of string * Ast.loc
  | RecursiveAlias of string * Ast.loc
  | MultipleActions of string * string * Ast.loc
  | ActionNotLast of string * Ast.loc
  | BuiltinRedefined of string * Ast.loc
  | DuplicateContext of string * Ast.loc
  | UndefinedContext of string * Ast.loc
  | ClosureTargetInvalid of string * string * Ast.loc
[@@deriving show]

val resolve_type :
  Env.t -> Ast.type_expr -> Ast.loc -> (Types.ty, collect_error) result
(** Resolve a syntactic type expression to an internal type *)

val collect_all :
  base_env:Env.t -> Ast.document -> (Env.t, collect_error) result
(** Collect all declarations from a document *)
