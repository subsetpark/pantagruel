(** Pass 2: Type checking *)

type type_error =
  | UnboundVariable of string * Ast.loc
  | UnboundType of string * Ast.loc
  | TypeMismatch of Types.ty * Types.ty * Ast.loc
  | ArityMismatch of int * int * Ast.loc
  | NotAFunction of Types.ty * Ast.loc
  | NotAList of Types.ty * Ast.loc
  | NotAProduct of Types.ty * Ast.loc
  | NotNumeric of Types.ty * Ast.loc
  | ExpectedBool of Types.ty * Ast.loc
  | PrimedNonRule of string * Ast.loc
  | PrimeOutsideActionContext of string * Ast.loc
  | OverrideRequiresArity1 of string * int * Ast.loc
  | ProjectionOutOfBounds of int * int * Ast.loc
  | PropositionNotBool of Types.ty * Ast.loc
  | ShadowingTypeMismatch of string * Types.ty * Types.ty * Ast.loc
  | AmbiguousName of string * string list * Ast.loc
  | UnboundQualified of string * string * Ast.loc
  | PrimedExtracontextual of string * string list * Ast.loc
  | BoolParam of string * string * Ast.loc
  | ComprehensionNeedEach of Types.ty * Ast.loc
[@@deriving show]

val get_warnings : unit -> type_error list

type context = { env : Env.t; loc : Ast.loc }

val infer_type : context -> Ast.expr -> (Types.ty, type_error) result
(** Infer the type of an expression *)

val check_document : Env.t -> Ast.document -> (unit, type_error) result
(** Type-check an entire document *)
