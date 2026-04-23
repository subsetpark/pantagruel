(** Pass 1: Collect declarations from all chapters *)

(** Which coordinate of a rule family two arity-overloads disagree on. Used by
    [OverloadCoherenceViolation] to discriminate between param-position
    mismatches and return-type mismatches. *)
type coherence_position = Param of int | Return [@@deriving show]

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
  | OverloadCoherenceViolation of {
      name : string;
      position : coherence_position;
      first : string * Types.ty * Ast.loc;
      second : string * Types.ty * Ast.loc;
    }
      (** Two declarations of [name] with different arities disagree on the
          parameter name/type at a shared position, or on the return type. The
          [first] tuple describes the previously-declared overload at that
          coordinate; [second] describes the new declaration. At [Return]
          position the name field is the empty string (unused). *)
[@@deriving show]

val resolve_type :
  Env.t -> Ast.type_expr -> Ast.loc -> (Types.ty, collect_error) result
(** Resolve a syntactic type expression to an internal type *)

val collect_all :
  base_env:Env.t -> Ast.document -> (Env.t, collect_error) result
(** Collect all declarations from a document *)
