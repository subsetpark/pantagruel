(** Error formatting for human-readable, vim-compatible output *)

open Ast

(** Format location as file:line:col *)
let format_loc loc = Printf.sprintf "%s:%d:%d" loc.file loc.line loc.col

(** Format a type error with location prefix *)
let format_type_error err =
  let open Check in
  let fmt loc msg = Printf.sprintf "%s: error: %s" (format_loc loc) msg in
  match err with
  | UnboundVariable (name, loc) ->
      fmt loc (Printf.sprintf "Undefined variable '%s'" name)
  | UnboundType (name, loc) ->
      fmt loc (Printf.sprintf "Undefined type '%s'" name)
  | TypeMismatch (expected, got, loc) ->
      fmt loc
        (Printf.sprintf "Type mismatch: expected %s, got %s"
           (Types.format_ty expected) (Types.format_ty got))
  | ArityMismatch (expected, got, loc) ->
      fmt loc
        (Printf.sprintf "Wrong number of arguments: expected %d, got %d"
           expected got)
  | NotAFunction (ty, loc) ->
      fmt loc
        (Printf.sprintf "Cannot call %s: not a function" (Types.format_ty ty))
  | NotAList (ty, loc) ->
      fmt loc (Printf.sprintf "Expected a list, got %s" (Types.format_ty ty))
  | NotAProduct (ty, loc) ->
      fmt loc (Printf.sprintf "Expected a tuple, got %s" (Types.format_ty ty))
  | NotNumeric (ty, loc) ->
      fmt loc
        (Printf.sprintf "Expected numeric type, got %s" (Types.format_ty ty))
  | ExpectedBool (ty, loc) ->
      fmt loc (Printf.sprintf "Expected Bool, got %s" (Types.format_ty ty))
  | PrimedNonProcedure (name, loc) ->
      fmt loc (Printf.sprintf "Cannot prime '%s': not a procedure" name)
  | PrimeOutsideVoidContext (name, loc) ->
      fmt loc
        (Printf.sprintf "Primed '%s' only valid in void procedure context" name)
  | VoidProcInExpression (name, loc) ->
      fmt loc
        (Printf.sprintf "Void procedure '%s' cannot be used in expression" name)
  | OverrideRequiresArity1 (name, arity, loc) ->
      fmt loc
        (Printf.sprintf "Override requires arity-1 procedure, '%s' has arity %d"
           name arity)
  | ProjectionOutOfBounds (idx, len, loc) ->
      fmt loc
        (Printf.sprintf "Projection .%d out of bounds for %d-tuple" idx len)
  | PropositionNotBool (ty, loc) ->
      fmt loc
        (Printf.sprintf "Proposition must be Bool, got %s" (Types.format_ty ty))
  | ShadowingTypeMismatch (name, old_ty, new_ty, loc) ->
      fmt loc
        (Printf.sprintf
           "Variable '%s' shadows binding with different type: expected %s, \
            got %s"
           name (Types.format_ty old_ty) (Types.format_ty new_ty))
  | AmbiguousName (name, modules, loc) ->
      fmt loc
        (Printf.sprintf "Ambiguous name '%s': defined in %s (use Module::%s)"
           name
           (String.concat ", " modules)
           name)
  | UnboundQualified (mod_name, name, loc) ->
      fmt loc (Printf.sprintf "Undefined '%s::%s'" mod_name name)
  | PrimedExtracontextual (name, ctx_name, loc) ->
      fmt loc
        (Printf.sprintf "'%s' is not a member of context '%s'" name ctx_name)

(** Format a collection error with location prefix *)
let format_collect_error err =
  let open Collect in
  let fmt loc msg = Printf.sprintf "%s: error: %s" (format_loc loc) msg in
  match err with
  | DuplicateDomain (name, loc, first_loc) ->
      fmt loc
        (Printf.sprintf "Duplicate domain '%s' (first defined at %s)" name
           (format_loc first_loc))
  | DuplicateProc (name, loc, first_loc) ->
      fmt loc
        (Printf.sprintf "Duplicate procedure '%s' (first defined at %s)" name
           (format_loc first_loc))
  | UndefinedType (name, loc) ->
      fmt loc (Printf.sprintf "Undefined type '%s'" name)
  | RecursiveAlias (name, loc) ->
      fmt loc (Printf.sprintf "Recursive type alias '%s'" name)
  | MultipleVoidProcs (p1, p2, loc) ->
      fmt loc
        (Printf.sprintf "Multiple void procedures in chapter: '%s' and '%s'" p1
           p2)
  | BuiltinRedefined (name, loc) ->
      fmt loc (Printf.sprintf "Cannot redefine builtin type '%s'" name)
  | DuplicateContext (name, loc) ->
      fmt loc (Printf.sprintf "Duplicate context '%s'" name)
  | UndefinedContext (name, loc) ->
      fmt loc (Printf.sprintf "Undefined context '%s'" name)
