(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** SMT-LIB2 document analysis: chapter classification, invariants, actions,
    frame conditions, and action parameter handling. *)

type chapter_class =
  | Invariant of {
      head_bindings : Ast.param list;
      propositions : Ast.expr Ast.located list;
      checks : Ast.expr Ast.located list;
    }
  | Action of {
      label : string;
      params : Ast.param list;
      guards : Ast.guard list;
      contexts : string list;
      head_bindings : Ast.param list;
      propositions : Ast.expr Ast.located list;
      checks : Ast.expr Ast.located list;
    }

module StringSet : Set.S with type elt = string

val free_vars : Ast.expr -> StringSet.t

val bind_head_params :
  ?exclude:StringSet.t ->
  Ast.param list ->
  Ast.expr Ast.located ->
  Ast.expr Ast.located

val classify_chapters : Ast.document -> chapter_class list
val collect_invariants : chapter_class list -> Ast.expr Ast.located list
val collect_initial_props : chapter_class list -> Ast.expr Ast.located list

type action_info = {
  a_label : string;
  a_params : Ast.param list;
  a_guards : Ast.guard list;
  a_contexts : string list;
  a_propositions : Ast.expr Ast.located list;
}

val collect_actions : chapter_class list -> action_info list

type check_context =
  | CheckInvariant of Ast.expr Ast.located list
  | CheckAction of action_info

val collect_checks :
  chapter_class list -> (Ast.expr Ast.located * check_context) list

val collect_frame_exprs :
  Smt_types.config -> Env.t -> string list -> (string * string) list

val generate_frame_conditions :
  Smt_types.config -> Env.t -> string list -> string

val env_with_action_params : Env.t -> Ast.param list -> Env.t
val declare_action_params : Env.t -> Ast.param list -> string
val declare_param_constraints : Env.t -> Ast.param list -> string
val extract_precondition_exprs : Ast.guard list -> Ast.expr list
val action_always_enabled : action_info -> bool
val collect_function_refs : Ast.expr -> string list

val invariant_touches_context :
  Env.t -> string list -> Ast.expr Ast.located list -> bool
