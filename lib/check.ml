(** Pass 2: Type checking *)

open Ast
open Types
open Util

type type_error =
  | UnboundVariable of string * loc
  | UnboundType of string * loc
  | TypeMismatch of ty * ty * loc
  | ArityMismatch of int * int * loc
  | NotAFunction of ty * loc
  | NotAList of ty * loc
  | NotAProduct of ty * loc
  | NotNumeric of ty * loc
  | ExpectedBool of ty * loc
  | PrimedNonRule of string * loc
  | PrimeOutsideActionContext of string * loc
  | OverrideRequiresArity1 of string * int * loc
  | ProjectionOutOfBounds of int * int * loc
  | PropositionNotBool of ty * loc
  | ShadowingTypeMismatch of
      string * ty * ty * loc (* name, existing_ty, new_ty *)
  | AmbiguousName of string * string list * loc
  | UnboundQualified of string * string * loc
  | PrimedExtracontextual of string * string * loc
      (** function name, context name *)
[@@deriving show]

let type_warnings : type_error list ref = ref []
let get_warnings () = List.rev !type_warnings

type context = {
  env : Env.t;
  loc : loc;  (** Current location for error reporting *)
}
(** Type checking context *)

let with_loc ctx loc = { ctx with loc }

(** Infer the type of an expression *)
let rec infer_type ctx (expr : expr) : (ty, type_error) result =
  match expr with
  | ELitBool _ -> Ok TyBool
  | ELitNat 0 -> Ok TyNat0
  | ELitNat _ -> Ok TyNat
  | ELitReal _ -> Ok TyReal
  | ELitString _ -> Ok TyString
  | EVar name -> (
      match Env.lookup_term name ctx.env with
      | Some { kind = Env.KVar ty; _ } -> Ok ty
      | Some { kind = Env.KRule (TyFunc ([], Some ret)); _ } ->
          (* Nullary rule: auto-apply *)
          Ok ret
      | Some { kind = Env.KRule ty; _ } -> Ok ty
      | Some { kind = Env.KDomain | Env.KAlias _; _ } ->
          (* This shouldn't happen - domains/aliases are in type namespace *)
          Error (UnboundVariable (name, ctx.loc))
      | None -> (
          match Env.ambiguous_term_modules name ctx.env with
          | Some modules -> Error (AmbiguousName (name, modules, ctx.loc))
          | None -> Error (UnboundVariable (name, ctx.loc))))
  | EDomain name -> (
      (* Domain in expression position has type [Domain] *)
      match Env.lookup_type name ctx.env with
      | Some { kind = Env.KDomain; _ } -> Ok (TyList (TyDomain name))
      | Some { kind = Env.KAlias ty; _ } -> Ok (TyList ty)
      | Some { kind = Env.KRule _ | Env.KVar _; _ } ->
          (* This shouldn't happen - rules/vars are in term namespace *)
          Error (UnboundType (name, ctx.loc))
      | None -> (
          match Env.ambiguous_type_modules name ctx.env with
          | Some modules -> Error (AmbiguousName (name, modules, ctx.loc))
          | None -> Error (UnboundType (name, ctx.loc))))
  | EQualified (mod_name, name) -> (
      (* Try term namespace first *)
      match Env.lookup_qualified_term mod_name name ctx.env with
      | Some { kind = Env.KRule (TyFunc ([], Some ret)); _ } -> Ok ret
      | Some { kind = Env.KRule ty; _ } -> Ok ty
      | Some { kind = Env.KVar ty; _ } -> Ok ty
      | _ -> (
          (* Try type namespace (domain as set) *)
          match Env.lookup_qualified_type mod_name name ctx.env with
          | Some { kind = Env.KDomain; _ } -> Ok (TyList (TyDomain name))
          | Some { kind = Env.KAlias ty; _ } -> Ok (TyList ty)
          | _ -> Error (UnboundQualified (mod_name, name, ctx.loc))))
  | EPrimed name -> (
      if
        (* Validate: must be rule, must be in action context *)
        not (Env.in_action_context ctx.env)
      then Error (PrimeOutsideActionContext (name, ctx.loc))
      else if Env.is_local_var name ctx.env then
        Error (PrimedNonRule (name, ctx.loc))
      else
        match Env.lookup_term name ctx.env with
        | Some { kind = Env.KRule ty; _ } -> (
            let result_ty =
              match ty with TyFunc ([], Some ret) -> ret | _ -> ty
            in
            (* If action has a context, check membership *)
            match ctx.env.action_context with
            | Some ctx_name -> (
                match Env.lookup_context ctx_name ctx.env with
                | Some members when List.mem name members -> Ok result_ty
                | Some _ ->
                    Error (PrimedExtracontextual (name, ctx_name, ctx.loc))
                | None ->
                    Ok result_ty (* Shouldn't happen: validated in collect *))
            | None -> Ok result_ty)
        | Some { kind = Env.KVar _; _ } -> Error (PrimedNonRule (name, ctx.loc))
        | Some { kind = Env.KDomain | Env.KAlias _; _ } ->
            Error (UnboundVariable (name, ctx.loc))
        | None -> Error (UnboundVariable (name, ctx.loc)))
  | EApp (func, args) ->
      let* func_ty = infer_type ctx func in
      check_application ctx func func_ty args
  | ETuple exprs ->
      let* tys = map_result (infer_type ctx) exprs in
      Ok (TyProduct tys)
  | EProj (e, idx) -> (
      let* ty = infer_type ctx e in
      match ty with
      | TyProduct tys when idx >= 1 && idx <= List.length tys ->
          Ok (List.nth tys (idx - 1))
      | TyProduct tys ->
          Error (ProjectionOutOfBounds (idx, List.length tys, ctx.loc))
      | _ -> Error (NotAProduct (ty, ctx.loc)))
  | EBinop (op, e1, e2) -> check_binop ctx op e1 e2
  | EUnop (op, e) -> check_unop ctx op e
  | EForall (params, guards, body) -> check_quantifier ctx params guards body
  | EExists (params, guards, body) -> check_quantifier ctx params guards body
  | EInitially e -> infer_type ctx e
  | EOverride (name, pairs) -> check_override ctx name pairs

and check_application ctx _func_expr func_ty args =
  match func_ty with
  | TyFunc (param_tys, ret_ty) ->
      if List.length args <> List.length param_tys then
        Error (ArityMismatch (List.length param_tys, List.length args, ctx.loc))
      else begin
        match ret_ty with
        | Some ret ->
            let* _ =
              map_result
                (fun (arg, expected) ->
                  let* arg_ty = infer_type ctx arg in
                  if is_subtype arg_ty expected then Ok ()
                  else Error (TypeMismatch (expected, arg_ty, ctx.loc)))
                (List.combine args param_tys)
            in
            Ok ret
        | None ->
            (* Actions are not in term namespace, so this is unreachable *)
            assert false
      end
  | TyList elem_ty -> (
      (* List indexing or search *)
      match args with
      | [ arg ] ->
          let* arg_ty = infer_type ctx arg in
          if is_subtype arg_ty TyNat then
            (* Indexing: xs i : T *)
            Ok elem_ty
          else if is_subtype arg_ty elem_ty && not (is_numeric elem_ty) then
            (* Search: xs x : Nat + Nothing *)
            Ok (TySum [ TyNat; TyNothing ])
          else if is_numeric elem_ty then
            (* Numeric list: only indexing allowed, and arg wasn't Nat *)
            Error (TypeMismatch (TyNat, arg_ty, ctx.loc))
          else Error (TypeMismatch (elem_ty, arg_ty, ctx.loc))
      | _ -> Error (ArityMismatch (1, List.length args, ctx.loc)))
  | _ -> Error (NotAFunction (func_ty, ctx.loc))

and check_binop ctx op e1 e2 =
  let* t1 = infer_type ctx e1 in
  let* t2 = infer_type ctx e2 in
  match op with
  | OpAnd | OpOr | OpImpl | OpIff ->
      if equal_ty t1 TyBool && equal_ty t2 TyBool then Ok TyBool
      else if not (equal_ty t1 TyBool) then Error (ExpectedBool (t1, ctx.loc))
      else Error (ExpectedBool (t2, ctx.loc))
  | OpEq | OpNeq -> (
      match join t1 t2 with
      | Ok _ -> Ok TyBool
      | Error (Types.TypeMismatch (a, b)) ->
          Error (TypeMismatch (a, b, ctx.loc))
      | Error _ -> Error (TypeMismatch (t1, t2, ctx.loc)))
  | OpLt | OpGt | OpLe | OpGe ->
      if is_numeric t1 && is_numeric t2 then Ok TyBool
      else if not (is_numeric t1) then Error (NotNumeric (t1, ctx.loc))
      else Error (NotNumeric (t2, ctx.loc))
  | OpIn -> (
      (* x in s: x : T, s : [T] — x must be a subtype of T *)
      match t2 with
      | TyList elem_ty ->
          if is_subtype t1 elem_ty then Ok TyBool
          else Error (TypeMismatch (elem_ty, t1, ctx.loc))
      | _ -> Error (NotAList (t2, ctx.loc)))
  | OpSubset -> (
      (* s1 subset s2: both [T], element types must be compatible *)
      match (t1, t2) with
      | TyList a, TyList b ->
          if is_subtype a b then Ok TyBool
          else Error (TypeMismatch (t1, t2, ctx.loc))
      | TyList _, _ -> Error (NotAList (t2, ctx.loc))
      | _, _ -> Error (NotAList (t1, ctx.loc)))
  | OpAdd | OpSub | OpMul | OpDiv -> (
      match lub_numeric t1 t2 with
      | Some lub -> Ok lub
      | None ->
          if not (is_numeric t1) then Error (NotNumeric (t1, ctx.loc))
          else Error (NotNumeric (t2, ctx.loc)))

and check_unop ctx op e =
  let* ty = infer_type ctx e in
  match op with
  | OpNot ->
      if equal_ty ty TyBool then Ok TyBool
      else Error (ExpectedBool (ty, ctx.loc))
  | OpNeg ->
      if is_numeric ty then
        (* Negation promotes to at least Int *)
        Ok (if is_subtype ty TyInt then TyInt else ty)
      else Error (NotNumeric (ty, ctx.loc))
  | OpCard -> (
      match ty with
      | TyList _ -> Ok TyNat0
      | _ -> Error (NotAList (ty, ctx.loc)))

(** Check that binding doesn't shadow with a different type. Emits a warning
    (rather than an error) when it does, since propositions from different
    chapters may legitimately reuse variable names at different types. *)
and check_no_type_shadow ctx name new_ty =
  (match Env.lookup_term name ctx.env with
  | Some { kind = Env.KVar existing_ty; _ } ->
      if not (is_subtype new_ty existing_ty) then
        type_warnings :=
          ShadowingTypeMismatch (name, existing_ty, new_ty, ctx.loc)
          :: !type_warnings
  | _ -> ());
  Ok ()

(** Resolve a parameter's type expression to an internal type *)
and resolve_param_type env loc p =
  match Collect.resolve_type env p.param_type loc with
  | Ok ty -> Ok (p.param_name, ty)
  | Error (Collect.UndefinedType (name, loc)) -> Error (UnboundType (name, loc))
  | Error _ -> Error (UnboundType ("unknown", loc))

(** Process guards, extending context and collecting additional bindings. When
    [check_shadow] is true, validates that new bindings don't shadow existing
    variables with incompatible types. *)
and process_guards ~check_shadow ~loc ctx guards =
  fold_result
    (fun (bindings, current_ctx) g ->
      match g with
      | GParam p ->
          let* name, ty = resolve_param_type current_ctx.env loc p in
          let* () =
            if check_shadow then check_no_type_shadow current_ctx name ty
            else Ok ()
          in
          Ok
            ( (name, ty) :: bindings,
              { current_ctx with env = Env.add_var name ty current_ctx.env } )
      | GIn (name, list_expr) -> (
          let* list_ty = infer_type (with_loc current_ctx loc) list_expr in
          match list_ty with
          | TyList elem_ty ->
              let* () =
                if check_shadow then
                  check_no_type_shadow current_ctx name elem_ty
                else Ok ()
              in
              Ok
                ( (name, elem_ty) :: bindings,
                  {
                    current_ctx with
                    env = Env.add_var name elem_ty current_ctx.env;
                  } )
          | _ -> Error (NotAList (list_ty, loc)))
      | GExpr e ->
          let* ty = infer_type (with_loc current_ctx loc) e in
          if equal_ty ty TyBool then Ok (bindings, current_ctx)
          else Error (ExpectedBool (ty, loc)))
    ([], ctx) guards

and check_quantifier ctx params guards body =
  (* Resolve parameter types with shadow checking *)
  let* param_bindings =
    map_result
      (fun p ->
        let* name, ty = resolve_param_type ctx.env ctx.loc p in
        let* () = check_no_type_shadow ctx name ty in
        Ok (name, ty))
      params
  in
  let env' = Env.with_vars param_bindings ctx.env in
  let ctx' = { ctx with env = env' } in
  let* guard_bindings, _ =
    process_guards ~check_shadow:true ~loc:ctx.loc ctx' guards
  in
  let env'' = Env.with_vars guard_bindings ctx'.env in
  let ctx'' = { ctx with env = env'' } in
  let* body_ty = infer_type ctx'' body in
  if equal_ty body_ty TyBool then Ok TyBool
  else Error (ExpectedBool (body_ty, ctx.loc))

and check_override ctx name pairs =
  match Env.lookup_term name ctx.env with
  | Some { kind = Env.KRule (TyFunc ([ param_ty ], Some ret_ty)); _ } ->
      (* Override only for arity-1 rules *)
      let* _ =
        map_result
          (fun (k, v) ->
            let* k_ty = infer_type ctx k in
            let* v_ty = infer_type ctx v in
            let* _ =
              if is_subtype k_ty param_ty then Ok ()
              else Error (TypeMismatch (param_ty, k_ty, ctx.loc))
            in
            if is_subtype v_ty ret_ty then Ok ()
            else Error (TypeMismatch (ret_ty, v_ty, ctx.loc)))
          pairs
      in
      Ok (TyFunc ([ param_ty ], Some ret_ty))
  | Some { kind = Env.KRule (TyFunc (params, _)); _ } ->
      Error (OverrideRequiresArity1 (name, List.length params, ctx.loc))
  | _ -> Error (UnboundVariable (name, ctx.loc))

(** Check a single proposition *)
let check_proposition ctx (prop : expr located) =
  let ctx' = with_loc ctx prop.loc in
  let* ty = infer_type ctx' prop.value in
  if equal_ty ty TyBool then Ok ()
  else Error (PropositionNotBool (ty, prop.loc))

(** Check guards on a rule declaration or action *)
let check_rule_guards ctx (decl : declaration located) =
  let check_guards params guards =
    let* param_bindings =
      map_result (resolve_param_type ctx.env decl.loc) params
    in
    let env' = Env.with_vars param_bindings ctx.env in
    let ctx' = { ctx with env = env' } in
    let* _ = process_guards ~check_shadow:false ~loc:decl.loc ctx' guards in
    Ok ()
  in
  match decl.value with
  | DeclRule { params; guards; _ } -> check_guards params guards
  | DeclAction { params; guards; _ } -> check_guards params guards
  | _ -> Ok () (* Not a rule/action, nothing to check *)

(** Find the action in a chapter head, if any *)
let find_action (head : declaration located list) =
  List.find_map
    (fun decl ->
      match decl.value with
      | DeclAction { label; params; context; _ } -> Some (label, params, context)
      | _ -> None)
    head

(** Collect all rule/action parameters from a chapter head *)
let collect_all_params (head : declaration located list) =
  List.concat_map
    (fun decl ->
      match decl.value with
      | DeclRule { params; _ } | DeclAction { params; _ } -> params
      | _ -> [])
    head

(** Check a chapter body *)
let check_chapter_body ~chapter_idx env (chapter : chapter) =
  (* Filter environment for visibility in this chapter's body *)
  let env_visible = Env.visible_in_body chapter_idx env in

  (* Set action context if chapter has one *)
  let env_with_action =
    match find_action chapter.head with
    | Some (name, _, context) ->
        Env.with_action name env_visible |> Env.with_action_context context
    | None -> env_visible
  in

  (* Add ALL rule/action parameters from this chapter's head to environment *)
  let all_params = collect_all_params chapter.head in
  let env' =
    List.fold_left
      (fun env p ->
        match Collect.resolve_type env p.param_type dummy_loc with
        | Ok ty -> Env.add_var p.param_name ty env
        | Error _ -> env (* Ignore resolution errors here, caught elsewhere *))
      env_with_action all_params
  in

  let ctx = { env = env'; loc = dummy_loc } in

  map_result (check_proposition ctx) chapter.body

(** Check guards on all rule declarations in a chapter head *)
let check_chapter_guards ~chapter_idx env (chapter : chapter) =
  (* Filter environment for visibility in this chapter's head *)
  let env_visible = Env.visible_in_head chapter_idx env in
  let ctx = { env = env_visible; loc = dummy_loc } in
  map_result (check_rule_guards ctx) chapter.head

(** Check entire document (Pass 2) *)
let check_document env (doc : document) : (unit, type_error) result =
  type_warnings := [];
  let rec check_chapters chapter_idx = function
    | [] -> Ok ()
    | chapter :: rest ->
        (* Check guards in chapter head *)
        let* _ = check_chapter_guards ~chapter_idx env chapter in
        (* Check propositions in chapter body *)
        let* _ = check_chapter_body ~chapter_idx env chapter in
        check_chapters (chapter_idx + 1) rest
  in
  check_chapters 0 doc.chapters
