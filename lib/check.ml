(** Pass 2: Type checking *)

open Ast
open Types

let ( let* ) = Result.bind

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
  | PrimedNonProcedure of string * loc
  | PrimeOutsideVoidContext of string * loc
  | VoidProcInExpression of string * loc
  | OverrideRequiresArity1 of string * int * loc
  | ProjectionOutOfBounds of int * int * loc
  | PropositionNotBool of ty * loc
  | ShadowingTypeMismatch of string * ty * ty * loc  (* name, existing_ty, new_ty *)
[@@deriving show]

(** Type checking context *)
type context = {
  env: Env.t;
  loc: loc;  (** Current location for error reporting *)
}

let with_loc ctx loc = { ctx with loc }

(** Infer the type of an expression *)
let rec infer_type ctx (expr : expr) : (ty, type_error) result =
  match expr with
  | ELitBool _ -> Ok TyBool
  | ELitNat 0 -> Ok TyNat0
  | ELitNat _ -> Ok TyNat
  | ELitReal _ -> Ok TyReal
  | ELitString _ -> Ok TyString

  | EVar name ->
      (match Env.lookup_term name ctx.env with
       | Some { kind = Env.KVar ty; _ } -> Ok ty
       | Some { kind = Env.KProc (TyFunc ([], Some ret)); _ } ->
           (* Nullary procedure: auto-apply *)
           Ok ret
       | Some { kind = Env.KProc ty; _ } -> Ok ty
       | Some { kind = (Env.KDomain | Env.KAlias _); _ } ->
           (* This shouldn't happen - domains/aliases are in type namespace *)
           Error (UnboundVariable (name, ctx.loc))
       | None -> Error (UnboundVariable (name, ctx.loc)))

  | EDomain name ->
      (* Domain in expression position has type [Domain] *)
      (match Env.lookup_type name ctx.env with
       | Some { kind = Env.KDomain; _ } -> Ok (TyList (TyDomain name))
       | Some { kind = Env.KAlias ty; _ } -> Ok (TyList ty)
       | Some { kind = (Env.KProc _ | Env.KVar _); _ } ->
           (* This shouldn't happen - procs/vars are in term namespace *)
           Error (UnboundType (name, ctx.loc))
       | None -> Error (UnboundType (name, ctx.loc)))

  | EQualified (_, _) ->
      (* TODO: implement qualified name lookup *)
      Error (UnboundVariable ("qualified names not yet implemented", ctx.loc))

  | EPrimed name ->
      (* Validate: must be procedure, must be in void context *)
      if not (Env.in_void_context ctx.env) then
        Error (PrimeOutsideVoidContext (name, ctx.loc))
      else if Env.is_local_var name ctx.env then
        Error (PrimedNonProcedure (name, ctx.loc))
      else
        (match Env.lookup_term name ctx.env with
         | Some { kind = Env.KProc ty; _ } -> Ok ty
         | Some { kind = Env.KVar _; _ } ->
             Error (PrimedNonProcedure (name, ctx.loc))
         | Some { kind = (Env.KDomain | Env.KAlias _); _ } ->
             Error (UnboundVariable (name, ctx.loc))
         | None -> Error (UnboundVariable (name, ctx.loc)))

  | EApp (func, args) ->
      let* func_ty = infer_type ctx func in
      check_application ctx func_ty args

  | ETuple exprs ->
      let* tys = Util.map_result (infer_type ctx) exprs in
      Ok (TyProduct tys)

  | EProj (e, idx) ->
      let* ty = infer_type ctx e in
      (match ty with
       | TyProduct tys when idx >= 1 && idx <= List.length tys ->
           Ok (List.nth tys (idx - 1))
       | TyProduct tys ->
           Error (ProjectionOutOfBounds (idx, List.length tys, ctx.loc))
       | _ -> Error (NotAProduct (ty, ctx.loc)))

  | EBinop (op, e1, e2) ->
      check_binop ctx op e1 e2

  | EUnop (op, e) ->
      check_unop ctx op e

  | EForall (params, guards, body) ->
      check_quantifier ctx params guards body

  | EExists (params, guards, body) ->
      check_quantifier ctx params guards body

  | EOverride (name, pairs) ->
      check_override ctx name pairs

and check_application ctx func_ty args =
  match func_ty with
  | TyFunc (param_tys, ret_ty) ->
      if List.length args <> List.length param_tys then
        Error (ArityMismatch (List.length param_tys, List.length args, ctx.loc))
      else begin
        (* Check Void procedures cannot be applied *)
        match ret_ty with
        | None ->
            (* Find the function name for error message *)
            Error (VoidProcInExpression ("void procedure", ctx.loc))
        | Some ret ->
            let* _ =
              Util.map_result (fun (arg, expected) ->
                let* arg_ty = infer_type ctx arg in
                if compatible arg_ty expected then Ok ()
                else Error (TypeMismatch (expected, arg_ty, ctx.loc)))
                (List.combine args param_tys)
            in
            Ok ret
      end

  | TyList elem_ty ->
      (* List indexing or search *)
      (match args with
       | [arg] ->
           let* arg_ty = infer_type ctx arg in
           if is_subtype arg_ty TyNat then
             (* Indexing: xs i : T *)
             Ok elem_ty
           else if compatible arg_ty elem_ty && not (is_numeric elem_ty) then
             (* Search: xs x : Nat + Nothing *)
             Ok (TySum [TyNat; TyNothing])
           else if is_numeric elem_ty then
             (* Numeric list: only indexing allowed *)
             if is_subtype arg_ty TyNat then Ok elem_ty
             else Error (TypeMismatch (TyNat, arg_ty, ctx.loc))
           else
             Error (TypeMismatch (elem_ty, arg_ty, ctx.loc))
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

  | OpEq | OpNeq ->
      (match unify t1 t2 with
       | Ok _ -> Ok TyBool
       | Error (Types.TypeMismatch (a, b)) ->
           Error (TypeMismatch (a, b, ctx.loc))
       | Error _ -> Error (TypeMismatch (t1, t2, ctx.loc)))

  | OpLt | OpGt | OpLe | OpGe ->
      if is_numeric t1 && is_numeric t2 then Ok TyBool
      else if not (is_numeric t1) then Error (NotNumeric (t1, ctx.loc))
      else Error (NotNumeric (t2, ctx.loc))

  | OpIn ->
      (* x in s: x : T, s : [T] *)
      (match t2 with
       | TyList elem_ty ->
           if compatible t1 elem_ty then Ok TyBool
           else Error (TypeMismatch (elem_ty, t1, ctx.loc))
       | _ -> Error (NotAList (t2, ctx.loc)))

  | OpSubset ->
      (* s1 subset s2: both [T] *)
      (match t1, t2 with
       | TyList a, TyList b ->
           (match unify a b with
            | Ok _ -> Ok TyBool
            | Error _ -> Error (TypeMismatch (t1, t2, ctx.loc)))
       | TyList _, _ -> Error (NotAList (t2, ctx.loc))
       | _, _ -> Error (NotAList (t1, ctx.loc)))

  | OpAdd | OpSub | OpMul | OpDiv ->
      (match lub_numeric t1 t2 with
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
  | OpCard ->
      (match ty with
       | TyList _ -> Ok TyNat0
       | _ -> Error (NotAList (ty, ctx.loc)))

(** Check that binding doesn't shadow with a different type *)
and check_no_type_shadow ctx name new_ty =
  match Env.lookup_term name ctx.env with
  | Some { kind = Env.KVar existing_ty; _ } ->
      if compatible existing_ty new_ty then Ok ()
      else Error (ShadowingTypeMismatch (name, existing_ty, new_ty, ctx.loc))
  | _ -> Ok ()  (* Not shadowing a variable, OK *)

and check_quantifier ctx params guards body =
  (* Resolve parameter types and extend environment *)
  let resolve_param p =
    match Collect.resolve_type ctx.env p.param_type ctx.loc with
    | Ok ty ->
        let* () = check_no_type_shadow ctx p.param_name ty in
        Ok (p.param_name, ty)
    | Error (Collect.UndefinedType (name, loc)) ->
        Error (UnboundType (name, loc))
    | Error _ -> Error (UnboundType ("unknown", ctx.loc))
  in
  let* param_bindings = Util.map_result resolve_param params in

  (* Extend environment with quantifier-bound variables *)
  let env' = Env.with_vars param_bindings ctx.env in
  let ctx' = { ctx with env = env' } in

  (* Process guards: check types and collect additional bindings from GIn *)
  let* (guard_bindings, _) =
    Util.fold_result (fun (bindings, current_ctx) g ->
      match g with
      | GParam p ->
          (* Additional parameter binding *)
          let* (name, ty) = resolve_param p in
          Ok ((name, ty) :: bindings, { current_ctx with env = Env.add_var name ty current_ctx.env })
      | GIn (name, list_expr) ->
          (* x in xs - infer type of xs, extract element type *)
          let* list_ty = infer_type current_ctx list_expr in
          (match list_ty with
           | TyList elem_ty ->
               let* () = check_no_type_shadow current_ctx name elem_ty in
               Ok ((name, elem_ty) :: bindings, { current_ctx with env = Env.add_var name elem_ty current_ctx.env })
           | _ -> Error (NotAList (list_ty, ctx.loc)))
      | GExpr e ->
          let* ty = infer_type current_ctx e in
          if equal_ty ty TyBool then Ok (bindings, current_ctx)
          else Error (ExpectedBool (ty, ctx.loc)))
      ([], ctx') guards
  in

  (* Extend environment with all guard bindings *)
  let env'' = Env.with_vars guard_bindings ctx'.env in
  let ctx'' = { ctx with env = env'' } in

  (* Check body is boolean *)
  let* body_ty = infer_type ctx'' body in
  if equal_ty body_ty TyBool then Ok TyBool
  else Error (ExpectedBool (body_ty, ctx.loc))

and check_override ctx name pairs =
  match Env.lookup_term name ctx.env with
  | Some { kind = Env.KProc (TyFunc ([param_ty], Some ret_ty)); _ } ->
      (* Override only for arity-1 procedures *)
      let* _ =
        Util.map_result (fun (k, v) ->
          let* k_ty = infer_type ctx k in
          let* v_ty = infer_type ctx v in
          let* _ =
            if compatible k_ty param_ty then Ok ()
            else Error (TypeMismatch (param_ty, k_ty, ctx.loc))
          in
          if compatible v_ty ret_ty then Ok ()
          else Error (TypeMismatch (ret_ty, v_ty, ctx.loc)))
          pairs
      in
      Ok (TyFunc ([param_ty], Some ret_ty))
  | Some { kind = Env.KProc (TyFunc (params, _)); _ } ->
      Error (OverrideRequiresArity1 (name, List.length params, ctx.loc))
  | _ -> Error (UnboundVariable (name, ctx.loc))

(** Check a single proposition *)
let check_proposition ctx (prop : expr located) =
  let ctx' = with_loc ctx prop.loc in
  let* ty = infer_type ctx' prop.value in
  if equal_ty ty TyBool then Ok ()
  else Error (PropositionNotBool (ty, prop.loc))

(** Check guards on a procedure declaration *)
let check_proc_guards ctx (decl : declaration located) =
  match decl.value with
  | DeclProc { params; guards; _ } ->
      (* Resolve parameter types and add them to environment *)
      let resolve_param p =
        match Collect.resolve_type ctx.env p.param_type decl.loc with
        | Ok ty -> Ok (p.param_name, ty)
        | Error (Collect.UndefinedType (name, loc)) ->
            Error (UnboundType (name, loc))
        | Error _ -> Error (UnboundType ("unknown", decl.loc))
      in
      let* param_bindings = Util.map_result resolve_param params in
      let env' = Env.with_vars param_bindings ctx.env in
      let ctx' = { ctx with env = env' } in

      (* Check each guard *)
      Util.map_result (fun g ->
        match g with
        | GParam p ->
            (* Additional parameter binding - just validate the type resolves *)
            let* _ = resolve_param p in
            Ok ()
        | GIn (_name, list_expr) ->
            (* x in xs - infer type of xs, validate it's a list *)
            let ctx'' = with_loc ctx' decl.loc in
            let* list_ty = infer_type ctx'' list_expr in
            (match list_ty with
             | TyList _ -> Ok ()  (* Valid - _name would be bound to element type *)
             | _ -> Error (NotAList (list_ty, decl.loc)))
        | GExpr e ->
            let ctx'' = with_loc ctx' decl.loc in
            let* ty = infer_type ctx'' e in
            if equal_ty ty TyBool then Ok ()
            else Error (ExpectedBool (ty, decl.loc)))
        guards
  | _ -> Ok []  (* Not a procedure, nothing to check *)

(** Find the Void procedure in a chapter head, if any *)
let find_void_proc (head : declaration located list) =
  List.find_map (fun decl ->
    match decl.value with
    | DeclProc { name; params; return_type = None; _ } -> Some (name, params)
    | _ -> None)
    head

(** Collect all procedure parameters from a chapter head *)
let collect_all_params (head : declaration located list) =
  List.concat_map (fun decl ->
    match decl.value with
    | DeclProc { params; _ } -> params
    | _ -> [])
    head

(** Check a chapter body *)
let check_chapter_body ~chapter_idx env (chapter : chapter) =
  (* Filter environment for visibility in this chapter's body *)
  let env_visible = Env.visible_in_body chapter_idx env in

  (* Set void proc context if chapter has one *)
  let env_with_void = match find_void_proc chapter.head with
    | Some (name, _) -> Env.with_void_proc name env_visible
    | None -> env_visible
  in

  (* Add ALL procedure parameters from this chapter's head to environment *)
  let all_params = collect_all_params chapter.head in
  let env' = List.fold_left (fun env p ->
    match Collect.resolve_type env p.param_type dummy_loc with
    | Ok ty -> Env.add_var p.param_name ty env
    | Error _ -> env  (* Ignore resolution errors here, caught elsewhere *)
  ) env_with_void all_params in

  let ctx = { env = env'; loc = dummy_loc } in

  Util.map_result (check_proposition ctx) chapter.body

(** Check guards on all procedure declarations in a chapter head *)
let check_chapter_guards ~chapter_idx env (chapter : chapter) =
  (* Filter environment for visibility in this chapter's head *)
  let env_visible = Env.visible_in_head chapter_idx env in
  let ctx = { env = env_visible; loc = dummy_loc } in
  Util.map_result (check_proc_guards ctx) chapter.head

(** Check entire document (Pass 2) *)
let check_document env (doc : document) : (unit, type_error) result =
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
