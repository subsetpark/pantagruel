(* @archlint.module core
   @archlint.domain pantagruel.collect *)

(** Pass 1: Collect declarations from all chapters *)

open Ast
open Types
open Util

type coherence_position = Param of int | Return [@@deriving show]

type collect_error =
  | DuplicateDomain of string * loc * loc
  | DuplicateRule of string * loc * loc
  | UndefinedType of string * loc
  | RecursiveAlias of string * loc
  | MultipleActions of string * string * loc
  | ActionNotLast of string * loc
  | BuiltinRedefined of string * loc
  | DuplicateContext of string * loc
  | UndefinedContext of string * loc
  | ClosureTargetInvalid of string * string * loc
  | OverloadCoherenceViolation of {
      name : string;
      position : coherence_position;
      first : string * ty * loc;
      second : string * ty * loc;
    }
[@@deriving show]

let is_builtin_type name =
  List.mem (Ast.upper_name name) Types.builtin_type_names

(** Check if a type mentions a given name (for recursion detection) *)
let rec mentions_type name = function
  | TyDomain n -> n = name
  | TyList t -> mentions_type name t
  | TyProduct ts | TySum ts -> List.exists (mentions_type name) ts
  | TyFunc (params, ret) -> (
      List.exists (mentions_type name) params
      || match ret with Some t -> mentions_type name t | None -> false)
  | TyBool | TyNat | TyNat0 | TyInt | TyReal | TyString | TyNothing -> false

(** Convert AST type_expr to ty, expanding aliases *)
let rec resolve_type env (te : type_expr) loc : (ty, collect_error) result =
  match te with
  | TName (Upper name) -> (
      match Types.builtin_of_name name with
      | Some ty -> Ok ty
      | None -> (
          match Env.lookup_type name env with
          | Some { kind = Env.KDomain; _ } -> Ok (TyDomain name)
          | Some { kind = Env.KAlias ty; _ } -> Ok ty
          | Some { kind = Env.KRule _ | Env.KVar _ | Env.KClosure _; _ } ->
              Error (UndefinedType (name, loc))
          | None -> Error (UndefinedType (name, loc))))
  | TQName (Upper mod_name, Upper type_name) -> (
      match Env.lookup_qualified_type mod_name type_name env with
      | Some { kind = Env.KDomain; _ } -> Ok (TyDomain type_name)
      | Some { kind = Env.KAlias ty; _ } -> Ok ty
      | Some { kind = Env.KRule _ | Env.KVar _ | Env.KClosure _; _ } ->
          Error (UndefinedType (mod_name ^ "::" ^ type_name, loc))
      | None -> Error (UndefinedType (mod_name ^ "::" ^ type_name, loc)))
  | TList t ->
      let* inner = resolve_type env t loc in
      Ok (TyList inner)
  | TProduct ts ->
      let* tys = map_result (fun t -> resolve_type env t loc) ts in
      Ok (TyProduct tys)
  | TSum ts ->
      let* tys = map_result (fun t -> resolve_type env t loc) ts in
      Ok (TySum tys)

(** Verify that a new rule declaration is coherent with any existing local
    overloads of the same name. Coherence (positional): all overloads share the
    return type, and at every shared parameter position both the name and type
    agree. Only local declarations participate in the check — imported overloads
    are considered a different family to keep cross-module ambiguity a separate
    concern. *)
let check_overload_coherence name (params : Ast.param list) param_types
    (ret_ty : ty) (new_loc : Ast.loc) env : (unit, collect_error) result =
  let new_arity = List.length params in
  let rec check_against = function
    | [] -> Ok ()
    | (existing_arity, (entry : Env.entry)) :: rest ->
        if entry.module_origin <> None then check_against rest
        else
          let existing_ret, existing_param_types, is_closure =
            match[@warning "-4"] entry.kind with
            | Env.KRule (TyFunc (ptys, Some r)) -> (r, ptys, false)
            | Env.KClosure (TyFunc (ptys, Some r), _) -> (r, ptys, true)
            | _ -> (TyNothing, [], false)
          in
          let* () =
            if equal_ty existing_ret ret_ty then Ok ()
            else
              Error
                (OverloadCoherenceViolation
                   {
                     name;
                     position = Return;
                     first = ("", existing_ret, entry.loc);
                     second = ("", ret_ty, new_loc);
                   })
          in
          (* Closures do not populate [Env.rule_guards], so the formal
             parameter names required for positional coherence aren't
             available. Skip the positional name-check for a closure
             overload — return-type coherence above still runs, and the
             param-type check [check_position] would normally do is also
             performed here against [existing_param_types]. *)
          let existing_params =
            match Env.lookup_rule_guards_arity name existing_arity env with
            | Some (ps, _) -> Some ps
            | None -> None
          in
          let shared = min new_arity existing_arity in
          let rec check_position i =
            if i >= shared then Ok ()
            else
              let newp : Ast.param = List.nth params i in
              let newp_name = Ast.lower_name newp.param_name in
              let newp_ty = List.nth param_types i in
              let exp_ty = List.nth existing_param_types i in
              match existing_params with
              | None when is_closure ->
                  (* Closure has no formal-name metadata. Check type
                     agreement only; skip positional name comparison. *)
                  if equal_ty exp_ty newp_ty then check_position (i + 1)
                  else
                    Error
                      (OverloadCoherenceViolation
                         {
                           name;
                           position = Param i;
                           first = ("", exp_ty, entry.loc);
                           second = (newp_name, newp_ty, new_loc);
                         })
              | None ->
                  (* Non-closure missing rule_guards entry shouldn't happen;
                     defensively compare types only. *)
                  if equal_ty exp_ty newp_ty then check_position (i + 1)
                  else
                    Error
                      (OverloadCoherenceViolation
                         {
                           name;
                           position = Param i;
                           first = ("", exp_ty, entry.loc);
                           second = (newp_name, newp_ty, new_loc);
                         })
              | Some ps ->
                  let exp : Ast.param = List.nth ps i in
                  let exp_name = Ast.lower_name exp.param_name in
                  if exp_name = newp_name && equal_ty exp_ty newp_ty then
                    check_position (i + 1)
                  else
                    Error
                      (OverloadCoherenceViolation
                         {
                           name;
                           position = Param i;
                           first = (exp_name, exp_ty, entry.loc);
                           second = (newp_name, newp_ty, new_loc);
                         })
          in
          let* () = check_position 0 in
          check_against rest
  in
  check_against (Env.overloads_of name env)

(** Collect declarations from one chapter head. Uses multi-pass approach so
    declarations can reference each other regardless of order. *)
let collect_chapter_head ~chapter ~doc_contexts env
    (decls : declaration located list) =
  let actions = ref [] in

  (* Pass 1: Register all type names (domains and alias names as placeholders) *)
  let register_type_name env (decl : declaration located) =
    match decl.value with
    | DeclDomain (Upper name) ->
        if is_builtin_type (Upper name) then
          Error (BuiltinRedefined (name, decl.loc))
        else begin
          match Env.lookup_type name env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateDomain (name, decl.loc, existing.loc))
          | _ -> Ok (Env.add_domain name decl.loc ~chapter env)
        end
    | DeclAlias (Upper name, _) ->
        if is_builtin_type (Upper name) then
          Error (BuiltinRedefined (name, decl.loc))
        else begin
          match Env.lookup_type name env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateDomain (name, decl.loc, existing.loc))
          | _ ->
              (* Add as domain placeholder - will be replaced in pass 2 *)
              Ok (Env.add_domain name decl.loc ~chapter env)
        end
    | DeclRule _ | DeclAction _ | DeclClosure _ ->
        Ok env (* Handled in later passes *)
  in

  (* Pass 2: Resolve alias types iteratively (handles mutual references) *)
  let resolve_aliases env decls =
    (* Keep resolving until no more changes *)
    let rec iterate env =
      let changed = ref false in
      let* env' =
        fold_result
          (fun env (decl : declaration located) ->
            match decl.value with
            | DeclAlias (Upper name, type_expr) ->
                let* ty = resolve_type env type_expr decl.loc in
                if mentions_type name ty then
                  Error (RecursiveAlias (name, decl.loc))
                else begin
                  (* Check if this changes anything *)
                  (match Env.lookup_type name env with
                  | Some { kind = Env.KAlias existing_ty; _ } ->
                      if existing_ty <> ty then changed := true
                  | Some
                      {
                        kind =
                          ( Env.KDomain | Env.KRule _ | Env.KVar _
                          | Env.KClosure _ );
                        _;
                      } ->
                      changed := true
                  | None -> changed := true);
                  Ok (Env.add_alias name ty decl.loc ~chapter env)
                end
            | DeclDomain _ | DeclRule _ | DeclAction _ | DeclClosure _ -> Ok env)
          env decls
      in
      if !changed then iterate env' else Ok env'
    in
    iterate env
  in

  (* Pass 3: Add rules/actions and register context footprints *)
  let add_rule env (decl : declaration located) =
    match decl.value with
    | DeclRule { name = Lower name; params; guards; return_type; contexts } ->
        let* param_types =
          map_result (fun p -> resolve_type env p.param_type decl.loc) params
        in
        let* ret_ty = resolve_type env return_type decl.loc in
        (* Validate context footprint: each named context must exist in doc.contexts *)
        let ctx_names = List.map Ast.upper_name contexts in
        let* () =
          map_result
            (fun ctx_name ->
              if
                List.exists
                  (fun (c : upper_ident located) ->
                    Ast.upper_name c.value = ctx_name)
                  doc_contexts
              then Ok ()
              else Error (UndefinedContext (ctx_name, decl.loc)))
            ctx_names
          |> Result.map (fun _ -> ())
        in
        let proc_ty = TyFunc (param_types, Some ret_ty) in
        let* env =
          let new_arity = List.length params in
          match Env.lookup_term_arity name new_arity env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateRule (name, decl.loc, existing.loc))
          | _ ->
              let* () =
                check_overload_coherence name params param_types ret_ty decl.loc
                  env
              in
              Ok (Env.add_rule name proc_ty decl.loc ~chapter env)
        in
        let env = Env.add_rule_guards name params guards env in
        (* Add rule to each context's member list *)
        let env =
          List.fold_left
            (fun env ctx_name -> Env.add_rule_to_context ctx_name name env)
            env ctx_names
        in
        Ok env
    | DeclAction { label; params; guards; _ } ->
        let* _param_types =
          map_result (fun p -> resolve_type env p.param_type decl.loc) params
        in
        let env = Env.add_rule_guards label params guards env in
        actions := (label, decl.loc) :: !actions;
        Ok env
    | DeclClosure
        { name = Lower name; param; return_type; target = Lower target } ->
        (* Validate: no duplicate rule name. Closures are always arity-1, so
           check for a same-arity collision. *)
        let* env =
          match Env.lookup_term_arity name 1 env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateRule (name, decl.loc, existing.loc))
          | _ -> Ok env
        in
        (* Resolve param type and return type *)
        let* param_ty = resolve_type env param.param_type decl.loc in
        let* ret_ty = resolve_type env return_type decl.loc in
        (* Return type must be [T] where T matches param type *)
        let* () =
          match[@warning "-4"] (ret_ty, param_ty) with
          | TyList t, _ when t = param_ty -> Ok ()
          | _ ->
              Error
                (ClosureTargetInvalid
                   ( name,
                     Printf.sprintf "return type must be [%s]"
                       (Types.format_ty param_ty),
                     decl.loc ))
        in
        (* Look up the target rule — closure targets are always unary, so
           probe the arity-1 slot so unary targets are found even when the
           name has other arity overloads. *)
        let* () =
          match[@warning "-4"] Env.lookup_term_arity target 1 env with
          | Some { kind = Env.KRule ty; _ } -> (
              match[@warning "-4"] ty with
              (* T => [T] (zero-or-more children per node) *)
              | TyFunc ([ t1 ], Some (TyList t2))
                when t1 = param_ty && t2 = param_ty ->
                  Ok ()
              | _ ->
                  Error
                    (ClosureTargetInvalid
                       ( name,
                         Printf.sprintf "target '%s' must have shape %s => [%s]"
                           target (Types.format_ty param_ty)
                           (Types.format_ty param_ty),
                         decl.loc )))
          | _ ->
              Error
                (ClosureTargetInvalid
                   ( name,
                     Printf.sprintf "target '%s' not found" target,
                     decl.loc ))
        in
        let closure_ty = TyFunc ([ param_ty ], Some (TyList param_ty)) in
        (* Closures must satisfy positional coherence with any other
           overloads of [name] already declared locally, just like rules.
           Without this check, acceptance of e.g. [closure f/1] alongside
           [rule f/2] with an incompatible shared-position type depends on
           declaration order. *)
        let* () =
          check_overload_coherence name [ param ] [ param_ty ] (TyList param_ty)
            decl.loc env
        in
        Ok (Env.add_closure name closure_ty target decl.loc ~chapter env)
    | DeclDomain _ | DeclAlias _ ->
        Ok env (* Domains and aliases already done *)
  in

  (* Pass 4: Validate context annotations on actions *)
  let validate_action_context env (decl : declaration located) =
    match decl.value with
    | DeclAction { contexts; _ } ->
        let* () =
          map_result
            (fun ctx ->
              let ctx_name = Ast.upper_name ctx in
              match Env.lookup_context ctx_name env with
              | None -> Error (UndefinedContext (ctx_name, decl.loc))
              | Some _ -> Ok ())
            contexts
          |> Result.map (fun _ -> ())
        in
        Ok env
    | DeclDomain _ | DeclAlias _ | DeclRule _ | DeclClosure _ -> Ok env
  in

  (* Execute all passes *)
  let* env1 = fold_result register_type_name env decls in
  let* env2 = resolve_aliases env1 decls in
  let* env3 = fold_result add_rule env2 decls in
  let* final_env = fold_result validate_action_context env3 decls in

  (* Validate: at most one action per chapter *)
  let* () =
    match !actions with
    | [] | [ _ ] -> Ok ()
    | (p1, _) :: (p2, loc) :: _ -> Error (MultipleActions (p1, p2, loc))
  in

  (* Validate: action must appear last in chapter head *)
  let* () =
    let rec check_last = function
      | [] -> Ok ()
      | [ _ ] -> Ok ()
      | { value = DeclAction { label; _ }; loc; _ } :: _ :: _ ->
          Error (ActionNotLast (label, loc))
      | { value = DeclDomain _ | DeclAlias _ | DeclRule _ | DeclClosure _; _ }
        :: rest ->
          check_last rest
    in
    check_last decls
  in

  Ok final_env

(** Try to recognize a chapter-body proposition as a split-form rule definition
    equation: `<rule-name> <param-name>* = <body>`. The LHS must be an
    application of a declared rule (in [chapter_head], same chapter) to bare
    variable references that exactly match the rule's parameter names in order —
    equivalent to the inline form `<name> <params> => <type> = <body>.` modulo
    the chapter divider. Returns [Some (decl, body)] if matched, [None]
    otherwise. *)
let recognize_rule_definition_eq
    (chapter_head : Ast.declaration Ast.located list) (e : Ast.expr) :
    (Ast.declaration * Ast.expr) option =
  let module StringSet = Set.Make (String) in
  let nullary_rule_names =
    List.fold_left
      (fun acc (decl : Ast.declaration Ast.located) ->
        match[@warning "-4"] decl.value with
        | DeclRule { name = Lower n; params = []; _ } -> StringSet.add n acc
        | _ -> acc)
      StringSet.empty chapter_head
  in
  let rec value_vars acc = function
    | EVar (Lower n) | EPrimed (Lower n) -> StringSet.add n acc
    | EApp (EVar _, args) -> List.fold_left value_vars acc args
    | EApp (func, args) -> List.fold_left value_vars (value_vars acc func) args
    | EBinop (_, e1, e2) -> value_vars (value_vars acc e1) e2
    | EUnop (_, e) | EProj (e, _) | EInitially e -> value_vars acc e
    | ETuple es -> List.fold_left value_vars acc es
    | EOverride (_, pairs) ->
        List.fold_left
          (fun acc (k, v) -> value_vars (value_vars acc k) v)
          acc pairs
    | ECond arms ->
        List.fold_left
          (fun acc (g, c) -> value_vars (value_vars acc g) c)
          acc arms
    | EForall (mb, metas) | EExists (mb, metas) | EEach (mb, metas, _) ->
        let params, guards, body = Ast.unbind_quant mb metas in
        let bound =
          List.fold_left
            (fun s (p : Ast.param) ->
              StringSet.add (Ast.lower_name p.param_name) s)
            StringSet.empty params
        in
        let guard_vars =
          List.fold_left
            (fun acc -> function
              | GParam _ -> acc
              | GIn (_, list_expr) -> value_vars acc list_expr
              | GExpr e -> value_vars acc e)
            StringSet.empty guards
        in
        StringSet.union acc
          (StringSet.diff
             (StringSet.union guard_vars (value_vars StringSet.empty body))
             bound)
    | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ | EDomain _
    | EQualified _ ->
        acc
  in
  let rhs_vars_bound_by_lhs rhs arg_names =
    let allowed =
      List.fold_left
        (fun acc name -> StringSet.add name acc)
        nullary_rule_names arg_names
    in
    StringSet.subset (value_vars StringSet.empty rhs) allowed
  in
  let arg_to_name (e : Ast.expr) : string option =
    match[@warning "-4"] e with EVar (Lower an) -> Some an | _ -> None
  in
  let match_lhs (e : Ast.expr) : (string * string list) option =
    match[@warning "-4"] e with
    | EApp (EVar (Lower n), args) ->
        let arg_names = List.filter_map arg_to_name args in
        if List.length arg_names = List.length args then Some (n, arg_names)
        else None
    | EVar (Lower n) -> Some (n, [])
    | _ -> None
  in
  match[@warning "-4"] e with
  | EBinop (OpEq, lhs, rhs) -> (
      match match_lhs lhs with
      | None -> None
      | Some (rule_name, arg_names) ->
          List.find_map
            (fun (decl : Ast.declaration Ast.located) ->
              match[@warning "-4"] decl.value with
              | DeclRule { name = Lower dn; params; _ }
                when dn = rule_name
                     && List.length params = List.length arg_names
                     && List.equal String.equal arg_names
                          (List.map
                             (fun (p : Ast.param) ->
                               Ast.lower_name p.param_name)
                             params)
                     && rhs_vars_bound_by_lhs rhs arg_names ->
                  Some (decl.value, rhs)
              | _ -> None)
            chapter_head)
  | _ -> None

(** Walk each chapter's body looking for split-form rule definition equations.
    Matched equations are registered as rule bodies in [env.rule_bodies] (so the
    SMT layer routes recursive ones to [define-fun-rec] exactly as inline bodies
    do) and removed from the chapter body (so the same equation is not also
    re-emitted as a quantified axiom). Returns the updated [env] and document.
*)
let recognize_split_form_bodies (env : Env.t) (doc : Ast.document) :
    Env.t * Ast.document =
  let process_chapter env (chapter : Ast.chapter) =
    let body', env =
      List.fold_left
        (fun (acc, env) (prop : Ast.expr Ast.located) ->
          match[@warning "-4"]
            recognize_rule_definition_eq chapter.head prop.value
          with
          | Some ((DeclRule { name = Lower name; params; _ } as decl), body)
            -> (
              let arity = List.length params in
              match Env.lookup_rule_body_arity name arity env with
              | Some _ ->
                  (* Keep duplicate equations in the body so they are not
                     silently discarded by overwriting [rule_bodies]. *)
                  (prop :: acc, env)
              | None ->
                  let env = Env.attach_rule_body name arity decl body env in
                  (acc, env))
          | Some _ | None -> (prop :: acc, env))
        ([], env) chapter.body
    in
    ({ chapter with body = List.rev body' }, env)
  in
  let chapters', env =
    List.fold_left
      (fun (acc, env) chapter ->
        let chapter', env = process_chapter env chapter in
        (chapter' :: acc, env))
      ([], env) doc.chapters
  in
  (env, { doc with chapters = List.rev chapters' })

(** Collect all declarations from document (Pass 1) *)
let collect_all ~base_env (doc : document) : (Env.t, collect_error) result =
  let mod_name = Option.fold ~none:"" ~some:Ast.upper_name doc.module_name in
  let env = Env.with_module_init mod_name base_env in

  (* Register context names from module-level declarations *)
  let* env =
    fold_result
      (fun env (ctx : upper_ident located) ->
        let ctx_name = Ast.upper_name ctx.value in
        match Env.lookup_context ctx_name env with
        | Some _ -> Error (DuplicateContext (ctx_name, ctx.loc))
        | None -> Ok (Env.add_context ctx_name [] env))
      env doc.contexts
  in

  (* Process all chapters, collecting declarations from heads *)
  let rec process_chapters env chapter_idx = function
    | [] -> Ok env
    | chapter :: rest ->
        let* env' =
          collect_chapter_head ~chapter:chapter_idx ~doc_contexts:doc.contexts
            env chapter.head
        in
        process_chapters env' (chapter_idx + 1) rest
  in
  process_chapters env 0 doc.chapters
