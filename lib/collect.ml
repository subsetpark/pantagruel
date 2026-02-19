(** Pass 1: Collect declarations from all chapters *)

open Ast
open Types
open Util

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
[@@deriving show]

let is_builtin_type name = List.mem name Types.builtin_type_names

(** Check if a type mentions a given name (for recursion detection) *)
let rec mentions_type name = function
  | TyDomain n -> n = name
  | TyList t -> mentions_type name t
  | TyProduct ts | TySum ts -> List.exists (mentions_type name) ts
  | TyFunc (params, ret) -> (
      List.exists (mentions_type name) params
      || match ret with Some t -> mentions_type name t | None -> false)
  | _ -> false

(** Convert AST type_expr to ty, expanding aliases *)
let rec resolve_type env (te : type_expr) loc : (ty, collect_error) result =
  match te with
  | TName name -> (
      match Types.builtin_of_name name with
      | Some ty -> Ok ty
      | None -> (
          match Env.lookup_type name env with
          | Some { kind = Env.KDomain; _ } -> Ok (TyDomain name)
          | Some { kind = Env.KAlias ty; _ } -> Ok ty
          | _ -> Error (UndefinedType (name, loc))))
  | TQName (mod_name, type_name) -> (
      match Env.lookup_qualified_type mod_name type_name env with
      | Some { kind = Env.KDomain; _ } -> Ok (TyDomain type_name)
      | Some { kind = Env.KAlias ty; _ } -> Ok ty
      | _ -> Error (UndefinedType (mod_name ^ "::" ^ type_name, loc)))
  | TList t ->
      let* inner = resolve_type env t loc in
      Ok (TyList inner)
  | TProduct ts ->
      let* tys = map_result (fun t -> resolve_type env t loc) ts in
      Ok (TyProduct tys)
  | TSum ts ->
      let* tys = map_result (fun t -> resolve_type env t loc) ts in
      Ok (TySum tys)

(** Collect declarations from one chapter head. Uses multi-pass approach so
    declarations can reference each other regardless of order. *)
let collect_chapter_head ~chapter ~doc_contexts env
    (decls : declaration located list) =
  let actions = ref [] in

  (* Pass 1: Register all type names (domains and alias names as placeholders) *)
  let register_type_name env (decl : declaration located) =
    match decl.value with
    | DeclDomain name ->
        if is_builtin_type name then Error (BuiltinRedefined (name, decl.loc))
        else begin
          match Env.lookup_type name env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateDomain (name, decl.loc, existing.loc))
          | _ -> Ok (Env.add_domain name decl.loc ~chapter env)
        end
    | DeclAlias (name, _) ->
        if is_builtin_type name then Error (BuiltinRedefined (name, decl.loc))
        else begin
          match Env.lookup_type name env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateDomain (name, decl.loc, existing.loc))
          | _ ->
              (* Add as domain placeholder - will be replaced in pass 2 *)
              Ok (Env.add_domain name decl.loc ~chapter env)
        end
    | DeclRule _ | DeclAction _ -> Ok env (* Handled in later passes *)
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
            | DeclAlias (name, type_expr) ->
                let* ty = resolve_type env type_expr decl.loc in
                if mentions_type name ty then
                  Error (RecursiveAlias (name, decl.loc))
                else begin
                  (* Check if this changes anything *)
                  (match Env.lookup_type name env with
                  | Some { kind = Env.KAlias existing_ty; _ } ->
                      if existing_ty <> ty then changed := true
                  | _ -> changed := true);
                  Ok (Env.add_alias name ty decl.loc ~chapter env)
                end
            | _ -> Ok env)
          env decls
      in
      if !changed then iterate env' else Ok env'
    in
    iterate env
  in

  (* Pass 3: Add rules/actions and register context footprints *)
  let add_rule env (decl : declaration located) =
    match decl.value with
    | DeclRule { name; params; guards = _; return_type; contexts } ->
        let* param_types =
          map_result (fun p -> resolve_type env p.param_type decl.loc) params
        in
        let* ret_ty = resolve_type env return_type decl.loc in
        (* Validate context footprint: each named context must exist in doc.contexts *)
        let* () =
          map_result
            (fun ctx_name ->
              if
                List.exists
                  (fun (c : upper_ident located) -> c.value = ctx_name)
                  doc_contexts
              then Ok ()
              else Error (UndefinedContext (ctx_name, decl.loc)))
            contexts
          |> Result.map (fun _ -> ())
        in
        let proc_ty = TyFunc (param_types, Some ret_ty) in
        let* env =
          match Env.lookup_term name env with
          | Some existing when existing.module_origin = None ->
              Error (DuplicateRule (name, decl.loc, existing.loc))
          | _ -> Ok (Env.add_rule name proc_ty decl.loc ~chapter env)
        in
        (* Add rule to each context's member list *)
        let env =
          List.fold_left
            (fun env ctx_name -> Env.add_rule_to_context ctx_name name env)
            env contexts
        in
        Ok env
    | DeclAction { label; params; guards = _; _ } ->
        let* _param_types =
          map_result (fun p -> resolve_type env p.param_type decl.loc) params
        in
        actions := (label, decl.loc) :: !actions;
        Ok env
    | _ -> Ok env (* Domains and aliases already done *)
  in

  (* Pass 4: Validate context annotations on actions *)
  let validate_action_context env (decl : declaration located) =
    match decl.value with
    | DeclAction { context = Some ctx_name; _ } -> (
        (* Check context exists (local or imported) *)
        match Env.lookup_context ctx_name env with
        | None -> Error (UndefinedContext (ctx_name, decl.loc))
        | Some _ -> Ok env)
    | _ -> Ok env
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
      | _ :: rest -> check_last rest
    in
    check_last decls
  in

  Ok final_env

(** Collect all declarations from document (Pass 1) *)
let collect_all ~base_env (doc : document) : (Env.t, collect_error) result =
  let mod_name = Option.value ~default:"" doc.module_name in
  let env =
    {
      base_env with
      Env.current_module = mod_name;
      action = None;
      local_vars = [];
    }
  in

  (* Register context names from module-level declarations *)
  let* env =
    fold_result
      (fun env (ctx : upper_ident located) ->
        match Env.lookup_context ctx.value env with
        | Some _ -> Error (DuplicateContext (ctx.value, ctx.loc))
        | None -> Ok (Env.add_context ctx.value [] env))
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
