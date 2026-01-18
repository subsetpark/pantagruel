(** Pass 1: Collect declarations from all chapters *)

open Ast
open Types

let ( let* ) = Result.bind

type collect_error =
  | DuplicateDomain of string * loc * loc
  | DuplicateProc of string * loc * loc
  | UndefinedType of string * loc
  | RecursiveAlias of string * loc
  | MultipleVoidProcs of string * string * loc
  | BuiltinRedefined of string * loc
[@@deriving show]

(** Built-in type names that cannot be redefined *)
let builtin_types = ["Bool"; "Nat"; "Nat0"; "Int"; "Real"; "String"; "Nothing"]

let is_builtin_type name = List.mem name builtin_types

(** Check if a type mentions a given name (for recursion detection) *)
let rec mentions_type name = function
  | TyDomain n -> n = name
  | TyList t -> mentions_type name t
  | TyProduct ts | TySum ts -> List.exists (mentions_type name) ts
  | TyFunc (params, ret) ->
      List.exists (mentions_type name) params ||
      (match ret with Some t -> mentions_type name t | None -> false)
  | _ -> false

(** Convert AST type_expr to ty, expanding aliases *)
let rec resolve_type env (te : type_expr) loc : (ty, collect_error) result =
  match te with
  | TName "Bool" -> Ok TyBool
  | TName "Nat" -> Ok TyNat
  | TName "Nat0" -> Ok TyNat0
  | TName "Int" -> Ok TyInt
  | TName "Real" -> Ok TyReal
  | TName "String" -> Ok TyString
  | TName "Nothing" -> Ok TyNothing
  | TName name ->
      (match Env.lookup_type name env with
       | Some { kind = Env.KDomain; _ } -> Ok (TyDomain name)
       | Some { kind = Env.KAlias ty; _ } -> Ok ty
       | _ -> Error (UndefinedType (name, loc)))
  | TList t ->
      let* inner = resolve_type env t loc in
      Ok (TyList inner)
  | TProduct ts ->
      let* tys = Util.map_result (fun t -> resolve_type env t loc) ts in
      Ok (TyProduct tys)
  | TSum ts ->
      let* tys = Util.map_result (fun t -> resolve_type env t loc) ts in
      Ok (TySum tys)

(** Collect declarations from one chapter head.
    Uses three-pass approach so declarations can reference each other regardless of order. *)
let collect_chapter_head ~chapter env (decls : declaration located list) =
  let void_procs = ref [] in

  (* Pass 1: Register all type names (domains and alias names as placeholders) *)
  let register_type_name env (decl : declaration located) =
    match decl.value with
    | DeclDomain name ->
        if is_builtin_type name then
          Error (BuiltinRedefined (name, decl.loc))
        else begin
          match Env.lookup_type name env with
          | Some existing ->
              Error (DuplicateDomain (name, decl.loc, existing.loc))
          | None ->
              Ok (Env.add_domain name decl.loc ~chapter env)
        end
    | DeclAlias (name, _) ->
        if is_builtin_type name then
          Error (BuiltinRedefined (name, decl.loc))
        else begin
          match Env.lookup_type name env with
          | Some existing ->
              Error (DuplicateDomain (name, decl.loc, existing.loc))
          | None ->
              (* Add as domain placeholder - will be replaced in pass 2 *)
              Ok (Env.add_domain name decl.loc ~chapter env)
        end
    | DeclProc _ -> Ok env  (* Procedures handled in pass 3 *)
  in

  (* Pass 2: Resolve alias types iteratively (handles mutual references) *)
  let resolve_aliases env decls =
    (* Keep resolving until no more changes *)
    let rec iterate env =
      let changed = ref false in
      let* env' = Util.fold_result (fun env (decl : declaration located) ->
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
        | _ -> Ok env
      ) env decls in
      if !changed then iterate env' else Ok env'
    in
    iterate env
  in

  (* Pass 3: Add procedures (all types now fully resolved) *)
  let add_proc env (decl : declaration located) =
    match decl.value with
    | DeclProc { name; params; guards = _; return_type } ->
        let* param_types =
          Util.map_result (fun p -> resolve_type env p.param_type decl.loc) params
        in
        let* ret_ty =
          match return_type with
          | None ->
              void_procs := (name, decl.loc) :: !void_procs;
              Ok None
          | Some t ->
              let* ty = resolve_type env t decl.loc in
              Ok (Some ty)
        in
        let proc_ty = TyFunc (param_types, ret_ty) in
        (match Env.lookup_term name env with
         | Some existing ->
             Error (DuplicateProc (name, decl.loc, existing.loc))
         | None ->
             Ok (Env.add_proc name proc_ty decl.loc ~chapter env))
    | _ -> Ok env  (* Domains and aliases already done *)
  in

  (* Execute all three passes *)
  let* env1 = Util.fold_result register_type_name env decls in
  let* env2 = resolve_aliases env1 decls in
  let* final_env = Util.fold_result add_proc env2 decls in

  (* Validate: at most one Void procedure per chapter *)
  match !void_procs with
  | [] | [_] -> Ok final_env
  | (p1, _) :: (p2, loc) :: _ -> Error (MultipleVoidProcs (p1, p2, loc))

(** Collect all declarations from document (Pass 1) *)
let collect_all (doc : document) : (Env.t, collect_error) result =
  let env = Env.empty doc.module_name in

  (* Process all chapters, collecting declarations from heads *)
  let rec process_chapters env chapter_idx = function
    | [] -> Ok env
    | chapter :: rest ->
        let* env' = collect_chapter_head ~chapter:chapter_idx env chapter.head in
        process_chapters env' (chapter_idx + 1) rest
  in
  process_chapters env 0 doc.chapters
