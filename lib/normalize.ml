(** Normal form transformation for Pantagruel documents.

    A document is in normal form when declarations are organized by their
    dependency levels, and propositions are placed in the earliest valid chapter
    based on their dependencies.

    The number of chapters is determined by the topological depth of the
    declaration dependency graph.

    Visibility rules:
    - Declaration in chapter N is visible in heads of chapter M where M >= N
    - Declaration in chapter N is visible in bodies of chapter M where M >= N-1

    Algorithm: 1. Build dependency graph of declarations (based on type
    references) 2. Topologically sort declarations into levels 3. Each level
    becomes a chapter 4. Propositions are placed in the earliest body where all
    dependencies are visible 5. Void procedures and their tied propositions stay
    together *)

open Ast
module StringSet = Set.Make (String)

type decl_info = {
  name : string;
  is_type : bool;  (** true for domains/aliases, false for procedures *)
  is_void : bool;  (** true for void procedures *)
  decl : declaration located;
  dependencies : StringSet.t;  (** Type names this declaration depends on *)
  mutable level : int;  (** Topological level, computed later *)
}
(** Information about a declaration *)

type void_unit = {
  void_proc : decl_info;
  tied_props : expr located list;
      (** Props using primed exprs or void proc params *)
}
(** A void unit: a void procedure + its associated propositions *)

(** Extract type names from a type expression *)
let rec types_in_type_expr = function
  | TName name -> StringSet.singleton name
  | TQName _ -> StringSet.empty (* Qualified refs are imports, not local deps *)
  | TList t -> types_in_type_expr t
  | TProduct ts | TSum ts ->
      List.fold_left
        (fun acc t -> StringSet.union acc (types_in_type_expr t))
        StringSet.empty ts

(** Extract type and term names from an expression *)
let symbols_in_expr expr =
  let types = ref StringSet.empty in
  let terms = ref StringSet.empty in
  let rec go = function
    | EVar name -> terms := StringSet.add name !terms
    | EDomain name -> types := StringSet.add name !types
    | EQualified _ -> () (* Skip qualified names for now *)
    | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ -> ()
    | EApp (f, args) ->
        go f;
        List.iter go args
    | EPrimed name -> terms := StringSet.add name !terms
    | EOverride (name, pairs) ->
        terms := StringSet.add name !terms;
        List.iter
          (fun (k, v) ->
            go k;
            go v)
          pairs
    | ETuple es -> List.iter go es
    | EProj (e, _) -> go e
    | EBinop (_, e1, e2) ->
        go e1;
        go e2
    | EUnop (_, e) -> go e
    | EForall (params, guards, body) | EExists (params, guards, body) ->
        List.iter
          (fun p ->
            types := StringSet.union !types (types_in_type_expr p.param_type))
          params;
        List.iter
          (function
            | GParam p ->
                types :=
                  StringSet.union !types (types_in_type_expr p.param_type)
            | GIn (_, e) ->
                go e (* x in xs - the list expression has dependencies *)
            | GExpr e -> go e)
          guards;
        go body
  in
  go expr;
  (!types, !terms)

(** Extract symbols from a declaration's dependencies (not the symbol it
    defines) *)
let symbols_in_decl_deps (decl : declaration) =
  let types = ref StringSet.empty in
  let terms = ref StringSet.empty in
  (match decl with
  | DeclDomain _ -> () (* No dependencies *)
  | DeclAlias (_, te) -> types := types_in_type_expr te
  | DeclProc { params; guards; return_type; _ } ->
      List.iter
        (fun p ->
          types := StringSet.union !types (types_in_type_expr p.param_type))
        params;
      List.iter
        (function
          | GParam p ->
              types := StringSet.union !types (types_in_type_expr p.param_type)
          | GIn (_, e) ->
              let t, m = symbols_in_expr e in
              types := StringSet.union !types t;
              terms := StringSet.union !terms m
          | GExpr e ->
              let t, m = symbols_in_expr e in
              types := StringSet.union !types t;
              terms := StringSet.union !terms m)
        guards;
      Option.iter
        (fun te -> types := StringSet.union !types (types_in_type_expr te))
        return_type);
  (!types, !terms)

(** Get the name defined by a declaration *)
let decl_name = function
  | DeclDomain name -> name
  | DeclAlias (name, _) -> name
  | DeclProc { name; _ } -> name

(** Is this a type-namespace declaration? *)
let is_type_decl = function
  | DeclDomain _ | DeclAlias _ -> true
  | DeclProc _ -> false

(** Is this a void procedure? *)
let is_void_proc = function
  | DeclProc { return_type = None; _ } -> true
  | DeclDomain _ | DeclAlias _ | DeclProc _ -> false

(** Check if an expression uses any primed names *)
let rec uses_primed = function
  | EPrimed _ -> true
  | EVar _ | EDomain _ | EQualified _ -> false
  | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ -> false
  | EApp (f, args) -> uses_primed f || List.exists uses_primed args
  | EOverride (_, pairs) ->
      List.exists (fun (k, v) -> uses_primed k || uses_primed v) pairs
  | ETuple es -> List.exists uses_primed es
  | EProj (e, _) -> uses_primed e
  | EBinop (_, e1, e2) -> uses_primed e1 || uses_primed e2
  | EUnop (_, e) -> uses_primed e
  | EForall (_, guards, body) | EExists (_, guards, body) ->
      List.exists
        (function
          | GExpr e -> uses_primed e
          | GIn (_, e) -> uses_primed e
          | GParam _ -> false)
        guards
      || uses_primed body

(** Check if a chapter body uses primed expressions *)
let body_uses_primed (props : expr located list) =
  List.exists (fun p -> uses_primed p.value) props

(** Get void procedure parameters from a chapter *)
let void_proc_params (chapter : chapter) : StringSet.t =
  let params =
    List.find_map
      (fun decl ->
        match decl.value with
        | DeclProc { return_type = None; params; _ } ->
            Some (List.map (fun p -> p.param_name) params)
        | _ -> None)
      chapter.head
  in
  match params with Some ps -> StringSet.of_list ps | None -> StringSet.empty

(** Check if a proposition references any of the given parameter names *)
let prop_uses_params params prop =
  let _, terms = symbols_in_expr prop in
  not (StringSet.is_empty (StringSet.inter params terms))

(** Built-in type names that don't need declarations *)
let builtin_types = StringSet.of_list Types.builtin_type_names

(** Compute topological levels for declarations. Level 0 = no dependencies
    (roots), Level 1 = depends only on level 0, etc. *)
let compute_levels (decls : decl_info list) : int =
  let by_name = Hashtbl.create 32 in
  List.iter (fun d -> Hashtbl.add by_name d.name d) decls;

  (* Initialize all levels to 0 *)
  List.iter (fun d -> d.level <- 0) decls;

  (* Iteratively compute levels until fixed point *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun d ->
        (* Level = 1 + max level of dependencies *)
        let max_dep_level =
          StringSet.fold
            (fun dep_name acc ->
              match Hashtbl.find_opt by_name dep_name with
              | Some dep -> max acc dep.level
              | None -> acc (* Built-in or unknown, level 0 *))
            d.dependencies 0
        in
        let new_level =
          if StringSet.is_empty d.dependencies then 0 else max_dep_level + 1
        in
        if new_level > d.level then begin
          d.level <- new_level;
          changed := true
        end)
      decls
  done;

  (* Return max level *)
  List.fold_left (fun acc d -> max acc d.level) 0 decls

(** Find the earliest chapter where a proposition can be placed. Based on its
    dependencies - needs all deps visible in that body. *)
let earliest_chapter_for_prop decl_levels prop =
  let types, terms = symbols_in_expr prop in
  let all_deps = StringSet.union types terms in
  (* Filter out builtins *)
  let deps =
    StringSet.filter (fun n -> not (StringSet.mem n builtin_types)) all_deps
  in
  (* Find max level among dependencies *)
  let max_level =
    StringSet.fold
      (fun name acc ->
        match Hashtbl.find_opt decl_levels name with
        | Some level -> max acc level
        | None -> acc)
      deps 0
  in
  (* Body of chapter C can see declarations from chapters 0..C+1
     So if max dep is at level L, earliest body is L-1 (or 0) *)
  max 0 (max_level - 1)

(** Normalize a document to normal form based on dependency levels *)
let normalize (doc : document) : document =
  if doc.chapters = [] then doc
  else begin
    (* Step 1: Collect all declarations with their dependencies *)
    let all_decls =
      List.concat_map
        (fun chapter ->
          List.map
            (fun decl_loc ->
              let type_deps, term_deps = symbols_in_decl_deps decl_loc.value in
              (* Filter out builtins; include term deps for contexts *)
              let deps = StringSet.union type_deps term_deps in
              let deps =
                StringSet.filter
                  (fun n -> not (StringSet.mem n builtin_types))
                  deps
              in
              {
                name = decl_name decl_loc.value;
                is_type = is_type_decl decl_loc.value;
                is_void = is_void_proc decl_loc.value;
                decl = decl_loc;
                dependencies = deps;
                level = 0;
              })
            chapter.head)
        doc.chapters
    in

    (* Step 2: Compute topological levels *)
    let max_level = compute_levels all_decls in
    let num_chapters = max_level + 1 in

    (* Step 3: Identify void units *)
    let void_decls = List.filter (fun d -> d.is_void) all_decls in
    let non_void_decls = List.filter (fun d -> not d.is_void) all_decls in

    (* For void procedures, find their tied propositions *)
    let void_units =
      List.map
        (fun vd ->
          (* Find the original chapter this void proc came from *)
          let orig_chapter =
            List.find
              (fun ch ->
                List.exists (fun d -> decl_name d.value = vd.name) ch.head)
              doc.chapters
          in
          let params = void_proc_params orig_chapter in
          let tied =
            List.filter
              (fun p -> uses_primed p.value || prop_uses_params params p.value)
              orig_chapter.body
          in
          { void_proc = vd; tied_props = tied })
        void_decls
    in

    (* Independent props: not tied to any void proc *)
    let independent_props =
      List.concat_map
        (fun chapter ->
          let params = void_proc_params chapter in
          List.filter
            (fun p ->
              (not (uses_primed p.value))
              && not (prop_uses_params params p.value))
            chapter.body)
        doc.chapters
    in

    (* Step 5: Assign declarations to chapters by level *)
    let decl_assignments = Array.make num_chapters [] in
    List.iter
      (fun d -> decl_assignments.(d.level) <- d :: decl_assignments.(d.level))
      non_void_decls;

    (* Step 6: Place void units - they go at their level, but we need to handle
       conflicts (multiple void procs at same level need separate chapters) *)

    (* Sort void units by level, then spread out conflicts *)
    let sorted_void_units =
      List.sort
        (fun a b -> compare a.void_proc.level b.void_proc.level)
        void_units
    in

    (* Assign void units to chapters, creating new chapters if needed *)
    let void_chapter_assignments = Hashtbl.create 16 in
    let next_available = ref 0 in
    List.iter
      (fun vu ->
        let min_level = vu.void_proc.level in
        let target = max min_level !next_available in
        Hashtbl.add void_chapter_assignments vu.void_proc.name target;
        vu.void_proc.level <- target;
        (* Update the level *)
        next_available := target + 1)
      sorted_void_units;

    (* Recompute num_chapters if we added any *)
    let num_chapters = max num_chapters !next_available in

    (* Resize arrays if needed *)
    let decl_assignments =
      if num_chapters > Array.length decl_assignments then
        Array.init num_chapters (fun i ->
            if i < Array.length decl_assignments then decl_assignments.(i)
            else [])
      else decl_assignments
    in

    (* Add void procs to their assigned chapters *)
    List.iter
      (fun vu ->
        let level = vu.void_proc.level in
        decl_assignments.(level) <- vu.void_proc :: decl_assignments.(level))
      void_units;

    (* Build level lookup table (after void proc reassignment so levels are final) *)
    let decl_levels = Hashtbl.create 32 in
    List.iter (fun d -> Hashtbl.replace decl_levels d.name d.level) all_decls;

    (* Step 7: Assign propositions to chapters *)
    let body_assignments = Array.make num_chapters [] in

    (* Void-tied props go with their void proc *)
    List.iter
      (fun vu ->
        let level = vu.void_proc.level in
        body_assignments.(level) <- vu.tied_props @ body_assignments.(level))
      void_units;

    (* Independent props go to earliest valid chapter *)
    List.iter
      (fun prop ->
        let chapter = earliest_chapter_for_prop decl_levels prop.value in
        let target = min chapter (num_chapters - 1) in
        body_assignments.(target) <- prop :: body_assignments.(target))
      independent_props;

    (* Step 8: Build the normalized document *)
    let new_chapters =
      Array.to_list
        (Array.mapi
           (fun level _ ->
             (* Sort declarations: non-void first (by name for stability), then void *)
             let decls = decl_assignments.(level) in
             let non_void = List.filter (fun d -> not d.is_void) decls in
             let void = List.filter (fun d -> d.is_void) decls in
             let sorted_decls =
               List.sort (fun a b -> String.compare a.name b.name) non_void
               @ void
             in
             let head = List.map (fun d -> d.decl) sorted_decls in
             let body = List.rev body_assignments.(level) in
             { head; body })
           decl_assignments)
    in

    (* Filter out empty chapters (shouldn't happen but just in case) *)
    let non_empty = List.filter (fun ch -> ch.head <> []) new_chapters in

    { doc with chapters = non_empty }
  end
