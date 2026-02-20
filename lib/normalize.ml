(** Top-down normal form transformation for Pantagruel documents.

    A document is normalized top-down with respect to a chosen root term.
    Chapter 1 contains the root term and its minimal supporting declarations.
    Each subsequent chapter glosses terms referenced in the previous chapter's
    body but not yet declared. This is the "progressive disclosure" pattern —
    the reader sees the high-level story first.

    Visibility rules:
    - Declaration in chapter N is visible in heads of chapter M where M >= N
    - Declaration in chapter N is visible in bodies of chapter M where M >= N-1

    Algorithm: 1. Flatten all declarations and propositions 2. BFS from root
    term: level 0 = root + transitive declaration deps; each subsequent level
    glosses new symbols from the previous level's propositions 3. Unreachable
    declarations go to a final appendix chapter 4. Actions spread one per
    chapter 5. Propositions placed in earliest valid body *)

open Ast
module StringSet = Set.Make (String)

type decl_info = {
  name : string;
  is_type : bool;  (** true for domains/aliases, false for rules *)
  is_void : bool;  (** true for actions *)
  decl : declaration located;
  dependencies : StringSet.t;  (** Type names this declaration depends on *)
  mutable level : int;  (** Topological level, computed later *)
}
(** Information about a declaration *)

type action_unit = {
  action_decl : decl_info;
  tied_props : expr located list;
      (** Props using primed exprs or action params *)
}
(** An action unit: an action + its associated propositions *)

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
    | EInitially e -> go e
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
  let collect_params_guards params guards =
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
      guards
  in
  (match decl with
  | DeclDomain _ -> () (* No dependencies *)
  | DeclAlias (_, te) -> types := types_in_type_expr te
  | DeclRule { params; guards; return_type; _ } ->
      collect_params_guards params guards;
      types := StringSet.union !types (types_in_type_expr return_type)
  | DeclAction { params; guards; _ } -> collect_params_guards params guards);
  (!types, !terms)

(** Get the name defined by a declaration *)
let decl_name = function
  | DeclDomain name -> name
  | DeclAlias (name, _) -> name
  | DeclRule { name; _ } -> name
  | DeclAction { label; _ } -> label

(** Is this a type-namespace declaration? *)
let is_type_decl = function
  | DeclDomain _ | DeclAlias _ -> true
  | DeclRule _ | DeclAction _ -> false

(** Is this an action? *)
let is_action = function
  | DeclAction _ -> true
  | DeclDomain _ | DeclAlias _ | DeclRule _ -> false

(** Check if an expression uses any primed names *)
let rec uses_primed = function
  | EPrimed _ -> true
  | EVar _ | EDomain _ | EQualified _ -> false
  | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ -> false
  | EInitially e -> uses_primed e
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

(** Get action parameters from a chapter *)
let action_params (chapter : chapter) : StringSet.t =
  let params =
    List.find_map
      (fun decl ->
        match decl.value with
        | DeclAction { params; _ } ->
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

(** Compute transitive closure of declaration-level dependencies. Given a set of
    seed names and a lookup table of all declarations, return the full set of
    names reachable by following dependencies. *)
let transitive_decl_deps (by_name : (string, decl_info) Hashtbl.t)
    (seeds : StringSet.t) : StringSet.t =
  let result = ref seeds in
  let worklist = Queue.create () in
  StringSet.iter (fun n -> Queue.push n worklist) seeds;
  while not (Queue.is_empty worklist) do
    let name = Queue.pop worklist in
    match Hashtbl.find_opt by_name name with
    | Some d ->
        StringSet.iter
          (fun dep ->
            if not (StringSet.mem dep !result) then begin
              result := StringSet.add dep !result;
              Queue.push dep worklist
            end)
          d.dependencies
    | None -> ()
  done;
  !result

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

(** Normalize a document top-down with respect to a root term. Chapter 1
    contains the root term and its minimal supporting declarations. Each
    subsequent chapter glosses terms referenced in the previous chapter's body
    but not yet declared. *)
let normalize (doc : document) (root_term : string) : document =
  if doc.chapters = [] then doc
  else begin
    (* Step 1: Collect all declarations with their dependencies *)
    let all_decls =
      List.concat_map
        (fun chapter ->
          List.map
            (fun decl_loc ->
              let type_deps, term_deps = symbols_in_decl_deps decl_loc.value in
              let deps = StringSet.union type_deps term_deps in
              let deps =
                StringSet.filter
                  (fun n -> not (StringSet.mem n builtin_types))
                  deps
              in
              {
                name = decl_name decl_loc.value;
                is_type = is_type_decl decl_loc.value;
                is_void = is_action decl_loc.value;
                decl = decl_loc;
                dependencies = deps;
                level = -1 (* unassigned *);
              })
            chapter.head)
        doc.chapters
    in
    let by_name = Hashtbl.create 32 in
    List.iter (fun d -> Hashtbl.replace by_name d.name d) all_decls;

    (* Step 2: BFS from root term *)
    (* Level 0: root term + transitive declaration-level deps *)
    let level_0 =
      transitive_decl_deps by_name (StringSet.singleton root_term)
    in
    StringSet.iter
      (fun name ->
        match Hashtbl.find_opt by_name name with
        | Some d -> d.level <- 0
        | None -> ())
      level_0;
    let assigned = ref level_0 in

    (* Collect all propositions for scanning *)
    let all_props = List.concat_map (fun ch -> ch.body) doc.chapters in
    let scanned = Hashtbl.create 64 in

    (* BFS: for each level N, find props referencing level-N symbols,
       collect new symbols from those props, assign them to level N+1 *)
    let current_level = ref 0 in
    let continue_bfs = ref true in
    while !continue_bfs do
      (* Symbols at exactly the current level *)
      let level_symbols =
        StringSet.filter
          (fun name ->
            match Hashtbl.find_opt by_name name with
            | Some d -> d.level = !current_level
            | None -> false)
          !assigned
      in

      (* Find unscanned props that reference at least one current-level symbol *)
      let matching_props =
        List.filter
          (fun (p : expr located) ->
            if Hashtbl.mem scanned p.loc then false
            else
              let types, terms = symbols_in_expr p.value in
              let all_syms = StringSet.union types terms in
              not (StringSet.is_empty (StringSet.inter all_syms level_symbols)))
          all_props
      in

      (* Mark as scanned *)
      List.iter
        (fun (p : expr located) -> Hashtbl.replace scanned p.loc ())
        matching_props;

      (* Collect new symbols from these propositions *)
      let new_symbols = ref StringSet.empty in
      List.iter
        (fun (p : expr located) ->
          let types, terms = symbols_in_expr p.value in
          let all_syms = StringSet.union types terms in
          let filtered =
            StringSet.filter
              (fun n ->
                (not (StringSet.mem n builtin_types))
                && not (StringSet.mem n !assigned))
              all_syms
          in
          new_symbols := StringSet.union !new_symbols filtered)
        matching_props;

      if StringSet.is_empty !new_symbols then continue_bfs := false
      else begin
        (* Include transitive declaration-level deps for head well-formedness *)
        let expanded = transitive_decl_deps by_name !new_symbols in
        let truly_new = StringSet.diff expanded !assigned in
        if StringSet.is_empty truly_new then continue_bfs := false
        else begin
          let next_level = !current_level + 1 in
          StringSet.iter
            (fun name ->
              match Hashtbl.find_opt by_name name with
              | Some d -> d.level <- next_level
              | None -> ())
            truly_new;
          assigned := StringSet.union !assigned truly_new;
          current_level := next_level
        end
      end
    done;

    let max_level = !current_level in

    (* Step 3: Unreachable declarations go to appendix *)
    let unreachable =
      List.filter (fun d -> not (StringSet.mem d.name !assigned)) all_decls
    in
    let has_appendix = unreachable <> [] in
    let appendix_level = max_level + 1 in
    List.iter (fun d -> d.level <- appendix_level) unreachable;
    let num_chapters =
      if has_appendix then appendix_level + 1 else max_level + 1
    in

    (* Step 4: Identify action units *)
    let action_decls = List.filter (fun d -> d.is_void) all_decls in
    let non_action_decls = List.filter (fun d -> not d.is_void) all_decls in

    let action_units =
      List.map
        (fun ad ->
          let orig_chapter =
            List.find
              (fun ch ->
                List.exists (fun d -> d.Ast.loc = ad.decl.Ast.loc) ch.head)
              doc.chapters
          in
          let params = action_params orig_chapter in
          let tied =
            List.filter
              (fun p -> uses_primed p.value || prop_uses_params params p.value)
              orig_chapter.body
          in
          { action_decl = ad; tied_props = tied })
        action_decls
    in

    (* Independent props: not tied to any action *)
    let independent_props =
      List.concat_map
        (fun chapter ->
          let params = action_params chapter in
          List.filter
            (fun p ->
              (not (uses_primed p.value))
              && not (prop_uses_params params p.value))
            chapter.body)
        doc.chapters
    in

    (* Step 5: Assign non-action declarations to chapters by level *)
    let decl_assignments = Array.make num_chapters [] in
    List.iter
      (fun d -> decl_assignments.(d.level) <- d :: decl_assignments.(d.level))
      non_action_decls;

    (* Step 6: Place action units - spread conflicts across adjacent chapters *)
    let sorted_action_units =
      List.sort
        (fun a b -> compare a.action_decl.level b.action_decl.level)
        action_units
    in

    let action_chapter_assignments = Hashtbl.create 16 in
    let next_available = ref 0 in
    List.iter
      (fun au ->
        let min_level = au.action_decl.level in
        let target = max min_level !next_available in
        Hashtbl.add action_chapter_assignments au.action_decl.name target;
        au.action_decl.level <- target;
        next_available := target + 1)
      sorted_action_units;

    let num_chapters = max num_chapters !next_available in

    let decl_assignments =
      if num_chapters > Array.length decl_assignments then
        Array.init num_chapters (fun i ->
            if i < Array.length decl_assignments then decl_assignments.(i)
            else [])
      else decl_assignments
    in

    List.iter
      (fun au ->
        let level = au.action_decl.level in
        decl_assignments.(level) <- au.action_decl :: decl_assignments.(level))
      action_units;

    (* Build level lookup table (after action reassignment so levels are final) *)
    let decl_levels = Hashtbl.create 32 in
    List.iter (fun d -> Hashtbl.replace decl_levels d.name d.level) all_decls;

    (* Step 7: Assign propositions to chapters *)
    let body_assignments = Array.make num_chapters [] in

    (* Action-tied props go with their action *)
    List.iter
      (fun au ->
        let level = au.action_decl.level in
        body_assignments.(level) <- au.tied_props @ body_assignments.(level))
      action_units;

    (* Independent props go to earliest valid chapter *)
    List.iter
      (fun prop ->
        let chapter = earliest_chapter_for_prop decl_levels prop.value in
        let target = min chapter (num_chapters - 1) in
        body_assignments.(target) <- prop :: body_assignments.(target))
      independent_props;

    (* Collect trailing docs from all original chapters *)
    let all_trailing_docs =
      List.concat_map (fun ch -> ch.trailing_docs) doc.chapters
    in

    (* Step 8: Build the normalized document *)
    let new_chapters =
      Array.to_list
        (Array.mapi
           (fun level _ ->
             let decls = decl_assignments.(level) in
             let non_action = List.filter (fun d -> not d.is_void) decls in
             let action = List.filter (fun d -> d.is_void) decls in
             let sorted_decls =
               List.sort
                 (fun a b ->
                   compare
                     (a.decl.loc.line, a.decl.loc.col)
                     (b.decl.loc.line, b.decl.loc.col))
                 non_action
               @ action
             in
             let head = List.map (fun d -> d.decl) sorted_decls in
             let body = List.rev body_assignments.(level) in
             { head; body; trailing_docs = [] })
           decl_assignments)
    in

    let non_empty = List.filter (fun ch -> ch.head <> []) new_chapters in
    (* Attach trailing docs to the last chapter *)
    let non_empty =
      if all_trailing_docs <> [] then
        match List.rev non_empty with
        | [] -> non_empty
        | last :: rest ->
            List.rev ({ last with trailing_docs = all_trailing_docs } :: rest)
      else non_empty
    in
    { doc with chapters = non_empty }
  end
