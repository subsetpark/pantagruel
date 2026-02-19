(** JSON serialization for Pantagruel documents *)

open Ast

(** Convert internal type to JSON *)
let rec ty_to_json = function
  | Types.TyBool -> `String "Bool"
  | Types.TyNat -> `String "Nat"
  | Types.TyNat0 -> `String "Nat0"
  | Types.TyInt -> `String "Int"
  | Types.TyReal -> `String "Real"
  | Types.TyString -> `String "String"
  | Types.TyNothing -> `String "Nothing"
  | Types.TyDomain name -> `String name
  | Types.TyList t -> `Assoc [ ("list", ty_to_json t) ]
  | Types.TyProduct ts -> `Assoc [ ("product", `List (List.map ty_to_json ts)) ]
  | Types.TySum ts -> `Assoc [ ("sum", `List (List.map ty_to_json ts)) ]
  | Types.TyFunc (params, ret) ->
      `Assoc
        [
          ( "func",
            `Assoc
              [
                ("params", `List (List.map ty_to_json params));
                ( "return",
                  match ret with None -> `Null | Some t -> ty_to_json t );
              ] );
        ]

(** Convert syntactic type expression to JSON *)
let rec type_expr_to_json = function
  | TName name -> `String name
  | TQName (m, name) ->
      `Assoc
        [
          ("qualified", `Assoc [ ("module", `String m); ("name", `String name) ]);
        ]
  | TList t -> `Assoc [ ("list", type_expr_to_json t) ]
  | TProduct ts -> `Assoc [ ("product", `List (List.map type_expr_to_json ts)) ]
  | TSum ts -> `Assoc [ ("sum", `List (List.map type_expr_to_json ts)) ]

(** Convert binary operator to string *)
let binop_to_string = function
  | OpAnd -> "and"
  | OpOr -> "or"
  | OpImpl -> "impl"
  | OpIff -> "iff"
  | OpEq -> "eq"
  | OpNeq -> "neq"
  | OpLt -> "lt"
  | OpGt -> "gt"
  | OpLe -> "le"
  | OpGe -> "ge"
  | OpIn -> "in"
  | OpSubset -> "subset"
  | OpAdd -> "add"
  | OpSub -> "sub"
  | OpMul -> "mul"
  | OpDiv -> "div"

(** Convert unary operator to string *)
let unop_to_string = function
  | OpNot -> "not"
  | OpNeg -> "neg"
  | OpCard -> "card"

(** Convert parameter to JSON *)
let param_to_json p =
  `Assoc
    [ ("name", `String p.param_name); ("type", type_expr_to_json p.param_type) ]

(** Convert expression to JSON *)
let rec expr_to_json = function
  | EVar name -> `Assoc [ ("var", `String name) ]
  | EDomain name -> `Assoc [ ("domain", `String name) ]
  | EQualified (m, name) ->
      `Assoc
        [
          ("qualified", `Assoc [ ("module", `String m); ("name", `String name) ]);
        ]
  | ELitNat n -> `Assoc [ ("nat", `Int n) ]
  | ELitReal r -> `Assoc [ ("real", `Float r) ]
  | ELitString s -> `Assoc [ ("string", `String s) ]
  | ELitBool b -> `Assoc [ ("bool", `Bool b) ]
  | EApp (f, args) ->
      `Assoc
        [
          ( "app",
            `Assoc
              [
                ("func", expr_to_json f);
                ("args", `List (List.map expr_to_json args));
              ] );
        ]
  | EPrimed name -> `Assoc [ ("primed", `String name) ]
  | EOverride (name, mappings) ->
      `Assoc
        [
          ( "override",
            `Assoc
              [
                ("func", `String name);
                ( "mappings",
                  `List
                    (List.map
                       (fun (k, v) ->
                         `Assoc
                           [
                             ("key", expr_to_json k); ("value", expr_to_json v);
                           ])
                       mappings) );
              ] );
        ]
  | ETuple es -> `Assoc [ ("tuple", `List (List.map expr_to_json es)) ]
  | EProj (e, n) ->
      `Assoc
        [ ("proj", `Assoc [ ("expr", expr_to_json e); ("index", `Int n) ]) ]
  | EBinop (op, left, right) ->
      `Assoc
        [
          ( "binop",
            `Assoc
              [
                ("op", `String (binop_to_string op));
                ("left", expr_to_json left);
                ("right", expr_to_json right);
              ] );
        ]
  | EUnop (op, e) ->
      `Assoc
        [
          ( "unop",
            `Assoc
              [ ("op", `String (unop_to_string op)); ("arg", expr_to_json e) ]
          );
        ]
  | EForall (params, guards, body) ->
      `Assoc
        [
          ( "forall",
            `Assoc
              [
                ("params", `List (List.map param_to_json params));
                ("guards", `List (List.map guard_to_json guards));
                ("body", expr_to_json body);
              ] );
        ]
  | EExists (params, guards, body) ->
      `Assoc
        [
          ( "exists",
            `Assoc
              [
                ("params", `List (List.map param_to_json params));
                ("guards", `List (List.map guard_to_json guards));
                ("body", expr_to_json body);
              ] );
        ]

(** Convert guard to JSON *)
and guard_to_json = function
  | GParam p -> `Assoc [ ("param", param_to_json p) ]
  | GIn (name, e) ->
      `Assoc
        [ ("in", `Assoc [ ("name", `String name); ("list", expr_to_json e) ]) ]
  | GExpr e -> `Assoc [ ("expr", expr_to_json e) ]

(** Convert doc comments to JSON *)
let doc_to_json docs =
  if docs = [] then []
  else [ ("doc", `List (List.map (fun s -> `String s) docs)) ]

(** Convert declaration to JSON, enriched with type info from environment *)
let decl_to_json env (decl_loc : declaration located) =
  let doc_json = doc_to_json decl_loc.doc in
  match decl_loc.value with
  | DeclDomain name ->
      `Assoc (doc_json @ [ ("kind", `String "domain"); ("name", `String name) ])
  | DeclAlias (name, te) ->
      (* Look up resolved type from environment *)
      let resolved =
        match Env.lookup_type name env with
        | Some { kind = Env.KAlias ty; _ } -> Some (ty_to_json ty)
        | _ -> None
      in
      `Assoc
        (doc_json
        @ [
            ("kind", `String "alias");
            ("name", `String name);
            ("type", type_expr_to_json te);
          ]
        @ match resolved with Some t -> [ ("resolved", t) ] | None -> [])
  | DeclRule { name; params; guards; return_type; contexts } ->
      (* Look up resolved type from environment *)
      let resolved =
        match Env.lookup_term name env with
        | Some { kind = Env.KRule ty; _ } -> Some (ty_to_json ty)
        | _ -> None
      in
      `Assoc
        (doc_json
        @ [
            ("kind", `String "rule");
            ("name", `String name);
            ("params", `List (List.map param_to_json params));
            ("guards", `List (List.map guard_to_json guards));
            ("return", type_expr_to_json return_type);
          ]
        @ (if contexts <> [] then
             [ ("contexts", `List (List.map (fun c -> `String c) contexts)) ]
           else [])
        @ match resolved with Some t -> [ ("resolved", t) ] | None -> [])
  | DeclAction { label; params; guards; context } ->
      let param_tys = List.map (fun p -> type_expr_to_json p.param_type) params in
      `Assoc
        (doc_json
        @ [
            ("kind", `String "action");
            ("label", `String label);
            ("params", `List (List.map param_to_json params));
            ("guards", `List (List.map guard_to_json guards));
          ]
        @ (match context with
          | Some c -> [ ("context", `String c) ]
          | None -> [])
        @ [ ("resolved",
              `Assoc [
                ("func",
                  `Assoc [
                    ("params", `List param_tys);
                    ("return", `Null);
                  ]);
              ]) ])

(** Convert proposition to JSON with doc comments *)
let prop_to_json (prop_loc : expr located) =
  let doc_json = doc_to_json prop_loc.doc in
  let expr_json = expr_to_json prop_loc.value in
  if doc_json = [] then expr_json
  else `Assoc (doc_json @ [ ("expr", expr_json) ])

(** Convert chapter to JSON *)
let chapter_to_json env chapter =
  `Assoc
    [
      ("head", `List (List.map (decl_to_json env) chapter.head));
      ("body", `List (List.map prop_to_json chapter.body));
    ]

(** Extract type definitions from environment *)
let types_to_json env =
  let bindings = Env.StringMap.bindings env.Env.types in
  let items =
    List.filter_map
      (fun (name, entry) ->
        (* Skip builtins *)
        if
          List.mem name
            [ "Bool"; "Nat"; "Nat0"; "Int"; "Real"; "String"; "Nothing" ]
        then None
        else
          let kind_json =
            match entry.Env.kind with
            | Env.KDomain -> `Assoc [ ("kind", `String "domain") ]
            | Env.KAlias ty ->
                `Assoc [ ("kind", `String "alias"); ("type", ty_to_json ty) ]
            | _ -> `Null
          in
          if kind_json = `Null then None else Some (name, kind_json))
      bindings
  in
  `Assoc items

(** Extract rule definitions from environment *)
let rules_to_json env =
  let bindings = Env.StringMap.bindings env.Env.terms in
  let items =
    List.filter_map
      (fun (name, entry) ->
        match entry.Env.kind with
        | Env.KRule ty -> (
            match ty with
            | Types.TyFunc (params, ret) ->
                Some
                  ( name,
                    `Assoc
                      [
                        ("params", `List (List.map ty_to_json params));
                        ( "return",
                          match ret with
                          | None -> `Null
                          | Some t -> ty_to_json t );
                      ] )
            | _ -> None)
        | _ -> None)
      bindings
  in
  `Assoc items

(** Convert entire document to JSON *)
let document_to_json env doc =
  `Assoc
    [
      ( "module",
        match doc.module_name with Some n -> `String n | None -> `Null );
      ("imports", `List (List.map (fun i -> `String i.value) doc.imports));
      ("contexts", `List (List.map (fun c -> `String c.value) doc.contexts));
      ("types", types_to_json env);
      ("rules", rules_to_json env);
      ("chapters", `List (List.map (chapter_to_json env) doc.chapters));
    ]

(** Output JSON to stdout *)
let output_json env doc =
  let json = document_to_json env doc in
  print_endline (Yojson.Basic.pretty_to_string json)
