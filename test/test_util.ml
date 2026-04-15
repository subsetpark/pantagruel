(** Shared test utilities *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let parse_expr str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.standalone_expr
    supplier

let parse_and_collect str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:
        (Env.empty (Option.fold ~none:"" ~some:Ast.upper_name doc.module_name))
      doc
  with
  | Error e -> failf "Collection error: %s" (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Error e -> failf "Type error: %s" (Check.show_type_error e)
      | Ok _warnings -> (env, doc))

(** QCheck generator for Types.ty, depth-limited *)
let[@warning "-44"] gen_ty_at_depth =
  let open QCheck.Gen in
  let base =
    oneof
      [
        return Types.TyBool;
        return Types.TyNat;
        return Types.TyNat0;
        return Types.TyInt;
        return Types.TyReal;
        return Types.TyString;
        return Types.TyNothing;
        return (Types.TyDomain "X");
        return (Types.TyDomain "Y");
      ]
  in
  fix (fun self n ->
      if n <= 0 then base
      else
        oneof_weighted
          [
            (5, base);
            (1, map (fun t -> Types.TyList t) (self (n - 1)));
            ( 1,
              map2
                (fun a b -> Types.TyProduct [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
            ( 1,
              map2
                (fun a b -> Types.TySum [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
          ])

let gen_ty = gen_ty_at_depth 2
let arb_ty = QCheck.make ~print:Types.format_ty gen_ty

(** QCheck generator for Ast.type_expr, depth-limited *)
let[@warning "-44"] gen_type_expr_at_depth =
  let open QCheck.Gen in
  let names = [ "Nat"; "Bool"; "Int"; "String"; "Real"; "Nothing" ] in
  let base = oneof_list (List.map (fun n -> Ast.TName (Ast.Upper n)) names) in
  fix (fun self n ->
      if n <= 0 then base
      else
        oneof_weighted
          [
            (4, base);
            (1, map (fun t -> Ast.TList t) (self (n - 1)));
            ( 1,
              map2
                (fun a b -> Ast.TProduct [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
            ( 1,
              map2 (fun a b -> Ast.TSum [ a; b ]) (self (n - 1)) (self (n - 1))
            );
          ])

let gen_type_expr = gen_type_expr_at_depth 2
let arb_type_expr = QCheck.make ~print:Pretty.str_type_expr gen_type_expr

(** ============================================================ AST document
    generators (Layer 2 of the SMT-translator tests).

    Strategy: build a small "world" of declared symbols (1–2 domains, a few
    rules with explicit parameters) first, then generate boolean body
    propositions that refer only to those declared symbols. The aim is to
    exercise the patterns that hit the known SMT translation bugs (multiple
    rules sharing parameter names, quantified bodies that may or may not
    reference rule parameters), while staying simple enough for
    [Check.check_document] to accept most of what we produce.
    ============================================================ *)

type gen_world = {
  domains : string list;  (** Declared domain names *)
  base_types : Ast.type_expr list;  (** Domain TName plus a few primitives *)
  rules : (string * Ast.param list * Ast.type_expr) list;
      (** name, formal params, return type *)
  vars : (string * Ast.type_expr) list;  (** In-scope variables *)
}
(** A minimal symbol world generated before any expression construction. *)

(** Generate a list of exactly [n] elements from [g]. (The deprecated
    [QCheck.Gen.list_repeat] does the same; this wrapper documents intent.) *)
let list_of_size n g = QCheck.Gen.list_size (QCheck.Gen.return n) g

(** Lower-case identifiers we cycle through. Some collisions are intentional —
    sharing names like [x]/[n] across rules is exactly what triggers the
    over-quantification bug we want Layer 2 to keep an eye on. *)
let lower_pool = [ "x"; "y"; "n"; "k"; "u"; "v"; "a"; "b" ]

let gen_lower : string QCheck.Gen.t =
  let open QCheck.Gen in
  oneof_list lower_pool

let gen_domain_name : string QCheck.Gen.t = QCheck.Gen.oneof_list [ "X"; "Y" ]

let mk_param name ty : Ast.param =
  { Ast.param_name = Ast.Lower name; param_type = ty }

let mk_located v : 'a Ast.located =
  { Ast.loc = Ast.dummy_loc; value = v; doc = []; doc_adjacent = false }

let gen_param_with world : Ast.param QCheck.Gen.t =
  let open QCheck.Gen in
  let* name = gen_lower in
  let* ty = oneof_list world.base_types in
  return (mk_param name ty)

(** Generate a small world: 1–2 domains, 0–4 rules, 0–2 vars. *)
let gen_world : gen_world QCheck.Gen.t =
  let open QCheck.Gen in
  let* n_domains = int_range 1 2 in
  let domains = List.filteri (fun i _ -> i < n_domains) [ "X"; "Y" ] in
  let base_types =
    List.map (fun d -> Ast.TName (Ast.Upper d)) domains
    @ [ Ast.TName (Ast.Upper "Bool"); Ast.TName (Ast.Upper "Nat") ]
  in
  let world0 = { domains; base_types; rules = []; vars = [] } in
  let rule_names = [ "f"; "g"; "h"; "p"; "q" ] in
  let* n_rules = int_range 0 (List.length rule_names) in
  let rule_names = List.filteri (fun i _ -> i < n_rules) rule_names in
  let* rules =
    list_of_size n_rules
      (let* n_params = int_range 0 2 in
       let* params = list_of_size n_params (gen_param_with world0) in
       let* ret = oneof_list base_types in
       return (params, ret))
  in
  let rules =
    List.map2 (fun name (params, ret) -> (name, params, ret)) rule_names rules
  in
  let* n_vars = int_range 0 2 in
  let* vars =
    list_of_size n_vars
      (let* name = gen_lower in
       let* ty = oneof_list base_types in
       return (name, ty))
  in
  return { world0 with rules; vars }

(** Boolean expression generator. Bias toward leaves; allow [forall]/[exists]
    that introduce fresh params (the patterns that exercise the
    over-quantification bug). *)
let[@warning "-44"] gen_bool_expr_at_depth =
  let open QCheck.Gen in
  let lit = oneof_list [ Ast.ELitBool true; Ast.ELitBool false ] in
  let var_of world =
    match world.vars with
    | [] -> lit
    | _ ->
        let* name, _ty = oneof_list world.vars in
        return (Ast.EVar (Ast.Lower name))
  in
  let app_of world =
    let bool_rules =
      List.filter
        (fun (_, _, ret) -> ret = Ast.TName (Ast.Upper "Bool"))
        world.rules
    in
    match bool_rules with
    | [] -> lit
    | _ ->
        let* name, params, _ret = oneof_list bool_rules in
        let* args =
          flatten_list
            (List.map
               (fun (p : Ast.param) ->
                 let v_opt =
                   List.find_opt (fun (_, ty) -> ty = p.param_type) world.vars
                 in
                 match v_opt with
                 | Some (vname, _) -> return (Ast.EVar (Ast.Lower vname))
                 | None -> (
                     match(* Fall back to a literal of the right shape — Bool/Nat
                        only, otherwise emit a free var (may fail check). *)
                          [@warning "-4"]
                       p.param_type
                     with
                     | Ast.TName (Ast.Upper "Bool") ->
                         return (Ast.ELitBool true)
                     | Ast.TName (Ast.Upper "Nat") -> return (Ast.ELitNat 0)
                     | _ -> return (Ast.EVar (Ast.Lower "x"))))
               params)
        in
        return (Ast.EApp (Ast.EVar (Ast.Lower name), args))
  in
  fix (fun self (depth, world) ->
      if depth <= 0 then oneof [ lit; var_of world; app_of world ]
      else
        oneof_weighted
          [
            (4, lit);
            (3, var_of world);
            (2, app_of world);
            ( 1,
              let* p = gen_param_with world in
              let* body =
                self
                  ( depth - 1,
                    {
                      world with
                      vars =
                        (Ast.lower_name p.param_name, p.param_type)
                        :: world.vars;
                    } )
              in
              return (Ast.EForall ([ p ], [], body)) );
            ( 1,
              let* p = gen_param_with world in
              let* body =
                self
                  ( depth - 1,
                    {
                      world with
                      vars =
                        (Ast.lower_name p.param_name, p.param_type)
                        :: world.vars;
                    } )
              in
              return (Ast.EExists ([ p ], [], body)) );
          ])

let gen_bool_expr world = gen_bool_expr_at_depth (2, world)

(** Build a chapter from a world: the rules become declarations in the head; the
    body is 1–3 generated boolean propositions. *)
let gen_chapter (world : gen_world) : Ast.chapter QCheck.Gen.t =
  let open QCheck.Gen in
  let head =
    List.map (fun d -> mk_located (Ast.DeclDomain (Ast.Upper d))) world.domains
    @ List.map
        (fun (name, params, ret) ->
          mk_located
            (Ast.DeclRule
               {
                 name = Ast.Lower name;
                 params;
                 guards = [];
                 return_type = ret;
                 contexts = [];
               }))
        world.rules
  in
  let* n_props = int_range 1 3 in
  let* props = list_of_size n_props (gen_bool_expr world) in
  let body = List.map mk_located props in
  return { Ast.head; body; checks = []; trailing_docs = [] }

(** Top-level document generator. *)
let gen_document : Ast.document QCheck.Gen.t =
  let open QCheck.Gen in
  let* world = gen_world in
  let* chapter = gen_chapter world in
  return
    {
      Ast.module_name = Some (Ast.Upper "GEN");
      imports = [];
      contexts = [];
      chapters = [ chapter ];
    }

(** Pretty-print a generated document for QCheck shrinking output. There is no
    top-level pretty-printer for full documents, so fall back to the derived
    [show_document]. *)
let print_document doc = Ast.show_document doc

let arb_document : Ast.document QCheck.arbitrary =
  QCheck.make ~print:print_document gen_document
