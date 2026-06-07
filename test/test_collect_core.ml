(* @archlint.module test
   @archlint.domain pantagruel.collect *)

open Alcotest
open Pantagruel

let mk_located value =
  { Ast.loc = Ast.dummy_loc; value; doc = []; doc_adjacent = false }

let gen_type_expr =
  let module Gen = QCheck2.Gen in
  let base =
    Gen.oneof_list
      [
        Ast.TName (Ast.Upper "Bool");
        Ast.TName (Ast.Upper "Nat");
        Ast.TName (Ast.Upper "D");
        Ast.TName (Ast.Upper "Alias");
      ]
  in
  Gen.oneof
    [
      base;
      Gen.map (fun t -> Ast.TList t) base;
      Gen.map2 (fun a b -> Ast.TProduct [ a; b ]) base base;
      Gen.map2 (fun a b -> Ast.TSum [ a; b ]) base base;
    ]

let print_type_expr = Pretty.str_type_expr

let resolve_expected =
  let rec go = function[@warning "-4"]
    | Ast.TName (Ast.Upper "Bool") -> Ok Types.TyBool
    | Ast.TName (Ast.Upper "Nat") -> Ok Types.TyNat
    | Ast.TName (Ast.Upper "D") -> Ok (Types.TyDomain "D")
    | Ast.TName (Ast.Upper "Alias") -> Ok Types.TyBool
    | Ast.TList t -> Result.map (fun ty -> Types.TyList ty) (go t)
    | Ast.TProduct ts ->
        Result.map
          (fun tys -> Types.TyProduct tys)
          (Util.sequence_results (List.map go ts))
    | Ast.TSum ts ->
        Result.map
          (fun tys -> Types.TySum tys)
          (Util.sequence_results (List.map go ts))
    | Ast.TName _ | Ast.TQName _ -> Error ()
  in
  go

let gen_domain_names =
  let module Gen = QCheck2.Gen in
  Gen.map
    (fun flags ->
      List.filter_map
        (fun (name, keep) -> if keep then Some name else None)
        (List.combine [ "A"; "B"; "C"; "D" ] flags))
    (Gen.list_size (Gen.return 4) Gen.bool)

let print_names names = "[" ^ String.concat "; " names ^ "]"

let doc_with_domains names =
  {
    Ast.module_name = Some (Upper "M");
    imports = [];
    contexts = [];
    chapters =
      [
        {
          head =
            List.map
              (fun name -> mk_located (Ast.DeclDomain (Ast.Upper name)))
              names;
          body = [];
          checks = [];
          trailing_docs = [];
        };
      ];
  }

let split_form_doc body =
  let param =
    { Ast.param_name = Ast.Lower "x"; param_type = Ast.TName (Ast.Upper "Nat") }
  in
  {
    Ast.module_name = Some (Ast.Upper "M");
    imports = [];
    contexts = [];
    chapters =
      [
        {
          head =
            [
              mk_located
                (Ast.DeclRule
                   {
                     name = Ast.Lower "next";
                     params = [ param ];
                     guards = [];
                     return_type = Ast.TName (Ast.Upper "Nat");
                     contexts = [];
                   });
            ];
          body =
            [
              mk_located
                (Ast.EBinop
                   ( OpEq,
                     Ast.EApp
                       ( Ast.EVar (Ast.Lower "next"),
                         [ Ast.EVar (Ast.Lower "x") ] ),
                     body ));
            ];
          checks = [];
          trailing_docs = [];
        };
      ];
  }

let gen_split_body =
  QCheck2.Gen.oneof_list
    [
      Ast.EVar (Lower "x");
      Ast.ELitNat 0;
      Ast.EBinop (Ast.OpAdd, Ast.EVar (Ast.Lower "x"), Ast.ELitNat 1);
    ]

let test_resolve_type_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"resolve_type matches generated environment"
       ~count:300 ~print:print_type_expr gen_type_expr (fun type_expr ->
         let env =
           Env.empty ""
           |> Env.add_domain "D" Ast.dummy_loc ~chapter:0
           |> Env.add_alias "Alias" Types.TyBool Ast.dummy_loc ~chapter:0
         in
         match[@warning "-4"]
           ( Collect.resolve_type env type_expr Ast.dummy_loc,
             resolve_expected type_expr )
         with
         | Ok actual, Ok expected -> Types.equal_ty actual expected
         | Error _, Error () -> true
         | _ -> false))

let test_collect_all_domains_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"collect_all registers generated domains"
       ~count:100 ~print:print_names gen_domain_names (fun names ->
         match
           Collect.collect_all ~base_env:(Env.empty "M")
             (doc_with_domains names)
         with
         | Error _ -> false
         | Ok env ->
             List.for_all
               (fun name ->
                 match[@warning "-4"] Env.lookup_type name env with
                 | Some { Env.kind = Env.KDomain; _ } -> true
                 | _ -> false)
               names))

let test_recognize_split_form_bodies_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"recognize_split_form_bodies attaches rule bodies"
       ~count:100 ~print:Ast.show_expr gen_split_body (fun body ->
         let doc = split_form_doc body in
         match Collect.collect_all ~base_env:(Env.empty "M") doc with
         | Error _ -> false
         | Ok env -> (
             let env, doc = Collect.recognize_split_form_bodies env doc in
             match (Env.lookup_rule_body_arity "next" 1 env, doc.chapters) with
             | Some (_decl, attached_body, _recursive), [ chapter ] ->
                 Ast.equal_expr body attached_body && chapter.body = []
             | _ -> false)))

let () =
  run "Collect"
    [
      ( "property",
        [
          test_resolve_type_property;
          test_collect_all_domains_property;
          test_recognize_split_form_bodies_property;
        ] );
    ]
