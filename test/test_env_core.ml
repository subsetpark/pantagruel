(* @archlint.module test
   @archlint.domain pantagruel.env *)

open Alcotest
open Pantagruel

let loc line = { Ast.file = "env-test.pant"; line; col = 1 }

let entry kind chapter =
  { Env.kind; loc = loc chapter; module_origin = None; decl_chapter = chapter }

let param name ty : Ast.param = { param_name = Ast.Lower name; param_type = ty }

let sample_decl =
  Ast.DeclRule
    {
      name = Ast.Lower "next";
      params = [ param "x" (Ast.TName (Ast.Upper "User")) ];
      guards = [];
      return_type = Ast.TName (Ast.Upper "User");
      contexts = [];
    }

let sample_body =
  Ast.EApp (Ast.EVar (Ast.Lower "next"), [ Ast.EVar (Ast.Lower "x") ])

let base_env () =
  Env.empty "EnvTest"
  |> Env.add_domain "User" (loc 1) ~chapter:0
  |> Env.add_alias "UserId" Types.TyNat (loc 2) ~chapter:0
  |> Env.add_rule "active"
       (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
       (loc 3) ~chapter:0
  |> Env.add_rule "flag"
       (Types.TyFunc ([], Some Types.TyBool))
       (loc 4) ~chapter:0
  |> Env.add_closure "reachable"
       (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
       "active" (loc 5) ~chapter:0
  |> Env.add_rule_guards "active"
       [ param "u" (Ast.TName (Ast.Upper "User")) ]
       [ Ast.GExpr (Ast.EVar (Ast.Lower "flag")) ]
  |> Env.add_context "Account" [ "active" ]
  |> Env.add_rule_to_context "Account" "reachable"
  |> Env.attach_rule_body "active" 1 sample_decl sample_body

let imported_env () =
  Env.empty "Imported"
  |> Env.add_domain "Widget" (loc 10) ~chapter:0
  |> Env.add_rule "make"
       (Types.TyFunc ([], Some (Types.TyDomain "Widget")))
       (loc 11) ~chapter:0
  |> Env.add_rule "over"
       (Types.TyFunc ([ Types.TyNat ], Some Types.TyNat))
       (loc 12) ~chapter:0

let colliding_import_env module_name =
  Env.empty module_name
  |> Env.add_domain "Widget" (loc 20) ~chapter:0
  |> Env.add_rule "make"
       (Types.TyFunc ([], Some (Types.TyDomain "Widget")))
       (loc 21) ~chapter:0

let core_construction_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"construction and lookup APIs agree" ~count:100
         QCheck2.Gen.unit (fun () ->
           let env =
             base_env ()
             |> Env.add_type_entry "Extra" (entry Env.KDomain 0)
             |> Env.add_term_entry "raw"
                  (entry (Env.KRule (Types.TyFunc ([], Some Types.TyNat))) 0)
             |> Env.add_var "local" Types.TyNat
             |> Env.with_vars [ ("scoped", Types.TyBool) ]
           in
           let term_count = Env.fold_terms (fun _ _ n -> n + 1) env 0 in
           Env.iter_terms (fun _ _ -> ()) env;
           let all_count = Env.fold_all_terms (fun _ _ n -> n + 1) env 0 in
           let type_count = Env.fold_types (fun _ _ n -> n + 1) env 0 in
           Env.iter_types (fun _ _ -> ()) env;
           Option.is_some (Env.lookup_type "User" env)
           && Option.is_some (Env.lookup_type "Extra" env)
           && Option.is_some (Env.lookup_term "flag" env)
           && Option.is_some (Env.lookup_term_arity "active" 1 env)
           && Env.overloads_of "active" env <> []
           && (not (Env.name_is_overloaded "active" env))
           && Option.is_some (Env.lookup_var "local" env)
           && Env.is_local_var "local" env
           && Option.is_some (Env.lookup_bare "local" env)
           && Option.is_some (Env.lookup_bare "flag" env)
           && Env.bindings_terms env <> []
           && Env.bindings_types env <> []
           && term_count > 0 && all_count >= term_count && type_count > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"action, context, and rule-body APIs agree"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let env =
             base_env () |> Env.with_action "Activate"
             |> Env.with_action_contexts [ "Account" ]
           in
           let body_count = Env.fold_rule_bodies (fun _ _ _ n -> n + 1) env 0 in
           Env.in_action_context env
           && Env.action_contexts env = [ "Account" ]
           && Env.current_module env = "EnvTest"
           && Env.lookup_context "Account" env = Some [ "active"; "reachable" ]
           && Option.is_some (Env.lookup_rule_guards "active" env)
           && Option.is_some (Env.lookup_rule_guards_arity "active" 1 env)
           && Option.is_some (Env.lookup_rule_body_arity "active" 1 env)
           && body_count = 1
           && (let cleared = Env.clear_action env in
               not (Env.in_action_context cleared))
           &&
           let reset = Env.with_module_init "Reset" env in
           Env.current_module reset = "Reset"
           && not (Env.is_local_var "local" reset)));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"nullary shadow detection is pure" ~count:100
         QCheck2.Gen.unit (fun () ->
           let env = base_env () in
           Env.nullary_shadow "flag" env = Some (Types.TyBool, loc 4)
           && Env.nullary_shadow "active" env = None));
  ]

let import_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"imports expose unambiguous qualified APIs"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let env =
             Env.add_import (base_env ()) (imported_env ()) "Imported"
           in
           let imported_terms =
             Env.fold_imported_terms
               (fun origin name arity _ acc -> (origin, name, arity) :: acc)
               env []
           in
           Option.is_some (Env.lookup_type "Widget" env)
           && Option.is_some (Env.lookup_term "make" env)
           && Option.is_some (Env.lookup_qualified_type "Imported" "Widget" env)
           && Option.is_some (Env.lookup_qualified_term "Imported" "make" env)
           && Option.is_some
                (Env.lookup_qualified_term_arity "Imported" "over" 1 env)
           && Option.is_none
                (Env.lookup_qualified_rule_guards_arity "Imported" "active" 1
                   env)
           && Env.ambiguous_type_modules "Widget" env = None
           && Env.ambiguous_term_modules "make" env = None
           && List.exists
                (fun (origin, name, arity) ->
                  origin = "Imported" && name = "over" && arity = 1)
                imported_terms
           && Env.exports env <> ([], [])));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"ambiguous imports stay qualified" ~count:100
         QCheck2.Gen.unit (fun () ->
           let env =
             Env.add_import
               (Env.add_import (Env.empty "Root") (colliding_import_env "A") "A")
               (colliding_import_env "B") "B"
           in
           Option.is_none (Env.lookup_type "Widget" env)
           && Option.is_none (Env.lookup_term "make" env)
           && Option.is_some (Env.lookup_qualified_type "A" "Widget" env)
           && Option.is_some (Env.lookup_qualified_term "B" "make" env)
           && Env.ambiguous_type_modules "Widget" env = Some [ "B"; "A" ]
           && Env.ambiguous_term_modules "make" env = Some [ "A"; "B" ]));
  ]

let visibility_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"chapter visibility filters declarations"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let env =
             Env.empty "Visibility"
             |> Env.add_domain "Now" (loc 1) ~chapter:0
             |> Env.add_domain "Later" (loc 2) ~chapter:2
             |> Env.add_rule "soon"
                  (Types.TyFunc ([], Some Types.TyBool))
                  (loc 3) ~chapter:1
           in
           let head0 = Env.visible_in_head 0 env in
           let body0 = Env.visible_in_body 0 env in
           Option.is_some (Env.lookup_type "Now" head0)
           && Option.is_none (Env.lookup_type "Later" head0)
           && Option.is_some (Env.lookup_term "soon" body0)
           && Option.is_none (Env.lookup_type "Later" body0)));
  ]

let () =
  run "Env"
    [
      ("core_construction", core_construction_properties);
      ("imports", import_properties);
      ("visibility", visibility_properties);
    ]
