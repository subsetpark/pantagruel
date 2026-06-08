(* @archlint.module test
   @archlint.domain pantagruel.env *)

open Alcotest
open Pantagruel

let loc line = { Ast.file = "env-test.pant"; line; col = 1 }

let entry kind chapter =
  { Env.kind; loc = loc chapter; module_origin = None; decl_chapter = chapter }

let param name ty : Ast.param = { param_name = Ast.Lower name; param_type = ty }

let gen_prefix =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 8)

let upper_of prefix suffix = String.capitalize_ascii prefix ^ suffix
let name prefix suffix = prefix ^ suffix

let sample_decl prefix =
  Ast.DeclRule
    {
      name = Ast.Lower (name prefix "next");
      params =
        [
          param (name prefix "x")
            (Ast.TName (Ast.Upper (upper_of prefix "User")));
        ];
      guards = [];
      return_type = Ast.TName (Ast.Upper (upper_of prefix "User"));
      contexts = [];
    }

let sample_body prefix =
  Ast.EApp
    ( Ast.EVar (Ast.Lower (name prefix "next")),
      [ Ast.EVar (Ast.Lower (name prefix "x")) ] )

let base_env prefix =
  let domain = upper_of prefix "User" in
  Env.empty (upper_of prefix "EnvTest")
  |> Env.add_domain domain (loc 1) ~chapter:0
  |> Env.add_alias (upper_of prefix "UserId") Types.TyNat (loc 2) ~chapter:0
  |> Env.add_rule (name prefix "active")
       (Types.TyFunc ([ Types.TyDomain domain ], Some Types.TyBool))
       (loc 3) ~chapter:0
  |> Env.add_rule (name prefix "flag")
       (Types.TyFunc ([], Some Types.TyBool))
       (loc 4) ~chapter:0
  |> Env.add_closure (name prefix "reachable")
       (Types.TyFunc ([ Types.TyDomain domain ], Some Types.TyBool))
       (name prefix "active") (loc 5) ~chapter:0
  |> Env.add_rule_guards (name prefix "active")
       [ param (name prefix "u") (Ast.TName (Ast.Upper domain)) ]
       [ Ast.GExpr (Ast.EVar (Ast.Lower (name prefix "flag"))) ]
  |> Env.add_context (upper_of prefix "Account") [ name prefix "active" ]
  |> Env.add_rule_to_context
       (upper_of prefix "Account")
       (name prefix "reachable")
  |> Env.attach_rule_body (name prefix "active") 1 (sample_decl prefix)
       (sample_body prefix)

let imported_env prefix module_name =
  let widget = upper_of prefix "Widget" in
  Env.empty module_name
  |> Env.add_domain widget (loc 10) ~chapter:0
  |> Env.add_rule (name prefix "make")
       (Types.TyFunc ([], Some (Types.TyDomain widget)))
       (loc 11) ~chapter:0
  |> Env.add_rule (name prefix "over")
       (Types.TyFunc ([ Types.TyNat ], Some Types.TyNat))
       (loc 12) ~chapter:0

let colliding_import_env prefix module_name =
  let widget = upper_of prefix "Widget" in
  Env.empty module_name
  |> Env.add_domain widget (loc 20) ~chapter:0
  |> Env.add_rule (name prefix "make")
       (Types.TyFunc ([], Some (Types.TyDomain widget)))
       (loc 21) ~chapter:0

let core_construction_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"construction and lookup APIs agree" ~count:100
         gen_prefix (fun prefix ->
           let env =
             base_env prefix
             |> Env.add_type_entry (upper_of prefix "Extra")
                  (entry Env.KDomain 0)
             |> Env.add_term_entry (name prefix "raw")
                  (entry (Env.KRule (Types.TyFunc ([], Some Types.TyNat))) 0)
             |> Env.add_var (name prefix "local") Types.TyNat
             |> Env.with_vars [ (name prefix "scoped", Types.TyBool) ]
           in
           let term_count = Env.fold_terms (fun _ _ n -> n + 1) env 0 in
           Env.iter_terms (fun _ _ -> ()) env;
           let all_count = Env.fold_all_terms (fun _ _ n -> n + 1) env 0 in
           let type_count = Env.fold_types (fun _ _ n -> n + 1) env 0 in
           Env.iter_types (fun _ _ -> ()) env;
           Option.is_some (Env.lookup_type (upper_of prefix "User") env)
           && Option.is_some (Env.lookup_type (upper_of prefix "Extra") env)
           && Option.is_some (Env.lookup_term (name prefix "flag") env)
           && Option.is_some
                (Env.lookup_term_arity (name prefix "active") 1 env)
           && Env.overloads_of (name prefix "active") env <> []
           && (not (Env.name_is_overloaded (name prefix "active") env))
           && Option.is_some (Env.lookup_var (name prefix "local") env)
           && Env.is_local_var (name prefix "local") env
           && Option.is_some (Env.lookup_bare (name prefix "local") env)
           && Option.is_some (Env.lookup_bare (name prefix "flag") env)
           && Env.bindings_terms env <> []
           && Env.bindings_types env <> []
           && term_count > 0 && all_count >= term_count && type_count > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"action, context, and rule-body APIs agree"
         ~count:100 gen_prefix (fun prefix ->
           let env =
             base_env prefix
             |> Env.with_action (upper_of prefix "Activate")
             |> Env.with_action_contexts [ upper_of prefix "Account" ]
           in
           let body_count = Env.fold_rule_bodies (fun _ _ _ n -> n + 1) env 0 in
           Env.in_action_context env
           && Env.action_contexts env = [ upper_of prefix "Account" ]
           && Env.current_module env = upper_of prefix "EnvTest"
           && Env.lookup_context (upper_of prefix "Account") env
              = Some [ name prefix "active"; name prefix "reachable" ]
           && Option.is_some (Env.lookup_rule_guards (name prefix "active") env)
           && Option.is_some
                (Env.lookup_rule_guards_arity (name prefix "active") 1 env)
           && Option.is_some
                (Env.lookup_rule_body_arity (name prefix "active") 1 env)
           && body_count = 1
           && (let cleared = Env.clear_action env in
               not (Env.in_action_context cleared))
           &&
           let reset = Env.with_module_init (upper_of prefix "Reset") env in
           Env.current_module reset = upper_of prefix "Reset"
           && not (Env.is_local_var (name prefix "local") reset)));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"nullary shadow detection is pure" ~count:100
         gen_prefix (fun prefix ->
           let env = base_env prefix in
           Env.nullary_shadow (name prefix "flag") env
           = Some (Types.TyBool, loc 4)
           && Env.nullary_shadow (name prefix "active") env = None));
  ]

let import_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"imports expose unambiguous qualified APIs"
         ~count:100 gen_prefix (fun prefix ->
           let imported_module = upper_of prefix "Imported" in
           let env =
             Env.add_import (base_env prefix)
               (imported_env prefix imported_module)
               imported_module
           in
           let imported_terms =
             Env.fold_imported_terms
               (fun origin name arity _ acc -> (origin, name, arity) :: acc)
               env []
           in
           Option.is_some (Env.lookup_type (upper_of prefix "Widget") env)
           && Option.is_some (Env.lookup_term (name prefix "make") env)
           && Option.is_some
                (Env.lookup_qualified_type imported_module
                   (upper_of prefix "Widget") env)
           && Option.is_some
                (Env.lookup_qualified_term imported_module (name prefix "make")
                   env)
           && Option.is_some
                (Env.lookup_qualified_term_arity imported_module
                   (name prefix "over") 1 env)
           && Option.is_none
                (Env.lookup_qualified_rule_guards_arity imported_module
                   (name prefix "active") 1 env)
           && Env.ambiguous_type_modules (upper_of prefix "Widget") env = None
           && Env.ambiguous_term_modules (name prefix "make") env = None
           && List.exists
                (fun (origin, term_name, arity) ->
                  origin = imported_module
                  && term_name = name prefix "over"
                  && arity = 1)
                imported_terms
           && Env.exports env <> ([], [])));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"ambiguous imports stay qualified" ~count:100
         gen_prefix (fun prefix ->
           let module_a = upper_of prefix "A" in
           let module_b = upper_of prefix "B" in
           let env =
             Env.add_import
               (Env.add_import
                  (Env.empty (upper_of prefix "Root"))
                  (colliding_import_env prefix module_a)
                  module_a)
               (colliding_import_env prefix module_b)
               module_b
           in
           Option.is_none (Env.lookup_type (upper_of prefix "Widget") env)
           && Option.is_none (Env.lookup_term (name prefix "make") env)
           && Option.is_some
                (Env.lookup_qualified_type module_a (upper_of prefix "Widget")
                   env)
           && Option.is_some
                (Env.lookup_qualified_term module_b (name prefix "make") env)
           && Env.ambiguous_type_modules (upper_of prefix "Widget") env
              = Some [ module_b; module_a ]
           && Env.ambiguous_term_modules (name prefix "make") env
              = Some [ module_a; module_b ]));
  ]

let visibility_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"chapter visibility filters declarations"
         ~count:100 gen_prefix (fun prefix ->
           let env =
             Env.empty (upper_of prefix "Visibility")
             |> Env.add_domain (upper_of prefix "Now") (loc 1) ~chapter:0
             |> Env.add_domain (upper_of prefix "Later") (loc 2) ~chapter:2
             |> Env.add_rule (name prefix "soon")
                  (Types.TyFunc ([], Some Types.TyBool))
                  (loc 3) ~chapter:1
           in
           let head0 = Env.visible_in_head 0 env in
           let body0 = Env.visible_in_body 0 env in
           Option.is_some (Env.lookup_type (upper_of prefix "Now") head0)
           && Option.is_none (Env.lookup_type (upper_of prefix "Later") head0)
           && Option.is_some (Env.lookup_term (name prefix "soon") body0)
           && Option.is_none (Env.lookup_type (upper_of prefix "Later") body0)));
  ]

let () =
  run "Env"
    [
      ("core_construction", core_construction_properties);
      ("imports", import_properties);
      ("visibility", visibility_properties);
    ]
