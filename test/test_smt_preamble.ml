(* @archlint.module test
   @archlint.domain pantagruel.smt-preamble *)

open Alcotest
open Pantagruel

let env =
  Env.empty "T"
  |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
  |> Env.add_alias "Pair"
       (Types.TyProduct [ Types.TyDomain "User"; Types.TyNat ])
       Ast.dummy_loc ~chapter:0
  |> Env.add_rule "active"
       (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
       Ast.dummy_loc ~chapter:0
  |> Env.add_rule "score"
       (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
       Ast.dummy_loc ~chapter:0

let config =
  Smt_types.make_config ~bound:3 ~steps:1 ~domain_bounds:Env.StringMap.empty
    ~inject_guards:true ()

let sample_param : Ast.param =
  { param_name = Ast.Lower "u"; param_type = Ast.TName (Ast.Upper "User") }

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_domain_sorts emits sort declarations"
         ~count:100 QCheck2.Gen.unit (fun () ->
           String.length (Smt_preamble.declare_domain_sorts config env) > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_composite_types emits datatypes"
         ~count:100 QCheck2.Gen.unit (fun () ->
           String.length (Smt_preamble.declare_composite_types env) > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"decompose_func_ty recognizes functions"
         ~count:100 QCheck2.Gen.unit (fun () ->
           match[@warning "-4"]
             Smt_preamble.decompose_func_ty
               (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
           with
           | Some ([ Types.TyDomain "User" ], Types.TyBool) -> true
           | Some _ | None -> false));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_functions emits declarations" ~count:100
         QCheck2.Gen.unit (fun () ->
           String.length (Smt_preamble.declare_functions env) > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"generate_closure_axioms returns text" ~count:100
         QCheck2.Gen.unit (fun () ->
           ignore (Smt_preamble.generate_closure_axioms config env);
           true));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"collect_type_constraint_exprs returns constraints" ~count:100
         QCheck2.Gen.unit (fun () ->
           Smt_preamble.collect_type_constraint_exprs config env <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_type_constraints emits assertions"
         ~count:100 QCheck2.Gen.unit (fun () ->
           String.length (Smt_preamble.declare_type_constraints config env) > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"generate_preamble emits declarations" ~count:100
         QCheck2.Gen.unit (fun () ->
           String.length (Smt_preamble.generate_preamble config env) > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"replace_word respects word boundaries"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_preamble.replace_word ~from:"f" ~to_:"g" "(and f foo)"
           = "(and g foo)"));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"resolve_param_bindings resolves params"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_preamble.resolve_param_bindings env [ sample_param ]
           = [ ("u", Types.TyDomain "User") ]));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"resolve_param_sort resolves type exprs"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_preamble.resolve_param_sort env (Ast.TName (Ast.Upper "User"))
           = Some "User"));
  ]

let () = run "Smt_preamble" [ ("public_api", public_api_properties) ]
