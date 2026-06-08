(* @archlint.module test
   @archlint.domain pantagruel.smt-preamble *)

open Alcotest
open Pantagruel

let gen_prefix =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 8)

let upper prefix suffix = String.capitalize_ascii prefix ^ suffix
let lower prefix suffix = prefix ^ suffix

let env prefix =
  let domain = upper prefix "User" in
  Env.empty (upper prefix "T")
  |> Env.add_domain domain Ast.dummy_loc ~chapter:0
  |> Env.add_alias (upper prefix "Pair")
       (Types.TyProduct [ Types.TyDomain domain; Types.TyNat ])
       Ast.dummy_loc ~chapter:0
  |> Env.add_rule (lower prefix "active")
       (Types.TyFunc ([ Types.TyDomain domain ], Some Types.TyBool))
       Ast.dummy_loc ~chapter:0
  |> Env.add_rule (lower prefix "score")
       (Types.TyFunc ([ Types.TyDomain domain ], Some Types.TyNat))
       Ast.dummy_loc ~chapter:0
  |> Env.add_closure (lower prefix "reachable")
       (Types.TyFunc
          ( [ Types.TyDomain domain ],
            Some (Types.TyList (Types.TyDomain domain)) ))
       (lower prefix "active") Ast.dummy_loc ~chapter:0

let config =
  Smt_types.make_config ~bound:3 ~steps:1 ~domain_bounds:Env.StringMap.empty
    ~inject_guards:true ()

let sample_param prefix : Ast.param =
  {
    param_name = Ast.Lower (lower prefix "u");
    param_type = Ast.TName (Ast.Upper (upper prefix "User"));
  }

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_domain_sorts emits sort declarations"
         ~count:100 gen_prefix (fun prefix ->
           String.contains
             (Smt_preamble.declare_domain_sorts config (env prefix))
             (upper prefix "User").[0]));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_composite_types emits datatypes"
         ~count:100 gen_prefix (fun prefix ->
           String.length (Smt_preamble.declare_composite_types (env prefix)) > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"decompose_func_ty recognizes functions"
         ~count:100 gen_prefix (fun prefix ->
           match[@warning "-4"]
             Smt_preamble.decompose_func_ty
               (Types.TyFunc
                  ([ Types.TyDomain (upper prefix "User") ], Some Types.TyBool))
           with
           | Some ([ Types.TyDomain domain ], Types.TyBool) ->
               domain = upper prefix "User"
           | Some _ | None -> false));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_functions emits declarations" ~count:100
         gen_prefix (fun prefix ->
           String.contains
             (Smt_preamble.declare_functions (env prefix))
             (lower prefix "active").[0]));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"generate_closure_axioms returns text" ~count:100
         gen_prefix (fun prefix ->
           String.length
             (Smt_preamble.generate_closure_axioms config (env prefix))
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"collect_type_constraint_exprs returns constraints" ~count:100
         gen_prefix (fun prefix ->
           Smt_preamble.collect_type_constraint_exprs config (env prefix) <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_type_constraints emits assertions"
         ~count:100 gen_prefix (fun prefix ->
           String.length
             (Smt_preamble.declare_type_constraints config (env prefix))
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"generate_preamble emits declarations" ~count:100
         gen_prefix (fun prefix ->
           String.length (Smt_preamble.generate_preamble config (env prefix))
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"replace_word respects word boundaries"
         ~count:100 gen_prefix (fun prefix ->
           let from_word = lower prefix "from" in
           let to_word = lower prefix "to" in
           Smt_preamble.replace_word ~from:from_word ~to_:to_word
             ("(and " ^ from_word ^ " " ^ from_word ^ "x)")
           = "(and " ^ to_word ^ " " ^ from_word ^ "x)"));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"resolve_param_bindings resolves params"
         ~count:100 gen_prefix (fun prefix ->
           Smt_preamble.resolve_param_bindings (env prefix)
             [ sample_param prefix ]
           = [ (lower prefix "u", Types.TyDomain (upper prefix "User")) ]));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"resolve_param_sort resolves type exprs"
         ~count:100 gen_prefix (fun prefix ->
           Smt_preamble.resolve_param_sort (env prefix)
             (Ast.TName (Ast.Upper (upper prefix "User")))
           = Some (upper prefix "User")));
  ]

let () = run "Smt_preamble" [ ("public_api", public_api_properties) ]
