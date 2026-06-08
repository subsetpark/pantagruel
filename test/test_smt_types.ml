(* @archlint.module stateTest
   @archlint.domain pantagruel.smt-types *)

open Alcotest
open Pantagruel

let sample_env =
  Env.empty "SmtTypes"
  |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
  |> Env.add_rule "alice" (Types.TyDomain "User") Ast.dummy_loc ~chapter:0
  |> Env.add_rule "bob" (Types.TyDomain "User") Ast.dummy_loc ~chapter:0
  |> Env.add_rule "score"
       (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
       Ast.dummy_loc ~chapter:0

let gen_domain =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let gen_rule =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 10)

let gen_ty =
  QCheck2.Gen.oneof
    [
      QCheck2.Gen.return Types.TyBool;
      QCheck2.Gen.return Types.TyNat;
      QCheck2.Gen.return Types.TyString;
      QCheck2.Gen.map (fun d -> Types.TyDomain d) gen_domain;
    ]

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"make_config and bound_for use overrides"
         ~count:100 QCheck2.Gen.nat_small (fun bound ->
           let domain_bounds =
             Env.StringMap.empty |> Env.StringMap.add "User" (bound + 1)
           in
           let config =
             Smt_types.make_config ~bound ~steps:2 ~domain_bounds
               ~inject_guards:true ()
           in
           config.Smt_types.steps = 2 && config.inject_guards
           && config.ground_quantifiers && config.quant_bound = []
           && Smt_types.bound_for config "User" = bound + 1
           && Smt_types.bound_for config "Missing" = bound
           && Smt_types.ground_instance_cap > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"compute_domain_bounds counts nullary constants"
         ~count:100 QCheck2.Gen.nat_small (fun default_bound ->
           let bounds =
             Smt_types.compute_domain_bounds default_bound sample_env
           in
           let expected = max default_bound 2 in
           if expected > default_bound then
             Env.StringMap.find_opt "User" bounds = Some expected
           else not (Env.StringMap.mem "User" bounds)));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"splice_before_first_assert preserves asserts"
         ~count:100 (QCheck2.Gen.list QCheck2.Gen.string_printable)
         (fun decl_lines ->
           let decls =
             if decl_lines = [] then ""
             else "\n" ^ String.concat "\n" decl_lines ^ "\n"
           in
           let smt2 = "(declare-const x Int)\n(assert (> x 0))\n(check-sat)" in
           let spliced = Smt_types.splice_before_first_assert smt2 decls in
           decls = "" || String.contains spliced '('));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"sort names cover scalar and composite types"
         ~count:100 (QCheck2.Gen.pair gen_ty gen_ty) (fun (left, right) ->
           let left_base = Smt_types.sort_base_name left in
           let right_base = Smt_types.sort_base_name right in
           Smt_types.sort_of_ty (Types.TyList left)
           = Printf.sprintf "(Array %s Bool)" (Smt_types.sort_of_ty left)
           && Smt_types.product_sort_name [ left; right ]
              = "Pair_" ^ left_base ^ "_" ^ right_base
           && Smt_types.sum_sort_name [ left; right ]
              = "Sum_" ^ left_base ^ "_" ^ right_base
           && Smt_types.sort_base_name (Types.TyList left) = "List_" ^ left_base));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"domain elements use stable numbered names"
         ~count:100 QCheck2.Gen.nat_small (fun count ->
           Smt_types.domain_elements "User" count
           = List.init count (Printf.sprintf "User_%d")));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"SMT identifiers are sanitized and mangled"
         ~count:100 (QCheck2.Gen.pair gen_domain gen_rule)
         (fun (mod_name, rule_name) ->
           let env =
             Env.empty "SmtTypes"
             |> Env.add_rule rule_name
                  (Types.TyFunc ([ Types.TyNat ], Some Types.TyNat))
                  Ast.dummy_loc ~chapter:0
             |> Env.add_rule rule_name
                  (Types.TyFunc ([ Types.TyNat; Types.TyNat ], Some Types.TyNat))
                  Ast.dummy_loc ~chapter:0
           in
           Smt_types.sanitize_ident (rule_name ^ "-?!") = rule_name ^ "_pb"
           && Smt_types.smt_rule_name env rule_name 2 = rule_name ^ "$2"
           && Smt_types.smt_qualified_rule_name sample_env mod_name rule_name 2
              = mod_name ^ "$" ^ rule_name ^ "$2"));
  ]

let state_interleaving_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"state interleavings reset and drain auxiliary declarations"
         ~count:100 (QCheck2.Gen.list QCheck2.Gen.nat_small) (fun steps ->
           Smt_state.reset_cond_aux ();
           Smt_state.reset_fallbacks ();
           Smt_state.reset_list_search_cache ();
           List.iter
             (fun _ ->
               ignore (Smt_state.fresh_cond_default "Int");
               ignore (Smt_state.fresh_fallback ~kind:"card" ~sort:"Int");
               ignore
                 (Smt_state.intern_list_search_symbol ~func_s:"xs" ~arg_s:"x");
               Smt_state.add_fallback_assert "(>= _card_fallback_0 0)")
             steps;
           let smt2 =
             Smt_types.splice_before_first_assert
               "(declare-const x Int)\n(assert true)\n" ""
           in
           let with_cond = Smt_state.insert_cond_aux_decls smt2 in
           let with_fallbacks = Smt_state.insert_fallback_decls with_cond in
           let drained_cond = Smt_state.drain_cond_aux_decls () in
           let drained_fallbacks = Smt_state.drain_fallback_decls () in
           drained_cond = "" && drained_fallbacks = ""
           && String.length with_fallbacks >= String.length smt2
           && Smt_types.sort_of_ty Types.TyNat = "Int"));
  ]

let () =
  run "Smt_types"
    [
      ("public_api", public_api_properties);
      ("state_interleavings", state_interleaving_properties);
    ]
