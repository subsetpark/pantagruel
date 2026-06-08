(* @archlint.module test
   @archlint.domain pantagruel.smt-doc *)

open Alcotest
open Pantagruel

let gen_prefix =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 8)

let upper prefix suffix = String.capitalize_ascii prefix ^ suffix
let lower prefix suffix = prefix ^ suffix

let invariant_source prefix =
  Printf.sprintf
    {|module %s.
%s.
%s u: %s => Bool.
---
initially true.
all u: %s | %s u.
check
all u: %s | %s u.
|}
    (upper prefix "T") (upper prefix "User") (lower prefix "active")
    (upper prefix "User") (upper prefix "User") (lower prefix "active")
    (upper prefix "User") (lower prefix "active")

let action_source prefix =
  Printf.sprintf
    {|module %s.
context %s.
%s.
{%s} %s u: %s => Bool.
%s u: %s => Nat.
%s ~> %s @ u: %s, n: Nat0, n < 3.
---
%s' u.
check
%s' u.
|}
    (upper prefix "T") (upper prefix "C") (upper prefix "User")
    (upper prefix "C") (lower prefix "active") (upper prefix "User")
    (lower prefix "count") (upper prefix "User") (upper prefix "C")
    (upper prefix "Activate") (upper prefix "User") (lower prefix "active")
    (lower prefix "active")

let parse_and_collect = Test_util.parse_and_collect

let config =
  Smt_types.make_config ~bound:3 ~steps:1 ~domain_bounds:Env.StringMap.empty
    ~inject_guards:true ()

let located value =
  { Ast.loc = Ast.dummy_loc; value; doc = []; doc_adjacent = false }

let invariant_doc prefix = snd (parse_and_collect (invariant_source prefix))
let action_env_doc prefix = parse_and_collect (action_source prefix)

let action_info action_doc =
  match Smt_doc.collect_actions (Smt_doc.classify_chapters action_doc) with
  | action :: _ -> action
  | [] -> fail "expected action"

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"free_vars finds unbound variables" ~count:100
         gen_prefix (fun prefix ->
           Smt_doc.free_vars (Ast.EVar (Ast.Lower (lower prefix "x")))
           |> Smt_doc.StringSet.mem (lower prefix "x")));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"bind_head_params quantifies used params"
         ~count:100 gen_prefix (fun prefix ->
           let param =
             {
               Ast.param_name = Ast.Lower (lower prefix "u");
               param_type = Ast.TName (Ast.Upper (upper prefix "User"));
             }
           in
           match[@warning "-4"]
             Smt_doc.bind_head_params [ param ]
               (located (Ast.EVar (Ast.Lower (lower prefix "u"))))
           with
           | { Ast.value = Ast.EForall _; _ } -> true
           | _ -> false));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"classify_chapters classifies document chapters"
         ~count:100 gen_prefix (fun prefix ->
           let _env, action_doc = action_env_doc prefix in
           Smt_doc.classify_chapters action_doc <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_invariants finds invariant props"
         ~count:100 gen_prefix (fun prefix ->
           Smt_doc.collect_invariants
             (Smt_doc.classify_chapters (invariant_doc prefix))
           <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_initial_props finds initially props"
         ~count:100 gen_prefix (fun prefix ->
           Smt_doc.collect_initial_props
             (Smt_doc.classify_chapters (invariant_doc prefix))
           <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_actions finds actions" ~count:100
         gen_prefix (fun prefix ->
           let _env, action_doc = action_env_doc prefix in
           Smt_doc.collect_actions (Smt_doc.classify_chapters action_doc) <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_checks finds checks" ~count:100
         gen_prefix (fun prefix ->
           let _env, action_doc = action_env_doc prefix in
           Smt_doc.collect_checks (Smt_doc.classify_chapters action_doc) <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_frame_exprs returns frame expressions"
         ~count:100 gen_prefix (fun prefix ->
           let action_env, _doc = action_env_doc prefix in
           Smt_doc.collect_frame_exprs config action_env [ upper prefix "C" ]
           <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"generate_frame_conditions returns text"
         ~count:100 gen_prefix (fun prefix ->
           let action_env, _doc = action_env_doc prefix in
           String.length
             (Smt_doc.generate_frame_conditions config action_env
                [ upper prefix "C" ])
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"env_with_action_params extends env" ~count:100
         gen_prefix (fun prefix ->
           let action_env, action_doc = action_env_doc prefix in
           let params = (action_info action_doc).Smt_doc.a_params in
           let env = Smt_doc.env_with_action_params action_env params in
           Env.lookup_var "u" env <> None));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_action_params emits declarations"
         ~count:100 gen_prefix (fun prefix ->
           let action_env, action_doc = action_env_doc prefix in
           String.length
             (Smt_doc.declare_action_params action_env
                (action_info action_doc).Smt_doc.a_params)
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_param_constraints emits constraints"
         ~count:100 gen_prefix (fun prefix ->
           let action_env, action_doc = action_env_doc prefix in
           String.length
             (Smt_doc.declare_param_constraints action_env
                (action_info action_doc).Smt_doc.a_params)
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"extract_precondition_exprs finds guard exprs"
         ~count:100 gen_prefix (fun prefix ->
           let _env, action_doc = action_env_doc prefix in
           Smt_doc.extract_precondition_exprs
             (action_info action_doc).Smt_doc.a_guards
           <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"action_always_enabled checks guards" ~count:100
         gen_prefix (fun prefix ->
           let _env, action_doc = action_env_doc prefix in
           not (Smt_doc.action_always_enabled (action_info action_doc))));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_function_refs finds rule names"
         ~count:100 gen_prefix (fun prefix ->
           List.mem (lower prefix "active")
             (Smt_doc.collect_function_refs
                (Ast.EApp (Ast.EVar (Ast.Lower (lower prefix "active")), [])))));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"invariant_touches_context returns a decision"
         ~count:100 gen_prefix (fun prefix ->
           let action_env, _doc = action_env_doc prefix in
           Smt_doc.invariant_touches_context action_env
             [ upper prefix "C" ]
             [ located (Ast.EVar (Ast.Lower (lower prefix "active"))) ]));
  ]

let () = run "Smt_doc" [ ("public_api", public_api_properties) ]
