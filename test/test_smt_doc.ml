(* @archlint.module test
   @archlint.domain pantagruel.smt-doc *)

open Alcotest
open Pantagruel

let invariant_source =
  {|module T.
User.
active u: User => Bool.
---
initially true.
all u: User | active u.
check
all u: User | active u.
|}

let action_source =
  {|module T.
context C.
User.
{C} active u: User => Bool.
count u: User => Nat.
C ~> Activate @ u: User, n: Nat0, n < 3.
---
active' u.
check
active' u.
|}

let parse_and_collect = Test_util.parse_and_collect
let _invariant_env, invariant_doc = parse_and_collect invariant_source
let action_env, action_doc = parse_and_collect action_source

let config =
  Smt_types.make_config ~bound:3 ~steps:1 ~domain_bounds:Env.StringMap.empty
    ~inject_guards:true ()

let located value =
  { Ast.loc = Ast.dummy_loc; value; doc = []; doc_adjacent = false }

let action_info () =
  match Smt_doc.collect_actions (Smt_doc.classify_chapters action_doc) with
  | action :: _ -> action
  | [] -> fail "expected action"

let action_params () = (action_info ()).Smt_doc.a_params
let action_guards () = (action_info ()).Smt_doc.a_guards

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"free_vars finds unbound variables" ~count:100
         QCheck2.Gen.unit (fun () ->
           Smt_doc.free_vars (Ast.EVar (Ast.Lower "x"))
           |> Smt_doc.StringSet.mem "x"));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"bind_head_params quantifies used params"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let param =
             {
               Ast.param_name = Ast.Lower "u";
               param_type = Ast.TName (Ast.Upper "User");
             }
           in
           match[@warning "-4"]
             Smt_doc.bind_head_params [ param ]
               (located (Ast.EVar (Ast.Lower "u")))
           with
           | { Ast.value = Ast.EForall _; _ } -> true
           | _ -> false));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"classify_chapters classifies document chapters"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_doc.classify_chapters action_doc <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_invariants finds invariant props"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_doc.collect_invariants (Smt_doc.classify_chapters invariant_doc)
           <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_initial_props finds initially props"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_doc.collect_initial_props
             (Smt_doc.classify_chapters invariant_doc)
           <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_actions finds actions" ~count:100
         QCheck2.Gen.unit (fun () ->
           Smt_doc.collect_actions (Smt_doc.classify_chapters action_doc) <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_checks finds checks" ~count:100
         QCheck2.Gen.unit (fun () ->
           Smt_doc.collect_checks (Smt_doc.classify_chapters action_doc) <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_frame_exprs returns frame expressions"
         ~count:100 QCheck2.Gen.unit (fun () ->
           ignore (Smt_doc.collect_frame_exprs config action_env [ "C" ]);
           true));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"generate_frame_conditions returns text"
         ~count:100 QCheck2.Gen.unit (fun () ->
           ignore (Smt_doc.generate_frame_conditions config action_env [ "C" ]);
           true));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"env_with_action_params extends env" ~count:100
         QCheck2.Gen.unit (fun () ->
           let env =
             Smt_doc.env_with_action_params action_env (action_params ())
           in
           Env.lookup_var "u" env <> None));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_action_params emits declarations"
         ~count:100 QCheck2.Gen.unit (fun () ->
           String.length
             (Smt_doc.declare_action_params action_env (action_params ()))
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"declare_param_constraints emits constraints"
         ~count:100 QCheck2.Gen.unit (fun () ->
           String.length
             (Smt_doc.declare_param_constraints action_env (action_params ()))
           > 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"extract_precondition_exprs finds guard exprs"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Smt_doc.extract_precondition_exprs (action_guards ()) <> []));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"action_always_enabled checks guards" ~count:100
         QCheck2.Gen.unit (fun () ->
           not (Smt_doc.action_always_enabled (action_info ()))));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"collect_function_refs finds rule names"
         ~count:100 QCheck2.Gen.unit (fun () ->
           List.mem "active"
             (Smt_doc.collect_function_refs
                (Ast.EApp (Ast.EVar (Ast.Lower "active"), [])))));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"invariant_touches_context returns a decision"
         ~count:100 QCheck2.Gen.unit (fun () ->
           ignore
             (Smt_doc.invariant_touches_context action_env [ "C" ]
                [ located (Ast.EVar (Ast.Lower "active")) ]);
           true));
  ]

let () = run "Smt_doc" [ ("public_api", public_api_properties) ]
