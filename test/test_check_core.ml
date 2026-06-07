(* @archlint.module test
   @archlint.domain pantagruel.check *)

open Alcotest
open Pantagruel

let loc = Ast.dummy_loc

let env =
  Env.empty "CheckCore"
  |> Env.add_domain "User" loc ~chapter:0
  |> Env.add_rule "active"
       (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
       loc ~chapter:0
  |> Env.add_rule "pi" (Types.TyFunc ([], Some Types.TyNat)) loc ~chapter:0
  |> Env.add_var "u" (Types.TyDomain "User")

let parse_and_collect source =
  let doc = Test_util.parse source in
  match Collect.collect_all ~base_env:(Env.empty "CheckCore") doc with
  | Ok env -> (env, doc)
  | Error e -> fail (Collect.show_collect_error e)

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"infer_type classifies rule applications"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let ctx = Check.{ env; loc } in
           Check.infer_type ctx
             (Ast.EApp
                (Ast.EVar (Ast.Lower "active"), [ Ast.EVar (Ast.Lower "u") ]))
           = Ok Types.TyBool));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"check_document accepts valid docs and returns warnings"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let env, doc =
             parse_and_collect
               {|module CheckCore.

Item.
pi => Nat.
circleArea r: Nat, pi: Nat => Nat.
---
all r: Nat, pi: Nat | circleArea r pi = pi * r * r.
|}
           in
           match Check.check_document env doc with
           | Ok warnings ->
               List.exists
                 (function
                   | Check.NullaryRuleShadowedByVar ("pi", _, _, _) -> true
                   | UnboundVariable _ | UnboundType _ | TypeMismatch _
                   | ArityMismatch _ | NotAFunction _ | NotAList _
                   | NotAProduct _ | NotNumeric _ | ExpectedBool _
                   | PrimedNonRule _ | PrimeOutsideActionContext _
                   | OverrideKeyArityMismatch _ | ProjectionOutOfBounds _
                   | PropositionNotBool _ | ShadowingTypeMismatch _
                   | AmbiguousName _ | UnboundQualified _
                   | PrimedExtracontextual _ | BoolParam _
                   | NullaryRuleShadowedByVar _ | ComprehensionNeedEach _
                   | AggregateRequiresNumeric _ | AggregateRequiresBool _
                   | CheckWithoutBody _ ->
                       false)
                 warnings
           | Error _ -> false));
  ]

let () = run "Check_core" [ ("public_api", public_api_properties) ]
