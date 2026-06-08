(* @archlint.module test
   @archlint.domain pantagruel.check *)

open Alcotest
open Pantagruel

let loc = Ast.dummy_loc

let parse_and_collect source =
  let doc = Test_util.parse source in
  match Collect.collect_all ~base_env:(Env.empty "CheckCore") doc with
  | Ok env -> (env, doc)
  | Error e -> fail (Collect.show_collect_error e)

let gen_upper =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let gen_lower =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 10)

let public_api_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"infer_type classifies rule applications"
         ~count:100 (QCheck2.Gen.pair gen_lower gen_lower)
         (fun (rule_name, var_name) ->
           let env =
             Env.empty "CheckCore"
             |> Env.add_domain "User" loc ~chapter:0
             |> Env.add_rule rule_name
                  (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
                  loc ~chapter:0
             |> Env.add_var var_name (Types.TyDomain "User")
           in
           let ctx = Check.{ env; loc } in
           Check.infer_type ctx
             (Ast.EApp
                ( Ast.EVar (Ast.Lower rule_name),
                  [ Ast.EVar (Ast.Lower var_name) ] ))
           = Ok Types.TyBool));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"check_document accepts valid docs and returns warnings"
         ~count:100 (QCheck2.Gen.triple gen_upper gen_lower gen_lower)
         (fun (module_name, const_name, area_name) ->
           let area_name =
             if String.equal const_name area_name then area_name ^ "f"
             else area_name
           in
           let env, doc =
             parse_and_collect
               (Printf.sprintf
                  {|module %s.

Item.
%s => Nat.
%s r: Nat, %s: Nat => Nat.
---
all r: Nat, %s: Nat | %s r %s = %s * r * r.
|}
                  module_name const_name area_name const_name const_name
                  area_name const_name const_name)
           in
           match Check.check_document env doc with
           | Ok warnings ->
               List.exists
                 (function
                   | Check.NullaryRuleShadowedByVar (name, _, _, _) ->
                       String.equal name const_name
                   | UnboundVariable _ | UnboundType _ | TypeMismatch _
                   | ArityMismatch _ | NotAFunction _ | NotAList _
                   | NotAProduct _ | NotNumeric _ | ExpectedBool _
                   | PrimedNonRule _ | PrimeOutsideActionContext _
                   | OverrideKeyArityMismatch _ | ProjectionOutOfBounds _
                   | PropositionNotBool _ | ShadowingTypeMismatch _
                   | AmbiguousName _ | UnboundQualified _
                   | PrimedExtracontextual _ | BoolParam _
                   | ComprehensionNeedEach _ | AggregateRequiresNumeric _
                   | AggregateRequiresBool _ | CheckWithoutBody _ ->
                       false)
                 warnings
           | Error _ -> false));
  ]

let () = run "Check_core" [ ("public_api", public_api_properties) ]
