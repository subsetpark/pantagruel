(* @archlint.module test
   @archlint.domain pantagruel.error *)

(** Tests for error formatting *)

open Alcotest
open Pantagruel

let loc = { Ast.file = "test.pant"; line = 10; col = 5 }
let gen_name = QCheck2.Gen.oneof_list [ "x"; "y"; "Foo"; "Ctx"; "rule" ]

let gen_loc =
  let open QCheck2.Gen in
  let* file = oneof_list [ "a.pant"; "b.pant"; "nested/file.pant" ] in
  let* line = int_range 1 500 in
  let* col = int_range 0 120 in
  return { Ast.file; line; col }

let print_loc loc = Error.format_loc loc

let has_prefix ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let has_location_prefix loc s =
  has_prefix ~prefix:(Error.format_loc loc ^ ":") s

let gen_type_error =
  let module Gen = QCheck2.Gen in
  Gen.bind gen_loc (fun loc ->
      Gen.oneof
        [
          Gen.map (fun name -> Check.UnboundVariable (name, loc)) gen_name;
          Gen.map (fun name -> Check.UnboundType (name, loc)) gen_name;
          Gen.map2
            (fun expected got -> Check.TypeMismatch (expected, got, loc))
            Test_util.gen_ty Test_util.gen_ty;
          Gen.map2
            (fun expected got -> Check.ArityMismatch (expected, got, loc))
            (Gen.int_range 0 5) (Gen.int_range 0 5);
          Gen.map (fun ty -> Check.NotAFunction (ty, loc)) Test_util.gen_ty;
          Gen.map (fun ty -> Check.ExpectedBool (ty, loc)) Test_util.gen_ty;
          Gen.map (fun name -> Check.PrimedNonRule (name, loc)) gen_name;
          Gen.return (Check.CheckWithoutBody loc);
        ])

let loc_of_type_error = function
  | Check.UnboundVariable (_, loc)
  | UnboundType (_, loc)
  | TypeMismatch (_, _, loc)
  | ArityMismatch (_, _, loc)
  | NotAFunction (_, loc)
  | NotAList (_, loc)
  | NotAProduct (_, loc)
  | NotNumeric (_, loc)
  | ExpectedBool (_, loc)
  | PrimedNonRule (_, loc)
  | PrimeOutsideActionContext (_, loc)
  | OverrideKeyArityMismatch (_, _, loc)
  | ProjectionOutOfBounds (_, _, loc)
  | PropositionNotBool (_, loc)
  | ShadowingTypeMismatch (_, _, _, loc)
  | AmbiguousName (_, _, loc)
  | UnboundQualified (_, _, loc)
  | PrimedExtracontextual (_, _, loc)
  | BoolParam (_, _, loc)
  | NullaryRuleShadowedByVar (_, _, _, loc)
  | ComprehensionNeedEach (_, loc)
  | AggregateRequiresNumeric (_, _, loc)
  | AggregateRequiresBool (_, _, loc)
  | CheckWithoutBody loc ->
      loc

let gen_collect_error =
  let module Gen = QCheck2.Gen in
  Gen.bind gen_loc (fun loc ->
      Gen.bind gen_loc (fun first_loc ->
          Gen.oneof
            [
              Gen.map
                (fun name -> Collect.DuplicateDomain (name, loc, first_loc))
                gen_name;
              Gen.map
                (fun name -> Collect.DuplicateRule (name, loc, first_loc))
                gen_name;
              Gen.map (fun name -> Collect.UndefinedType (name, loc)) gen_name;
              Gen.map (fun name -> Collect.RecursiveAlias (name, loc)) gen_name;
              Gen.map2
                (fun a b -> Collect.MultipleActions (a, b, loc))
                gen_name gen_name;
              Gen.map (fun name -> Collect.ActionNotLast (name, loc)) gen_name;
              Gen.map
                (fun name -> Collect.BuiltinRedefined (name, loc))
                gen_name;
              Gen.map
                (fun name -> Collect.DuplicateContext (name, loc))
                gen_name;
              Gen.map
                (fun name -> Collect.UndefinedContext (name, loc))
                gen_name;
              Gen.map
                (fun name -> Collect.ClosureTargetInvalid (name, "reason", loc))
                gen_name;
            ]))

let loc_of_collect_error = function
  | Collect.DuplicateDomain (_, loc, _)
  | DuplicateRule (_, loc, _)
  | UndefinedType (_, loc)
  | RecursiveAlias (_, loc)
  | MultipleActions (_, _, loc)
  | ActionNotLast (_, loc)
  | BuiltinRedefined (_, loc)
  | DuplicateContext (_, loc)
  | UndefinedContext (_, loc)
  | ClosureTargetInvalid (_, _, loc) ->
      loc
  | OverloadCoherenceViolation { second = _, _, loc; _ } -> loc

let gen_type_warning =
  let module Gen = QCheck2.Gen in
  Gen.bind gen_loc (fun loc ->
      Gen.oneof
        [
          Gen.map3
            (fun name old_ty new_ty ->
              Check.ShadowingTypeMismatch (name, old_ty, new_ty, loc))
            gen_name Test_util.gen_ty Test_util.gen_ty;
          Gen.map2
            (fun name decl_name -> Check.BoolParam (name, decl_name, loc))
            gen_name gen_name;
          Gen.map3
            (fun name rule_ret var_ty ->
              Check.NullaryRuleShadowedByVar (name, rule_ret, var_ty, loc))
            gen_name Test_util.gen_ty Test_util.gen_ty;
        ])

(* --- format_type_error: one test per variant --- *)

let test_format_type_error_coverage () =
  let cases : (string * Check.type_error) list =
    [
      ("UnboundVariable", UnboundVariable ("x", loc));
      ("UnboundType", UnboundType ("Foo", loc));
      ("TypeMismatch", TypeMismatch (TyBool, TyNat, loc));
      ("ArityMismatch", ArityMismatch (2, 3, loc));
      ("NotAFunction", NotAFunction (TyBool, loc));
      ("NotAList", NotAList (TyBool, loc));
      ("NotAProduct", NotAProduct (TyBool, loc));
      ("NotNumeric", NotNumeric (TyBool, loc));
      ("ExpectedBool", ExpectedBool (TyNat, loc));
      ("PrimedNonRule", PrimedNonRule ("x", loc));
      ("PrimeOutsideActionContext", PrimeOutsideActionContext ("f", loc));
      ("OverrideKeyArityMismatch", OverrideKeyArityMismatch ("f", 2, loc));
      ("ProjectionOutOfBounds", ProjectionOutOfBounds (5, 2, loc));
      ("PropositionNotBool", PropositionNotBool (TyNat, loc));
      ("ShadowingTypeMismatch", ShadowingTypeMismatch ("x", TyBool, TyNat, loc));
      ("AmbiguousName", AmbiguousName ("f", [ "A"; "B" ], loc));
      ("UnboundQualified", UnboundQualified ("Mod", "name", loc));
      ("PrimedExtracontextual", PrimedExtracontextual ("f", [ "Ctx" ], loc));
      ("BoolParam", BoolParam ("b", "rule", loc));
      ("ComprehensionNeedEach", ComprehensionNeedEach (TyNat, loc));
      ("AggregateRequiresNumeric", AggregateRequiresNumeric ("+", TyBool, loc));
      ("AggregateRequiresBool", AggregateRequiresBool ("and", TyNat, loc));
      ("CheckWithoutBody", CheckWithoutBody loc);
    ]
  in
  List.iter
    (fun (name, err) ->
      let formatted = Error.format_type_error err in
      check bool
        (Printf.sprintf "%s produces non-empty output" name)
        true
        (String.length formatted > 0);
      check bool
        (Printf.sprintf "%s contains location prefix" name)
        true
        (String.length formatted > 10
        && String.sub formatted 0 10 = "test.pant:"))
    cases

(* --- format_collect_error: one test per variant --- *)

let test_format_collect_error_coverage () =
  let loc2 = { Ast.file = "test.pant"; line = 1; col = 1 } in
  let cases : (string * Collect.collect_error) list =
    [
      ("DuplicateDomain", DuplicateDomain ("User", loc, loc2));
      ("DuplicateRule", DuplicateRule ("f", loc, loc2));
      ("UndefinedType", UndefinedType ("Foo", loc));
      ("RecursiveAlias", RecursiveAlias ("T", loc));
      ("MultipleActions", MultipleActions ("a1", "a2", loc));
      ("ActionNotLast", ActionNotLast ("act", loc));
      ("BuiltinRedefined", BuiltinRedefined ("Bool", loc));
      ("DuplicateContext", DuplicateContext ("Ctx", loc));
      ("UndefinedContext", UndefinedContext ("Ctx", loc));
      ("ClosureTargetInvalid", ClosureTargetInvalid ("c", "reason", loc));
    ]
  in
  List.iter
    (fun (name, err) ->
      let formatted = Error.format_collect_error err in
      check bool
        (Printf.sprintf "%s produces non-empty output" name)
        true
        (String.length formatted > 0);
      check bool
        (Printf.sprintf "%s contains location prefix" name)
        true
        (String.length formatted > 10
        && String.sub formatted 0 10 = "test.pant:"))
    cases

(* --- format_type_warning tests --- *)

let test_format_type_warning_shadowing () =
  let formatted =
    Error.format_type_warning
      (Check.ShadowingTypeMismatch ("x", TyBool, TyNat, loc))
  in
  check bool "contains 'warning'" true
    (let re = "warning" in
     let len = String.length re in
     let flen = String.length formatted in
     let rec find i =
       if i + len > flen then false
       else if String.sub formatted i len = re then true
       else find (i + 1)
     in
     find 0)

let test_format_type_warning_bool_param () =
  let formatted =
    Error.format_type_warning (Check.BoolParam ("b", "rule", loc))
  in
  check bool "contains 'warning'" true
    (let re = "warning" in
     let len = String.length re in
     let flen = String.length formatted in
     let rec find i =
       if i + len > flen then false
       else if String.sub formatted i len = re then true
       else find (i + 1)
     in
     find 0)

let test_format_loc_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"format_loc includes file line and column"
       ~count:200 ~print:print_loc gen_loc (fun loc ->
         Error.format_loc loc
         = Printf.sprintf "%s:%d:%d" loc.Ast.file loc.line loc.col))

let test_format_type_error_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"format_type_error is location-prefixed" ~count:300
       ~print:Check.show_type_error gen_type_error (fun err ->
         let formatted = Error.format_type_error err in
         formatted <> ""
         && has_location_prefix (loc_of_type_error err) formatted))

let test_format_collect_error_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"format_collect_error is location-prefixed"
       ~count:300 ~print:Collect.show_collect_error gen_collect_error
       (fun err ->
         let formatted = Error.format_collect_error err in
         formatted <> ""
         && has_location_prefix (loc_of_collect_error err) formatted))

let test_format_type_warning_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"format_type_warning is location-prefixed"
       ~count:300 ~print:Check.show_type_error gen_type_warning (fun warning ->
         let formatted = Error.format_type_warning warning in
         formatted <> ""
         && has_location_prefix (loc_of_type_error warning) formatted))

let () =
  run "Error"
    [
      ( "format_type_error",
        [ test_case "all variants" `Quick test_format_type_error_coverage ] );
      ( "format_collect_error",
        [ test_case "all variants" `Quick test_format_collect_error_coverage ]
      );
      ( "format_type_warning",
        [
          test_case "shadowing" `Quick test_format_type_warning_shadowing;
          test_case "bool param" `Quick test_format_type_warning_bool_param;
        ] );
      ( "property",
        [
          test_format_loc_property;
          test_format_type_error_property;
          test_format_collect_error_property;
          test_format_type_warning_property;
        ] );
    ]
