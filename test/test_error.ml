(** Tests for error formatting *)

open Alcotest
open Pantagruel

let loc = { Ast.file = "test.pant"; line = 10; col = 5 }

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
      ("OverrideRequiresArity1", OverrideRequiresArity1 ("f", 2, loc));
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
    ]
