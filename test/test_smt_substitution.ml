(** Capture-avoiding substitution tests for the Bindlib migration.

    PENDING: Patch 4. Every test in this file is skipped via [Alcotest.skip] —
    the executable compiles and runs, but the assertions are inert. Patch 4
    (Activate capture-avoidance tests) removes the [pending ()] calls once the
    AST reshape (Patch 3) is in place and [Smt.substitute_vars] /
    [Smt.free_vars] delegate to [Pantagruel.Binder].

    The test bodies after [pending ()] are intentionally written against the
    *current* (pre-Patch-3) AST and walker API. They are dead code today; they
    act as a type-check on the call shapes Patch 4 will assert against. If Patch
    3's AST reshape breaks these bodies, Patch 4 must update them in lockstep
    with activating the checks. *)

open Alcotest
open Pantagruel

(** Marker for tests that should compile and run but remain inert until a later
    patch activates them. Alcotest treats [skip ()] as a skipped test case. The
    [unit] return annotation is what tells the compiler [pending] is not a
    non-returning call, so the assertion skeleton that follows each invocation
    is accepted as live (though never executed) code. *)
let pending () : unit = skip ()

(* ------------------------------------------------------------------ *)
(* Test 1: substitute_vars is the identity when the domain name is not
   free in e.                                                           *)
(* ------------------------------------------------------------------ *)

let test_substitute_identity_on_nonfree () =
  pending ();
  let e : Ast.expr =
    Ast.EBinop
      ( Ast.OpAnd,
        Ast.EVar (Ast.Lower "a"),
        Ast.EBinop (Ast.OpEq, Ast.EVar (Ast.Lower "b"), Ast.ELitNat 0) )
  in
  let subst = [ ("x", Ast.EVar (Ast.Lower "y")) ] in
  check bool "substitute_vars is identity when domain name is not free" true
    (Smt.substitute_vars subst e = e)

(* ------------------------------------------------------------------ *)
(* Test 2: substitute_vars [x -> EVar y] (EForall [y:T] (EVar x))
   produces a quantifier alpha-equivalent to EForall [z:T] (EVar y).
   The current implementation captures — its binder [y] swallows the
   fresh occurrence of [y] introduced by the substitution. The
   post-migration implementation must alpha-rename the binder before
   substituting.                                                        *)
(* ------------------------------------------------------------------ *)

let test_substitute_avoids_capture () =
  pending ();
  let t : Ast.type_expr = Ast.TName (Ast.Upper "T") in
  let param_y : Ast.param = { param_name = Ast.Lower "y"; param_type = t } in
  let body_x = Ast.EVar (Ast.Lower "x") in
  let input = Ast.make_forall [ param_y ] [] body_x in
  let subst = [ ("x", Ast.EVar (Ast.Lower "y")) ] in
  let result = Smt.substitute_vars subst input in
  (* Expected: a forall whose binder has been alpha-renamed away from [y]
     (e.g. [z:T]) and whose body references the substituted [y] as a FREE
     variable. Equivalently: the free variables of the result must include
     [y]. The current implementation returns [EForall [y:T] | y] where [y] is
     bound — so [y] is NOT free. Patch 4 asserts [y] IS free. *)
  let free = Smt.free_vars result in
  check bool "y is free in the substituted quantifier" true
    (Smt.StringSet.mem "y" free)

(* ------------------------------------------------------------------ *)
(* Test 3: alpha-equivalent inputs produce alpha-equivalent outputs
   under substitute_vars.                                              *)
(* ------------------------------------------------------------------ *)

let test_substitute_preserves_alpha_equivalence () =
  pending ();
  let t : Ast.type_expr = Ast.TName (Ast.Upper "T") in
  let mk name : Ast.param = { param_name = Ast.Lower name; param_type = t } in
  (* [all a: T | f a]  vs  [all b: T | f b] — alpha-equivalent. *)
  let e1 =
    Ast.make_forall
      [ mk "a" ]
      []
      (Ast.EApp (Ast.EVar (Ast.Lower "f"), [ Ast.EVar (Ast.Lower "a") ]))
  in
  let e2 =
    Ast.make_forall
      [ mk "b" ]
      []
      (Ast.EApp (Ast.EVar (Ast.Lower "f"), [ Ast.EVar (Ast.Lower "b") ]))
  in
  let subst = [ ("f", Ast.EVar (Ast.Lower "g")) ] in
  let r1 = Smt.substitute_vars subst e1 in
  let r2 = Smt.substitute_vars subst e2 in
  (* Patch 4 asserts alpha-equivalence via Binder.Mbinder.equal. Pre-migration
     there is no alpha-aware equality on [Ast.expr]; the closest surrogate is
     free-variable equality, which should hold if capture is avoided. *)
  check bool "alpha-equivalent inputs produce equal free-variable sets" true
    (Smt.StringSet.equal (Smt.free_vars r1) (Smt.free_vars r2))

(* ------------------------------------------------------------------ *)
(* Test 4: free_vars over the regression fixture corpus agrees with
   the legacy StringSet-based implementation. The legacy result is the
   frozen baseline; Patch 4 compares the library-backed implementation
   against it fixture-by-fixture.                                       *)
(* ------------------------------------------------------------------ *)

let regression_dir =
  Test_util.find_dir
    [
      "test/regression";
      "../test/regression";
      "../../test/regression";
      "regression";
      "../regression";
      Filename.concat (Sys.getcwd ()) "test/regression";
    ]

(** Legacy free-variable set baseline: for each body proposition in each chapter
    of [doc], pair the proposition with the result of the current
    [Smt.free_vars]. Patch 4 recomputes this with the library-backed
    implementation and checks agreement. *)
let legacy_free_vars_baseline (doc : Ast.document) :
    (Ast.expr * Smt.StringSet.t) list =
  List.concat_map
    (fun (ch : Ast.chapter) ->
      List.map
        (fun (p : Ast.expr Ast.located) -> (p.value, Smt.free_vars p.value))
        ch.body)
    doc.chapters

let test_free_vars_matches_legacy () =
  pending ();
  match regression_dir with
  | None -> ()
  | Some dir ->
      let fixtures = Test_util.pant_files dir in
      List.iter
        (fun name ->
          let path = Filename.concat dir name in
          let doc = Test_util.parse_pant_file path in
          let baseline = legacy_free_vars_baseline doc in
          (* Patch 4: compute the same list using the library-backed
             free-variable function and assert set equality fixture-by-fixture.
             Today both sides call the same function, so the check is trivially
             true — and skipped by [pending ()] regardless. *)
          List.iter
            (fun (e, legacy) ->
              check bool
                (Printf.sprintf "free_vars agrees with legacy for %s" name)
                true
                (Smt.StringSet.equal legacy (Smt.free_vars e)))
            baseline)
        fixtures

(* ------------------------------------------------------------------ *)
(* Test registration                                                    *)
(* ------------------------------------------------------------------ *)

let () =
  run "Smt_substitution"
    [
      ( "capture_avoidance",
        [
          test_case "substitute_vars is identity on non-free names" `Quick
            test_substitute_identity_on_nonfree;
          test_case "substitute_vars avoids capture in (all y:T | x) case"
            `Quick test_substitute_avoids_capture;
          test_case "substitute_vars preserves alpha-equivalence" `Quick
            test_substitute_preserves_alpha_equivalence;
          test_case "free_vars matches legacy implementation" `Quick
            test_free_vars_matches_legacy;
        ] );
    ]
