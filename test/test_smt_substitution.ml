(** Capture-avoiding substitution tests for the Bindlib migration.

    Patch 4 activates the four properties pinned by Patch 2's stubs: identity on
    non-free names, capture avoidance in the [(all y:T | x)] case, substitution
    preservation of alpha-equivalence, and free_vars agreement with the frozen
    baseline shipped from Patch 2. The bodies run against the Bindlib-backed AST
    landed in Patch 3 — quantifiers are built via [Ast.make_forall] and
    structural equality goes through [Ast.equal_expr], which unbinds
    [Binder.Mbinder.t] for alpha-aware comparison. *)

open Alcotest
open Pantagruel

(* ------------------------------------------------------------------ *)
(* Test 1: substitute_vars is the identity when the domain name is not
   free in e.                                                           *)
(* ------------------------------------------------------------------ *)

let expr_testable = testable Ast.pp_expr Ast.equal_expr

let test_substitute_identity_on_nonfree () =
  let e : Ast.expr =
    Ast.EBinop
      ( Ast.OpAnd,
        Ast.EVar (Ast.Lower "a"),
        Ast.EBinop (Ast.OpEq, Ast.EVar (Ast.Lower "b"), Ast.ELitNat 0) )
  in
  let subst = [ ("x", Ast.EVar (Ast.Lower "y")) ] in
  check expr_testable "substitute_vars is identity when name is not free" e
    (Smt.substitute_vars subst e)

(* ------------------------------------------------------------------ *)
(* Test 2: substitute_vars [x -> EVar y] (EForall [y:T] (EVar x))
   must avoid capture — the free [y] introduced by the substitution
   must survive as free in the result (the binder must be alpha-
   renamed away from [y]).                                              *)
(* ------------------------------------------------------------------ *)

let test_substitute_avoids_capture () =
  let t : Ast.type_expr = Ast.TName (Ast.Upper "T") in
  let param_y : Ast.param = { param_name = Ast.Lower "y"; param_type = t } in
  let body_x = Ast.EVar (Ast.Lower "x") in
  let input = Ast.make_forall [ param_y ] [] body_x in
  let subst = [ ("x", Ast.EVar (Ast.Lower "y")) ] in
  let result = Smt.substitute_vars subst input in
  let free = Smt.free_vars result in
  check bool "y is free in the substituted quantifier" true
    (Smt.StringSet.mem "y" free);
  (* Stronger: the result must be alpha-equivalent to [all z:T | y] for any
     fresh binder name [z]. Use a binder name distinct from [y] to avoid
     accidental structural equality. *)
  let param_z : Ast.param = { param_name = Ast.Lower "z"; param_type = t } in
  let expected = Ast.make_forall [ param_z ] [] (Ast.EVar (Ast.Lower "y")) in
  check expr_testable "result is alpha-equivalent to (all z:T | y)" expected
    result

(** Sequential guard binders ([GParam] / [GIn]) also shadow, and need the same
    alpha-rename treatment as top-level quantifier params. Regression for the
    [all | y in ys, x] shape: substituting [x -> EVar y] must keep the
    introduced [y] free rather than capturing it under the [GIn y] binder.

    Asserts the result's shape directly rather than leaning on [free_vars] as an
    oracle: a buggy implementation that left the [GIn] binder as [y] and
    substituted [y] into the body would produce a quantifier that [free_vars]
    correctly reports as not-free-in-y, but the structural check below would
    still fail. *)
let test_substitute_avoids_capture_in_guards () =
  let ys = Ast.EVar (Ast.Lower "ys") in
  let guards : Ast.guard list =
    [ Ast.GIn (Ast.Lower "y", ys); Ast.GExpr (Ast.EVar (Ast.Lower "x")) ]
  in
  let input = Ast.make_forall [] guards (Ast.ELitBool true) in
  let subst = [ ("x", Ast.EVar (Ast.Lower "y")) ] in
  let result = Smt.substitute_vars subst input in
  (match[@warning "-4"] result with
  | Ast.EForall (mb, metas) -> (
      let _params, result_guards, _body = Ast.unbind_quant mb metas in
      match[@warning "-4"] result_guards with
      | [
       Ast.GIn (Ast.Lower bind_name, _);
       Ast.GExpr (Ast.EVar (Ast.Lower body_name));
      ] ->
          check bool "GIn binder is alpha-renamed away from y" true
            (not (String.equal bind_name "y"));
          check string "GExpr references the substituted free y" "y" body_name
      | _ -> fail "unexpected guard shape in substituted result")
  | _ -> fail "result is not an EForall");
  (* free_vars is consistent with the structural check: y stays free. *)
  check bool "y is free in the substituted quantifier" true
    (Smt.StringSet.mem "y" (Smt.free_vars result))

(** Sibling regression for [GParam] (typed) guard binders. Shape:
    [all | y: T, x]. The [GParam y:T] binds [y] for subsequent guards and the
    body; substituting [x -> EVar y] must rename it so the introduced [y] stays
    free. *)
let test_substitute_avoids_capture_in_gparam () =
  let t : Ast.type_expr = Ast.TName (Ast.Upper "T") in
  let guards : Ast.guard list =
    [
      Ast.GParam { param_name = Ast.Lower "y"; param_type = t };
      Ast.GExpr (Ast.EVar (Ast.Lower "x"));
    ]
  in
  let input = Ast.make_forall [] guards (Ast.ELitBool true) in
  let subst = [ ("x", Ast.EVar (Ast.Lower "y")) ] in
  let result = Smt.substitute_vars subst input in
  (match[@warning "-4"] result with
  | Ast.EForall (mb, metas) -> (
      let _params, result_guards, _body = Ast.unbind_quant mb metas in
      match[@warning "-4"] result_guards with
      | [
       Ast.GParam { param_name = Ast.Lower bind_name; _ };
       Ast.GExpr (Ast.EVar (Ast.Lower body_name));
      ] ->
          check bool "GParam binder is alpha-renamed away from y" true
            (not (String.equal bind_name "y"));
          check string "GExpr references the substituted free y" "y" body_name
      | _ -> fail "unexpected guard shape in substituted result")
  | _ -> fail "result is not an EForall");
  check bool "y is free in the substituted quantifier" true
    (Smt.StringSet.mem "y" (Smt.free_vars result))

(* ------------------------------------------------------------------ *)
(* Test 3: alpha-equivalent inputs produce alpha-equivalent outputs
   under substitute_vars. Smoke check on a hand-built pair; then a
   QCheck generator pairs two quantifiers with renamed binders and
   asserts alpha-equivalence is preserved across substitution.          *)
(* ------------------------------------------------------------------ *)

let test_substitute_preserves_alpha_equivalence () =
  let t : Ast.type_expr = Ast.TName (Ast.Upper "T") in
  let mk name : Ast.param = { param_name = Ast.Lower name; param_type = t } in
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
  check expr_testable "alpha-equivalent inputs are structurally equal" e1 e2;
  let subst = [ ("f", Ast.EVar (Ast.Lower "g")) ] in
  let r1 = Smt.substitute_vars subst e1 in
  let r2 = Smt.substitute_vars subst e2 in
  check expr_testable "substitute_vars preserves alpha-equivalence" r1 r2

(** QCheck property: pairs of alpha-equivalent quantifiers produce
    alpha-equivalent results under a caller-chosen substitution. *)
let prop_alpha_equiv_preserved =
  let gen_names = QCheck.Gen.oneof_list [ "a"; "b"; "c"; "p"; "q"; "r" ] in
  let gen_fresh_pair =
    QCheck.Gen.map
      (fun (n1, n2) -> if n1 = n2 then (n1, n1 ^ "_alt") else (n1, n2))
      (QCheck.Gen.pair gen_names gen_names)
  in
  let gen_body_ref name =
    QCheck.Gen.oneof
      [
        QCheck.Gen.return (Ast.EVar (Ast.Lower name));
        QCheck.Gen.return
          (Ast.EApp (Ast.EVar (Ast.Lower "f"), [ Ast.EVar (Ast.Lower name) ]));
        QCheck.Gen.return
          (Ast.EBinop (Ast.OpEq, Ast.EVar (Ast.Lower name), Ast.ELitNat 0));
      ]
  in
  let t : Ast.type_expr = Ast.TName (Ast.Upper "T") in
  let gen =
    QCheck.Gen.bind gen_fresh_pair (fun (n1, n2) ->
        QCheck.Gen.map
          (fun body1 ->
            let body2 =
              Smt.substitute_vars [ (n1, Ast.EVar (Ast.Lower n2)) ] body1
            in
            let p1 : Ast.param =
              { param_name = Ast.Lower n1; param_type = t }
            in
            let p2 : Ast.param =
              { param_name = Ast.Lower n2; param_type = t }
            in
            let e1 = Ast.make_forall [ p1 ] [] body1 in
            let e2 = Ast.make_forall [ p2 ] [] body2 in
            let subst = [ ("f", Ast.EVar (Ast.Lower "g")) ] in
            (e1, e2, subst))
          (gen_body_ref n1))
  in
  let arb =
    QCheck.make
      ~print:(fun (e1, e2, subst) ->
        Printf.sprintf "(%s, %s, [%s])" (Ast.show_expr e1) (Ast.show_expr e2)
          (String.concat "; "
             (List.map
                (fun (k, v) -> Printf.sprintf "%s -> %s" k (Ast.show_expr v))
                subst)))
      gen
  in
  QCheck.Test.make ~count:100 ~name:"substitute preserves alpha-equivalence" arb
    (fun (e1, e2, subst) ->
      let r1 = Smt.substitute_vars subst e1 in
      let r2 = Smt.substitute_vars subst e2 in
      Ast.equal_expr r1 r2)

(* ------------------------------------------------------------------ *)
(* Test 4: free_vars over the regression fixture corpus agrees with
   the frozen baseline shipped alongside this test.                     *)
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

let baseline_filename = "free_vars.baseline"

(** Serialise one fixture's free-variable sets into newline-separated lines:

    [fixture\tchapter_idx\tprop_idx\tv1,v2,...]

    Empty sets produce an empty fourth field. Names are sorted to make the
    baseline independent of [StringSet]'s internal order. Chapter / prop indices
    are the positional index within [doc.chapters] and the chapter's [body],
    respectively. *)
let serialise_fixture ~name (doc : Ast.document) : string list =
  List.concat
    (List.mapi
       (fun ci (ch : Ast.chapter) ->
         List.mapi
           (fun pi (p : Ast.expr Ast.located) ->
             let vars = Smt.free_vars p.value |> Smt.StringSet.elements in
             Printf.sprintf "%s\t%d\t%d\t%s" name ci pi (String.concat "," vars))
           ch.body)
       doc.chapters)

let regen_env = "PANT_REGEN_BASELINE"

let regen_enabled () =
  match Sys.getenv_opt regen_env with
  | Some "1" | Some "true" -> true
  | _ -> false

let read_lines path =
  let ch = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let rec loop acc =
        match input_line ch with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let write_lines path lines =
  let ch = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr ch)
    (fun () -> List.iter (fun l -> output_string ch (l ^ "\n")) lines)

let test_free_vars_matches_legacy () =
  match regression_dir with
  | None -> fail "regression directory not found"
  | Some dir ->
      let fixtures = Test_util.pant_files dir in
      let actual =
        List.concat_map
          (fun name ->
            let path = Filename.concat dir name in
            let doc = Test_util.parse_pant_file path in
            serialise_fixture ~name doc)
          fixtures
      in
      let baseline_path = Filename.concat dir baseline_filename in
      if regen_enabled () then begin
        write_lines baseline_path actual;
        Printf.printf "[regen] wrote %s (%d lines)\n" baseline_path
          (List.length actual)
      end
      else
        let expected = read_lines baseline_path in
        check (list string)
          (Printf.sprintf "free_vars matches baseline (%s)" baseline_filename)
          expected actual

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
          test_case "substitute_vars avoids capture across GIn guard binders"
            `Quick test_substitute_avoids_capture_in_guards;
          test_case "substitute_vars avoids capture across GParam guard binders"
            `Quick test_substitute_avoids_capture_in_gparam;
          test_case "substitute_vars preserves alpha-equivalence" `Quick
            test_substitute_preserves_alpha_equivalence;
          QCheck_alcotest.to_alcotest prop_alpha_equiv_preserved;
          test_case "free_vars matches legacy implementation" `Quick
            test_free_vars_matches_legacy;
        ] );
    ]
