(** Layer 2 SMT translation tests: property-based checks on randomly generated
    Pantagruel documents.

    Strategy: generate small documents via [Test_util.gen_document], filter via
    [QCheck.assume] to those that type-check, then run the same structural
    invariants as Layer 1 ([Smt_check]) on the emitted SMT. Properties:
    - translation totality (no exception)
    - well-formed SMT (parses as sexps)
    - no structural failures except those in the kind-level allowlist below
      (mirroring the per-fixture allowlist used in Layer 1)

    The allowlist is a set of failure kinds that are tolerated globally until
    the corresponding translator bug is fixed. When a tracked bug is fixed,
    remove its kind(s) here. *)

open Pantagruel

(** Failure kinds Layer 2 currently tolerates. Empty: all generated documents
    must satisfy every structural invariant. Re-add a tag here only when a new
    known-pending translator bug appears, with a comment linking to it. *)
let kinds_pending_fix : string list = []

let pp_doc = Test_util.print_document

(** Run the canonical translation pipeline for one generated document. *)
let translate (doc : Ast.document) : (Smt.query list, string) result =
  let mod_name = Option.fold ~none:"" ~some:Ast.upper_name doc.module_name in
  match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
  | Error e -> Error (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Error e -> Error (Check.show_type_error e)
      | Ok _ ->
          let domain_bounds = Smt.compute_domain_bounds 3 env in
          let config =
            Smt.make_config ~bound:3 ~steps:1 ~domain_bounds ~inject_guards:true
          in
          Ok (Smt.generate_queries config env doc))

(** [accepted doc] returns the queries iff [doc] type-checks. Discards via
    [QCheck.assume_fail] otherwise. *)
let accepted doc : Smt.query list =
  match translate doc with Error _ -> QCheck.assume_fail () | Ok qs -> qs

(* ------------------------------------------------------------------ *)
(* Properties                                                           *)
(* ------------------------------------------------------------------ *)

(** [Smt.generate_queries] doesn't raise on any type-checking input. *)
let prop_translation_totality doc =
  let _ = accepted doc in
  true

(** Every emitted query is well-formed SMT-LIB2 (parses as sexps). *)
let prop_well_formed doc =
  let qs = accepted doc in
  List.for_all
    (fun (q : Smt.query) ->
      match Smt_check.parse_smt2 q.smt2 with Ok _ -> true | Error _ -> false)
    qs

(** No emitted query has a structural failure outside [kinds_pending_fix]. When
    the pending list is empty, this is the strict "no failures at all" property.
*)
let prop_no_unexpected_failures doc =
  let qs = accepted doc in
  List.for_all
    (fun (q : Smt.query) ->
      Smt_check.check_query q.smt2
      |> List.for_all (fun (f : Smt_check.failure) ->
          List.mem (Smt_check.failure_kind_tag f.kind) kinds_pending_fix))
    qs

(* ------------------------------------------------------------------ *)
(* Suite assembly                                                       *)
(* ------------------------------------------------------------------ *)

let count = 200

let mk_property ~name ~prop =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make ~name ~count
       (QCheck.set_print pp_doc Test_util.arb_document)
       prop)

let () =
  Alcotest.run "SmtProperty"
    [
      ( "translation",
        [
          mk_property ~name:"totality" ~prop:prop_translation_totality;
          mk_property ~name:"well-formed SMT" ~prop:prop_well_formed;
          mk_property ~name:"no unexpected structural failures"
            ~prop:prop_no_unexpected_failures;
        ] );
    ]
