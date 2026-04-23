(** Layer 2 SMT translation tests: property-based checks on randomly generated
    Pantagruel documents.

    Strategy: generate small documents via [Test_util.gen_document], filter via
    [QCheck2.assume] to those that type-check, then run the same structural
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

(** [accepted doc] returns the queries iff [doc] type-checks. Discards via
    [QCheck2.assume_fail] otherwise. *)
let accepted doc : Smt.query list =
  match Test_util.translate_to_queries doc with
  | Error _ -> QCheck2.assume_fail ()
  | Ok qs -> qs

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
    (QCheck2.Test.make ~name ~count ~print:pp_doc Test_util.gen_document prop)

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
