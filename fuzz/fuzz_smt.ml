(** Fuzz the Pantagruel→SMT translator end-to-end. Catches:

    - Crashes in the translator on type-checking input (via the bare
      [try...with]).
    - Malformed emitted SMT (via [Smt_check.parse_smt2]).
    - Unbounded fallback emissions of new (unknown) kinds — the known set
      mirrors the one in [test_smt_property.ml].

    Mirrors [fuzz_pipeline.ml]: feed [Crowbar.bytes] through the parser, run
    [Collect.collect_all] and [Check.check_document], and only assert invariants
    if those succeeded. Parse/collect errors don't count as finds; only
    translator-stage crashes or malformed SMT do. *)

open Pantagruel

(* Same allowlist as test_smt_property.ml. Keep these in sync; once a bug is
   fixed, drop the corresponding kinds from BOTH lists. *)
let kinds_pending_fix =
  [ "duplicate_binder"; "vacuous_binder"; "fallback_emission" ]

let translate_and_check (doc : Ast.document) =
  let mod_name = Option.fold ~none:"" ~some:Ast.upper_name doc.module_name in
  match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
  | Error _ -> ()
  | Ok env -> (
      match Check.check_document env doc with
      | Error _ -> ()
      | Ok _ ->
          let domain_bounds = Smt.compute_domain_bounds 3 env in
          let config =
            Smt.make_config ~bound:3 ~steps:1 ~domain_bounds ~inject_guards:true
          in
          let queries = Smt.generate_queries config env doc in
          List.iter
            (fun (q : Smt.query) ->
              (* Well-formedness: must parse as sexps. *)
              (match Smt_check.parse_smt2 q.smt2 with
              | Ok _ -> ()
              | Error msg ->
                  Crowbar.failf "malformed SMT in query %S: %s" q.name msg);
              (* Structural failures: only the allowlisted kinds are
                 tolerated. *)
              List.iter
                (fun (f : Smt_check.failure) ->
                  let tag = Smt_check.failure_kind_tag f.kind in
                  if not (List.mem tag kinds_pending_fix) then
                    Crowbar.failf "unexpected SMT failure in %S: %s" q.name
                      (Smt_check.format_failure f))
                (Smt_check.check_query q.smt2))
            queries)

let () =
  Crowbar.add_test ~name:"smt_no_crash_or_malformed" [ Crowbar.bytes ]
    (fun input ->
      try
        let lexer = Lexer.create_from_string "<fuzz>" input in
        let supplier = Lexer.menhir_token lexer in
        let doc =
          MenhirLib.Convert.Simplified.traditional2revised Parser.document
            supplier
        in
        translate_and_check doc
      with _ -> ())
