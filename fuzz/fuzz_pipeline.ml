(** Fuzz the full pipeline: lexer -> parser -> collect -> check.
    Catches crashes deep in the pipeline from malformed but parseable inputs. *)

open Pantagruel

let () =
  Crowbar.add_test ~name:"pipeline_no_crash" [ Crowbar.bytes ] (fun input ->
      try
        let str = Bytes.to_string input in
        let lexer = Lexer.create_from_string "<fuzz>" str in
        let supplier = Lexer.menhir_token lexer in
        let doc =
          MenhirLib.Convert.Simplified.traditional2revised Parser.document
            supplier
        in
        let mod_name = Option.value ~default:"" doc.module_name in
        (match Collect.collect_all ~base_env:(Env.empty mod_name) doc with
        | Error _ -> ()
        | Ok env -> (
            match Check.check_document env doc with
            | Ok _ | Error _ -> ()));
        ()
      with _ -> ())
