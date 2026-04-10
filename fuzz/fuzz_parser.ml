(** Fuzz the parser: feed arbitrary bytes through lexer+parser. Any exception is
    acceptable (parse errors are expected); only unhandled crashes indicate
    bugs. *)

open Pantagruel

let () =
  Crowbar.add_test ~name:"parser_no_crash" [ Crowbar.bytes ] (fun input ->
      try
        let lexer = Lexer.create_from_string "<fuzz>" input in
        let supplier = Lexer.menhir_token lexer in
        ignore
          (MenhirLib.Convert.Simplified.traditional2revised Parser.document
             supplier)
      with _ -> ())
