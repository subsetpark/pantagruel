(** Fuzz the lexer: feed arbitrary bytes and ensure no crashes. Only Lexer_error
    and Sedlexing.MalFormed are acceptable exceptions. *)

open Pantagruel

let () =
  Crowbar.add_test ~name:"lexer_no_crash" [ Crowbar.bytes ] (fun input ->
      try
        let lexer = Lexer.create_from_string "<fuzz>" input in
        let rec drain () =
          match[@warning "-4"] Lexer.token lexer with
          | Parser.EOF -> ()
          | _ -> drain ()
        in
        drain ()
      with Lexer.Lexer_error _ | Sedlexing.MalFormed -> ())
