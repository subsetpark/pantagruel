(* @archlint.module test
   @archlint.domain pantagruel.lexer-tokens *)

open Alcotest
open Pantagruel

let token_display_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"lexer token helpers classify and display tokens"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Lexer_tokens.keyword_or_lower_ident "module" = Parser.MODULE
           && Lexer_tokens.keyword_or_lower_ident "owner"
              = Parser.LOWER_IDENT "owner"
           && Lexer_tokens.string_of_token (Parser.LOWER_IDENT "owner")
              = "'owner'"
           && Lexer_tokens.describe_token (Parser.LOWER_IDENT "owner")
              = "identifier"
           && List.mem Parser.MODULE Lexer_tokens.all_tokens));
  ]

let () = run "Lexer_tokens_core" [ ("tokens", token_display_properties) ]
