(* @archlint.module test
   @archlint.domain pantagruel.lexer-tokens *)

open Alcotest
open Pantagruel

let gen_lower =
  QCheck2.Gen.map
    (fun s -> "x" ^ s)
    (QCheck2.Gen.string_size
       ~gen:QCheck2.Gen.(char_range 'a' 'z')
       (QCheck2.Gen.int_range 1 10))

let token_display_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"lexer token helpers classify and display tokens"
         ~count:100 gen_lower (fun ident ->
           Lexer_tokens.keyword_or_lower_ident "module" = Parser.MODULE
           && Lexer_tokens.keyword_or_lower_ident ident
              = Parser.LOWER_IDENT ident
           && Lexer_tokens.string_of_token (Parser.LOWER_IDENT ident)
              = "'" ^ ident ^ "'"
           && Lexer_tokens.describe_token (Parser.LOWER_IDENT "owner")
              = "identifier"
           && List.mem Parser.MODULE Lexer_tokens.all_tokens));
  ]

let () = run "Lexer_tokens_core" [ ("tokens", token_display_properties) ]
