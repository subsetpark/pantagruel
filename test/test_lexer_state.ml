(* @archlint.module stateTest
   @archlint.domain pantagruel.lexer-tokens *)

open Alcotest
open Pantagruel

let state_interleavings =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"lexer state keeps independent token streams under interleaving"
         ~count:100 (QCheck2.Gen.list QCheck2.Gen.unit) (fun _ ->
           let left = Lexer.create_from_string "<left>" "module LEFT." in
           let right = Lexer.create_from_string "<right>" "User." in
           let l1 = Lexer.token left in
           let r1 = Lexer.token right in
           let l2 = Lexer.token left in
           let r2 = Lexer.token right in
           let l3 = Lexer.token left in
           let r3 = Lexer.token right in
           Lexer_tokens.keyword_or_lower_ident "module" = Parser.MODULE
           && Lexer_tokens.describe_token l1 = "'module'"
           && Lexer_tokens.string_of_token r1 = "'User'"
           && List.mem Parser.DOT Lexer_tokens.all_tokens
           && l1 = Parser.MODULE
           && l2 = Parser.UPPER_IDENT "LEFT"
           && l3 = Parser.DOT
           && r1 = Parser.UPPER_IDENT "User"
           && r2 = Parser.DOT && r3 = Parser.EOF));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"current lexer drains pending docs from selected stream"
         ~count:100 (QCheck2.Gen.list QCheck2.Gen.unit) (fun _ ->
           let with_doc = Lexer.create_from_string "<doc>" "User." in
           let plain = Lexer.create_from_string "<plain>" "Other." in
           with_doc.pending_docs <- [ [ "hello" ] ];
           Lexer.set_current with_doc;
           let docs = Lexer.get_pending_docs () in
           Lexer.set_current plain;
           docs = [ [ "hello" ] ] && Lexer.get_pending_docs () = []));
  ]

let () = run "Lexer_state" [ ("state_interleavings", state_interleavings) ]
