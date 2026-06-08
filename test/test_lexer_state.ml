(* @archlint.module stateTest
   @archlint.domain pantagruel.lexer-tokens *)

open Alcotest
open Pantagruel

type stream_op = Left | Right

let stream_op_gen = QCheck2.Gen.oneof_list [ Left; Right ]

let lex_all source =
  let lexer = Lexer.create_from_string "<expected>" source in
  let rec loop acc =
    match[@warning "-4"] Lexer.token lexer with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | tok -> loop (tok :: acc)
  in
  loop []

let stream_matches expected observed =
  let rec loop expected observed =
    match[@warning "-4"] (expected, observed) with
    | _, [] -> true
    | [ Parser.EOF ], got :: got_rest when got = Parser.EOF ->
        loop [ Parser.EOF ] got_rest
    | exp :: exp_rest, got :: got_rest -> exp = got && loop exp_rest got_rest
    | [], _ :: _ -> false
  in
  loop expected observed

let state_operation_sequences =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"lexer state keeps independent token streams under interleaving"
         ~count:100 (QCheck2.Gen.list stream_op_gen) (fun ops ->
           let left = Lexer.create_from_string "<left>" "module LEFT." in
           let right = Lexer.create_from_string "<right>" "User." in
           let left_tokens, right_tokens =
             List.fold_left
               (fun (left_seen, right_seen) op ->
                 match op with
                 | Left -> (Lexer.token left :: left_seen, right_seen)
                 | Right -> (left_seen, Lexer.token right :: right_seen))
               ([], []) ops
           in
           let left_tokens = List.rev left_tokens in
           let right_tokens = List.rev right_tokens in
           Lexer_tokens.keyword_or_lower_ident "module" = Parser.MODULE
           && Lexer_tokens.describe_token Parser.MODULE = "'module'"
           && Lexer_tokens.string_of_token (Parser.UPPER_IDENT "User")
              = "'User'"
           && List.mem Parser.DOT Lexer_tokens.all_tokens
           && stream_matches (lex_all "module LEFT.") left_tokens
           && stream_matches (lex_all "User.") right_tokens));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"current lexer drains pending docs from selected stream"
         ~count:100 (QCheck2.Gen.list stream_op_gen) (fun ops ->
           let with_doc = Lexer.create_from_string "<doc>" "User." in
           let plain = Lexer.create_from_string "<plain>" "Other." in
           with_doc.pending_docs <- [ [ "hello" ] ];
           let doc_reads, plain_reads =
             List.fold_left
               (fun (doc_reads, plain_reads) op ->
                 match op with
                 | Left ->
                     Lexer.set_current with_doc;
                     (Lexer.get_pending_docs () :: doc_reads, plain_reads)
                 | Right ->
                     Lexer.set_current plain;
                     (doc_reads, Lexer.get_pending_docs () :: plain_reads))
               ([], []) ops
           in
           let doc_reads = List.rev doc_reads in
           let plain_reads = List.rev plain_reads in
           let doc_payloads = List.filter (( <> ) []) doc_reads in
           let plain_payloads = List.filter (( <> ) []) plain_reads in
           List.length doc_payloads <= 1
           && List.for_all (( = ) [ [ "hello" ] ]) doc_payloads
           && plain_payloads = []
           &&
           if List.mem Left ops then true
           else (
             Lexer.set_current with_doc;
             Lexer.get_pending_docs () = [ [ "hello" ] ])));
  ]

let () =
  run "Lexer_state" [ ("state_operation_sequences", state_operation_sequences) ]
