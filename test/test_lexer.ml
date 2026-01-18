(** Lexer tests *)

open Alcotest
open Pantagruel

(* Simple token comparison - just check constructor name for now *)
let token_to_string = function
  | Parser.MODULE -> "MODULE"
  | Parser.IMPORT -> "IMPORT"
  | Parser.WHERE -> "WHERE"
  | Parser.TRUE -> "TRUE"
  | Parser.FALSE -> "FALSE"
  | Parser.UPPER_IDENT s -> "UPPER_IDENT(" ^ s ^ ")"
  | Parser.LOWER_IDENT s -> "LOWER_IDENT(" ^ s ^ ")"
  | Parser.NAT n -> "NAT(" ^ string_of_int n ^ ")"
  | Parser.REAL r -> "REAL(" ^ string_of_float r ^ ")"
  | Parser.STRING s -> "STRING(" ^ s ^ ")"
  | Parser.DARROW -> "DARROW"
  | Parser.ARROW -> "ARROW"
  | Parser.IFF -> "IFF"
  | Parser.EQ -> "EQ"
  | Parser.NEQ -> "NEQ"
  | Parser.LT -> "LT"
  | Parser.GT -> "GT"
  | Parser.LE -> "LE"
  | Parser.GE -> "GE"
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.TIMES -> "TIMES"
  | Parser.DIVIDE -> "DIVIDE"
  | Parser.CARD -> "CARD"
  | Parser.PRIME -> "PRIME"
  | Parser.MAPSTO -> "MAPSTO"
  | Parser.PROJ n -> "PROJ(" ^ string_of_int n ^ ")"
  | Parser.AND -> "AND"
  | Parser.OR -> "OR"
  | Parser.NOT -> "NOT"
  | Parser.FORALL -> "FORALL"
  | Parser.EXISTS -> "EXISTS"
  | Parser.IN -> "IN"
  | Parser.SUBSET -> "SUBSET"
  | Parser.DOT -> "DOT"
  | Parser.COMMA -> "COMMA"
  | Parser.COLON -> "COLON"
  | Parser.DCOLON -> "DCOLON"
  | Parser.PIPE -> "PIPE"
  | Parser.SEPARATOR -> "SEPARATOR"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACKET -> "LBRACKET"
  | Parser.RBRACKET -> "RBRACKET"
  | Parser.EOF -> "EOF"

let token_testable = testable (fun fmt t -> Format.pp_print_string fmt (token_to_string t))
  (fun a b -> token_to_string a = token_to_string b)

let lex_all str =
  let lexer = Lexer.create_from_string "<test>" str in
  let rec go acc =
    match Lexer.token lexer with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | tok -> go (tok :: acc)
  in
  go []

let test_keywords () =
  check (list token_testable) "keywords"
    [Parser.MODULE; Parser.IMPORT; Parser.WHERE; Parser.TRUE; Parser.FALSE; Parser.EOF]
    (lex_all "module import where true false")

let test_identifiers () =
  check (list token_testable) "identifiers"
    [Parser.UPPER_IDENT "User"; Parser.LOWER_IDENT "owner";
     Parser.LOWER_IDENT "has-perm?"; Parser.LOWER_IDENT "check-out!"; Parser.EOF]
    (lex_all "User owner has-perm? check-out!")

let test_numbers () =
  check (list token_testable) "numbers"
    [Parser.NAT 0; Parser.NAT 42; Parser.REAL 3.14; Parser.EOF]
    (lex_all "0 42 3.14")

let test_operators () =
  check (list token_testable) "operators"
    [Parser.DARROW; Parser.ARROW; Parser.EQ; Parser.NEQ;
     Parser.LE; Parser.GE; Parser.LT; Parser.GT; Parser.EOF]
    (lex_all "=> -> = != <= >= < >")

let test_logical () =
  check (list token_testable) "logical"
    [Parser.AND; Parser.OR; Parser.NOT; Parser.FORALL; Parser.EXISTS;
     Parser.IN; Parser.SUBSET; Parser.EOF]
    (lex_all "and or not all some in subset")

let test_punctuation () =
  check (list token_testable) "punctuation"
    [Parser.DOT; Parser.COMMA; Parser.COLON; Parser.PIPE;
     Parser.SEPARATOR; Parser.LPAREN; Parser.RPAREN;
     Parser.LBRACKET; Parser.RBRACKET; Parser.EOF]
    (lex_all ". , : | --- ( ) [ ]")

let test_string () =
  check (list token_testable) "string"
    [Parser.STRING "hello world"; Parser.EOF]
    (lex_all "\"hello world\"")

let test_unicode_operators () =
  (* Test Unicode equivalents *)
  check (list token_testable) "unicode darrow"
    [Parser.DARROW; Parser.EOF]
    (lex_all "\xE2\x87\x92");  (* U+21D2 *)
  check (list token_testable) "unicode arrow"
    [Parser.ARROW; Parser.EOF]
    (lex_all "\xE2\x86\x92");  (* U+2192 *)
  check (list token_testable) "unicode and"
    [Parser.AND; Parser.EOF]
    (lex_all "\xE2\x88\xA7");  (* U+2227 *)
  check (list token_testable) "unicode forall"
    [Parser.FORALL; Parser.EOF]
    (lex_all "\xE2\x88\x80")   (* U+2200 *)

let test_comments () =
  check (list token_testable) "line comment"
    [Parser.UPPER_IDENT "Foo"; Parser.DOT; Parser.EOF]
    (lex_all "Foo. // this is a comment\n")

let test_doc_comments () =
  (* Doc comments starting with > at beginning of line are captured, not returned as tokens *)
  check (list token_testable) "doc comment followed by decl"
    [Parser.UPPER_IDENT "Foo"; Parser.DOT; Parser.EOF]
    (lex_all "> This is a doc comment\nFoo.")

let test_projection_token () =
  check (list token_testable) "projection"
    [Parser.LOWER_IDENT "p"; Parser.PROJ 1; Parser.PROJ 2; Parser.EOF]
    (lex_all "p.1.2")

let test_dcolon () =
  check (list token_testable) "double colon"
    [Parser.UPPER_IDENT "Module"; Parser.DCOLON; Parser.LOWER_IDENT "name"; Parser.EOF]
    (lex_all "Module::name")

let () =
  run "Lexer" [
    "keywords", [test_case "keywords" `Quick test_keywords];
    "identifiers", [test_case "identifiers" `Quick test_identifiers];
    "numbers", [test_case "numbers" `Quick test_numbers];
    "operators", [test_case "operators" `Quick test_operators];
    "logical", [test_case "logical" `Quick test_logical];
    "punctuation", [test_case "punctuation" `Quick test_punctuation];
    "string", [test_case "string" `Quick test_string];
    "unicode", [test_case "unicode operators" `Quick test_unicode_operators];
    "comments", [test_case "comments" `Quick test_comments];
    "doc_comments", [test_case "doc comments" `Quick test_doc_comments];
    "projection", [test_case "projection" `Quick test_projection_token];
    "dcolon", [test_case "double colon" `Quick test_dcolon];
  ]
