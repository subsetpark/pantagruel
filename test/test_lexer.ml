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
  | Parser.EACH -> "EACH"
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
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.SQUIG_ARROW -> "SQUIG_ARROW"
  | Parser.CONTEXT -> "CONTEXT"
  | Parser.INITIALLY -> "INITIALLY"
  | Parser.CLOSURE -> "CLOSURE"
  | Parser.COND -> "COND"
  | Parser.OVER -> "OVER"
  | Parser.MIN -> "MIN"
  | Parser.MAX -> "MAX"
  | Parser.CHECK -> "CHECK"
  | Parser.AT -> "AT"
  | Parser.ACTION_LABEL s -> "ACTION_LABEL(" ^ s ^ ")"
  | Parser.EOF -> "EOF"

let token_testable =
  testable
    (fun fmt t -> Format.pp_print_string fmt (token_to_string t))
    (fun a b -> token_to_string a = token_to_string b)

let lex_all str =
  let lexer = Lexer.create_from_string "<test>" str in
  let rec go acc =
    match[@warning "-4"] Lexer.token lexer with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | tok -> go (tok :: acc)
  in
  go []

let test_keywords () =
  check (list token_testable) "keywords"
    [
      Parser.MODULE;
      Parser.IMPORT;
      Parser.WHERE;
      Parser.CHECK;
      Parser.TRUE;
      Parser.FALSE;
      Parser.EOF;
    ]
    (lex_all "module import where check true false")

let test_identifiers () =
  check (list token_testable) "identifiers"
    [
      Parser.UPPER_IDENT "User";
      Parser.LOWER_IDENT "owner";
      Parser.LOWER_IDENT "has-perm?";
      Parser.LOWER_IDENT "check-out!";
      Parser.EOF;
    ]
    (lex_all "User owner has-perm? check-out!")

let test_numbers () =
  check (list token_testable) "numbers"
    [ Parser.NAT 0; Parser.NAT 42; Parser.REAL 3.14; Parser.EOF ]
    (lex_all "0 42 3.14")

let test_operators () =
  check (list token_testable) "operators"
    [
      Parser.DARROW;
      Parser.ARROW;
      Parser.EQ;
      Parser.NEQ;
      Parser.LE;
      Parser.GE;
      Parser.LT;
      Parser.GT;
      Parser.EOF;
    ]
    (lex_all "=> -> = != <= >= < >");
  check (list token_testable) "~= is NEQ" [ Parser.NEQ; Parser.EOF ]
    (lex_all "~=")

let test_logical () =
  check (list token_testable) "logical"
    [
      Parser.AND;
      Parser.OR;
      Parser.NOT;
      Parser.FORALL;
      Parser.EXISTS;
      Parser.IN;
      Parser.SUBSET;
      Parser.EOF;
    ]
    (lex_all "and or ~ all some in subset")

let test_punctuation () =
  check (list token_testable) "punctuation"
    [
      Parser.DOT;
      Parser.COMMA;
      Parser.COLON;
      Parser.PIPE;
      Parser.SEPARATOR;
      Parser.LPAREN;
      Parser.RPAREN;
      Parser.LBRACKET;
      Parser.RBRACKET;
      Parser.EOF;
    ]
    (lex_all ". , : | --- ( ) [ ]")

let test_string () =
  check (list token_testable) "string"
    [ Parser.STRING "hello world"; Parser.EOF ]
    (lex_all "\"hello world\"")

let test_comments () =
  check (list token_testable) "line comment"
    [ Parser.UPPER_IDENT "Foo"; Parser.DOT; Parser.EOF ]
    (lex_all "Foo. // this is a comment\n")

let test_doc_comments () =
  (* Doc comments starting with > at beginning of line are captured, not returned as tokens *)
  check (list token_testable) "doc comment followed by decl"
    [ Parser.UPPER_IDENT "Foo"; Parser.DOT; Parser.EOF ]
    (lex_all "> This is a doc comment\nFoo.")

let test_projection_token () =
  check (list token_testable) "projection"
    [ Parser.LOWER_IDENT "p"; Parser.PROJ 1; Parser.PROJ 2; Parser.EOF ]
    (lex_all "p.1.2")

let test_dcolon () =
  check (list token_testable) "double colon"
    [
      Parser.UPPER_IDENT "Module";
      Parser.DCOLON;
      Parser.LOWER_IDENT "name";
      Parser.EOF;
    ]
    (lex_all "Module::name")

let test_squig_arrow () =
  check (list token_testable) "squig arrow with label"
    [
      Parser.SQUIG_ARROW;
      Parser.ACTION_LABEL "Withdraw";
      Parser.AT;
      Parser.LOWER_IDENT "a";
      Parser.COLON;
      Parser.UPPER_IDENT "Account";
      Parser.DOT;
      Parser.EOF;
    ]
    (lex_all "~> Withdraw @ a: Account.");
  check (list token_testable) "squig arrow with context"
    [
      Parser.UPPER_IDENT "Banking";
      Parser.SQUIG_ARROW;
      Parser.ACTION_LABEL "Withdraw";
      Parser.DOT;
      Parser.EOF;
    ]
    (lex_all "Banking ~> Withdraw.");
  check (list token_testable) "squig arrow label with spaces"
    [
      Parser.SQUIG_ARROW;
      Parser.ACTION_LABEL "Check out a book";
      Parser.AT;
      Parser.LOWER_IDENT "u";
      Parser.COLON;
      Parser.UPPER_IDENT "User";
      Parser.DOT;
      Parser.EOF;
    ]
    (lex_all "~> Check out a book @ u: User.")

let test_iff_token () =
  check (list token_testable) "iff" [ Parser.IFF; Parser.EOF ] (lex_all "<->")

let test_mapsto_token () =
  check (list token_testable) "mapsto"
    [ Parser.MAPSTO; Parser.EOF ]
    (lex_all "|->")

let test_arithmetic_tokens () =
  check (list token_testable) "arithmetic"
    [ Parser.PLUS; Parser.MINUS; Parser.TIMES; Parser.DIVIDE; Parser.EOF ]
    (lex_all "+ - * /")

let test_card_token () =
  check (list token_testable) "card"
    [ Parser.CARD; Parser.UPPER_IDENT "User"; Parser.EOF ]
    (lex_all "#User")

let test_prime_token () =
  check (list token_testable) "prime"
    [ Parser.LOWER_IDENT "f"; Parser.PRIME; Parser.EOF ]
    (lex_all "f'")

let test_braces () =
  check (list token_testable) "braces"
    [ Parser.LBRACE; Parser.UPPER_IDENT "Ctx"; Parser.RBRACE; Parser.EOF ]
    (lex_all "{Ctx}")

let test_context_keyword () =
  check (list token_testable) "context"
    [ Parser.CONTEXT; Parser.UPPER_IDENT "Banking"; Parser.DOT; Parser.EOF ]
    (lex_all "context Banking.")

let test_initially_keyword () =
  check (list token_testable) "initially"
    [ Parser.INITIALLY; Parser.LOWER_IDENT "x"; Parser.EOF ]
    (lex_all "initially x")

let test_closure_keyword () =
  check (list token_testable) "closure"
    [ Parser.CLOSURE; Parser.LOWER_IDENT "parent"; Parser.EOF ]
    (lex_all "closure parent")

let test_each_keyword () =
  check (list token_testable) "each"
    [ Parser.EACH; Parser.LOWER_IDENT "u"; Parser.EOF ]
    (lex_all "each u")

let test_cond_keyword () =
  check (list token_testable) "cond"
    [ Parser.COND; Parser.TRUE; Parser.DARROW; Parser.NAT 1; Parser.EOF ]
    (lex_all "cond true => 1")

let test_over_each_tokens () =
  check (list token_testable) "+ over each"
    [ Parser.PLUS; Parser.OVER; Parser.EACH; Parser.EOF ]
    (lex_all "+ over each");
  check (list token_testable) "* over each"
    [ Parser.TIMES; Parser.OVER; Parser.EACH; Parser.EOF ]
    (lex_all "* over each");
  check (list token_testable) "and over each"
    [ Parser.AND; Parser.OVER; Parser.EACH; Parser.EOF ]
    (lex_all "and over each");
  check (list token_testable) "or over each"
    [ Parser.OR; Parser.OVER; Parser.EACH; Parser.EOF ]
    (lex_all "or over each");
  check (list token_testable) "min over each"
    [ Parser.MIN; Parser.OVER; Parser.EACH; Parser.EOF ]
    (lex_all "min over each");
  check (list token_testable) "max over each"
    [ Parser.MAX; Parser.OVER; Parser.EACH; Parser.EOF ]
    (lex_all "max over each")

let test_string_escapes () =
  check (list token_testable) "string escapes"
    [ Parser.STRING "line1\nline2"; Parser.EOF ]
    (lex_all {|"line1\nline2"|});
  check (list token_testable) "tab escape"
    [ Parser.STRING "a\tb"; Parser.EOF ]
    (lex_all {|"a\tb"|});
  check (list token_testable) "quote escape"
    [ Parser.STRING {|say "hi"|}; Parser.EOF ]
    (lex_all {|"say \"hi\""|});
  check (list token_testable) "backslash escape"
    [ Parser.STRING {|a\b|}; Parser.EOF ]
    (lex_all {|"a\\b"|})

let test_empty_input () =
  check (list token_testable) "empty" [ Parser.EOF ] (lex_all "")

let test_unterminated_string () =
  try
    ignore (lex_all {|"hello|});
    fail "expected Lexer_error"
  with Lexer.Lexer_error (_, msg) ->
    check string "error message" "Unterminated string literal" msg

(* --- Bug-finding tests --- *)

let test_unknown_escape () =
  (* Bug #3: Unknown escape sequences silently accepted.
     "\q" silently becomes "q". Users think they wrote an escape. *)
  let tokens = lex_all {|"\q"|} in
  match[@warning "-4"] tokens with
  | [ Parser.STRING s; Parser.EOF ] ->
      (* Document the behavior: unknown escape \q produces just 'q' *)
      check string "unknown escape \\q becomes q" "q" s
  | _ -> fail "Expected string token"

let test_large_integer () =
  (* Bug #4: int_of_string on huge numbers raises opaque Failure.
     Should produce a Lexer_error with useful message, not crash. *)
  try
    ignore (lex_all "99999999999999999999");
    (* If it doesn't raise, that's unexpected but not a crash *)
    ()
  with
  | Lexer.Lexer_error (_, _msg) ->
      (* Good: lexer caught it and gave a located error *)
      ()
  | Failure msg ->
      (* Bug: raw Failure from int_of_string propagated *)
      fail
        (Printf.sprintf "Raw Failure propagated (should be Lexer_error): %s" msg)

let test_leading_zeros () =
  (* Verify that leading zeros work correctly *)
  check (list token_testable) "leading zeros"
    [ Parser.NAT 7; Parser.EOF ]
    (lex_all "007")

(* --- Property-based tests --- *)

let test_lexer_no_crash =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make ~name:"lexer never crashes on arbitrary input" ~count:2000
       QCheck.string (fun input ->
         (try
            let lexer = Lexer.create_from_string "<fuzz>" input in
            let rec drain () =
              match[@warning "-4"] Lexer.token lexer with
              | Parser.EOF -> ()
              | _ -> drain ()
            in
            drain ()
          with
         | Lexer.Lexer_error _ -> ()
         | Sedlexing.MalFormed -> () (* invalid UTF-8 — sedlex limitation *));
         true))

let () =
  run "Lexer"
    [
      ("keywords", [ test_case "keywords" `Quick test_keywords ]);
      ("identifiers", [ test_case "identifiers" `Quick test_identifiers ]);
      ("numbers", [ test_case "numbers" `Quick test_numbers ]);
      ("operators", [ test_case "operators" `Quick test_operators ]);
      ("logical", [ test_case "logical" `Quick test_logical ]);
      ("punctuation", [ test_case "punctuation" `Quick test_punctuation ]);
      ("string", [ test_case "string" `Quick test_string ]);
      ("comments", [ test_case "comments" `Quick test_comments ]);
      ("doc_comments", [ test_case "doc comments" `Quick test_doc_comments ]);
      ("projection", [ test_case "projection" `Quick test_projection_token ]);
      ("dcolon", [ test_case "double colon" `Quick test_dcolon ]);
      ("squig_arrow", [ test_case "squig arrow" `Quick test_squig_arrow ]);
      ("iff", [ test_case "iff token" `Quick test_iff_token ]);
      ("mapsto", [ test_case "mapsto token" `Quick test_mapsto_token ]);
      ( "arithmetic",
        [ test_case "arithmetic tokens" `Quick test_arithmetic_tokens ] );
      ("card", [ test_case "card token" `Quick test_card_token ]);
      ("prime", [ test_case "prime token" `Quick test_prime_token ]);
      ("braces", [ test_case "brace tokens" `Quick test_braces ]);
      ("context_kw", [ test_case "context keyword" `Quick test_context_keyword ]);
      ( "initially_kw",
        [ test_case "initially keyword" `Quick test_initially_keyword ] );
      ("closure_kw", [ test_case "closure keyword" `Quick test_closure_keyword ]);
      ("each_kw", [ test_case "each keyword" `Quick test_each_keyword ]);
      ("cond_kw", [ test_case "cond keyword" `Quick test_cond_keyword ]);
      ( "over_each",
        [ test_case "over, min, max tokens" `Quick test_over_each_tokens ] );
      ( "string_escapes",
        [ test_case "string escapes" `Quick test_string_escapes ] );
      ("empty_input", [ test_case "empty input" `Quick test_empty_input ]);
      ( "unterminated",
        [ test_case "unterminated string" `Quick test_unterminated_string ] );
      ( "bug_finding",
        [
          test_case "unknown escape" `Quick test_unknown_escape;
          test_case "large integer" `Quick test_large_integer;
          test_case "leading zeros" `Quick test_leading_zeros;
        ] );
      ("property", [ test_lexer_no_crash ]);
    ]
