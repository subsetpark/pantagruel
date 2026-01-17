(** Sedlex-based lexer for Pantagruel *)

exception Lexer_error of Ast.loc * string

(** Lexer state *)
type t = {
  buf: Sedlexing.lexbuf;
  mutable filename: string;
  mutable at_bol: bool;  (** At beginning of line (for doc comments) *)
}

let create_from_channel filename channel =
  let buf = Sedlexing.Utf8.from_channel channel in
  Sedlexing.set_filename buf filename;
  { buf; filename; at_bol = true }

let create_from_string filename str =
  let buf = Sedlexing.Utf8.from_string str in
  Sedlexing.set_filename buf filename;
  { buf; filename; at_bol = true }

(** Get current position as Ast.loc *)
let current_loc lexer =
  let (pos, _) = Sedlexing.lexing_positions lexer.buf in
  {
    Ast.file = lexer.filename;
    line = pos.Lexing.pos_lnum;
    col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
  }

(** Get current position as Lexing.position (for menhir compatibility) *)
let lexing_position lexer =
  let (pos, _) = Sedlexing.lexing_positions lexer.buf in
  pos

(** Character classes *)
let digit = [%sedlex.regexp? '0'..'9']
let lower_start = [%sedlex.regexp? 'a'..'z']
let upper_start = [%sedlex.regexp? 'A'..'Z']
let ident_continue = [%sedlex.regexp? 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_']

(* Lower identifier: starts lowercase, may end with ? or ! *)
let lower_ident_base = [%sedlex.regexp? lower_start, Star ident_continue]
let lower_ident = [%sedlex.regexp? lower_ident_base, Opt ('?' | '!')]

(* Upper identifier: starts uppercase *)
let upper_ident = [%sedlex.regexp? upper_start, Star ident_continue]

(* Numbers *)
let nat = [%sedlex.regexp? Plus digit]
let real = [%sedlex.regexp? Plus digit, '.', Plus digit]

(* Whitespace *)
let whitespace = [%sedlex.regexp? ' ' | '\t' | '\r']
let newline = [%sedlex.regexp? '\n']

(* Unicode patterns for operators *)
let darrow_u = [%sedlex.regexp? 0x21D2]    (* ⇒ *)
let arrow_u = [%sedlex.regexp? 0x2192]     (* → *)
let and_u = [%sedlex.regexp? 0x2227]       (* ∧ *)
let or_u = [%sedlex.regexp? 0x2228]        (* ∨ *)
let not_u = [%sedlex.regexp? 0x00AC]       (* ¬ *)
let forall_u = [%sedlex.regexp? 0x2200]    (* ∀ *)
let exists_u = [%sedlex.regexp? 0x2203]    (* ∃ *)
let neq_u = [%sedlex.regexp? 0x2260]       (* ≠ *)
let le_u = [%sedlex.regexp? 0x2264]        (* ≤ *)
let ge_u = [%sedlex.regexp? 0x2265]        (* ≥ *)
let in_u = [%sedlex.regexp? 0x2208]        (* ∈ *)
let subset_u = [%sedlex.regexp? 0x2286]    (* ⊆ *)
let times_u = [%sedlex.regexp? 0x00D7]     (* × *)
let prime_u = [%sedlex.regexp? 0x2032]     (* ′ *)
let mapsto_u = [%sedlex.regexp? 0x21A6]    (* ↦ *)

(** Map identifier strings to keywords *)
let keyword_or_lower_ident = function
  | "module" -> Parser.MODULE
  | "import" -> Parser.IMPORT
  | "where" -> Parser.WHERE
  | "true" -> Parser.TRUE
  | "false" -> Parser.FALSE
  | "and" -> Parser.AND
  | "or" -> Parser.OR
  | "not" -> Parser.NOT
  | "all" -> Parser.FORALL
  | "some" -> Parser.EXISTS
  | "in" -> Parser.IN
  | "subset" -> Parser.SUBSET
  | s -> Parser.LOWER_IDENT s

(** Skip line comment *)
let rec skip_line_comment buf =
  match%sedlex buf with
  | newline -> ()
  | eof -> ()
  | any -> skip_line_comment buf
  | _ -> assert false

(** Skip doc comment (> at start of line) - we preserve nothing for now *)
let rec skip_doc_comment buf =
  match%sedlex buf with
  | newline -> ()
  | eof -> ()
  | any -> skip_doc_comment buf
  | _ -> assert false

(** Parse string literal (after opening quote) *)
let rec read_string buf acc =
  match%sedlex buf with
  | '"' -> Parser.STRING (Buffer.contents acc)
  | '\\', any ->
      let s = Sedlexing.Utf8.lexeme buf in
      let c = match s.[1] with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'r' -> '\r'
        | '"' -> '"'
        | '\\' -> '\\'
        | c -> c
      in
      Buffer.add_char acc c;
      read_string buf acc
  | any ->
      Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
      read_string buf acc
  | eof ->
      failwith "Unterminated string literal"
  | _ -> assert false

(** Main tokenizer - takes the lexer record *)
let rec token_impl lexer =
  let buf = lexer.buf in
  match%sedlex buf with
  (* Whitespace and newlines *)
  | Plus whitespace -> token_impl lexer
  | newline -> lexer.at_bol <- true; token_impl lexer

  (* Comments *)
  | "//" -> skip_line_comment buf; lexer.at_bol <- true; token_impl lexer

  (* Section separator *)
  | "---" -> Parser.SEPARATOR

  (* Multi-char operators - check before single chars *)
  | "=>" -> Parser.DARROW
  | darrow_u -> Parser.DARROW
  | "->" -> Parser.ARROW
  | arrow_u -> Parser.ARROW
  | "!=" -> Parser.NEQ
  | neq_u -> Parser.NEQ
  | "<=" -> Parser.LE
  | le_u -> Parser.LE
  | ">=" -> Parser.GE
  | ge_u -> Parser.GE
  | "|->" -> Parser.MAPSTO
  | mapsto_u -> Parser.MAPSTO

  (* Unicode logical operators *)
  | and_u -> Parser.AND
  | or_u -> Parser.OR
  | not_u -> Parser.NOT
  | '~' -> Parser.NOT
  | forall_u -> Parser.FORALL
  | exists_u -> Parser.EXISTS
  | in_u -> Parser.IN
  | subset_u -> Parser.SUBSET
  | times_u -> Parser.TIMES
  | prime_u -> Parser.PRIME
  | '\'' -> Parser.PRIME

  (* Single-char operators *)
  | '=' -> Parser.EQ
  | '<' -> Parser.LT
  | '>' ->
      (* '>' at beginning of line is a doc comment *)
      if lexer.at_bol then begin
        skip_doc_comment buf;
        lexer.at_bol <- true;
        token_impl lexer
      end else
        Parser.GT
  | '+' -> Parser.PLUS
  | '-' -> Parser.MINUS
  | '*' -> Parser.TIMES
  | '/' -> Parser.DIVIDE
  | '#' -> Parser.CARD

  (* Punctuation *)
  | "::" -> Parser.DCOLON
  (* Projection: .N - must come before plain DOT *)
  | '.', Plus digit ->
      let s = Sedlexing.Utf8.lexeme buf in
      let num_str = String.sub s 1 (String.length s - 1) in
      Parser.PROJ (int_of_string num_str)
  | '.' -> Parser.DOT
  | ',' -> Parser.COMMA
  | ':' -> Parser.COLON
  | '|' -> Parser.PIPE
  | '(' -> Parser.LPAREN
  | ')' -> Parser.RPAREN
  | '[' -> Parser.LBRACKET
  | ']' -> Parser.RBRACKET

  (* String literal *)
  | '"' -> read_string buf (Buffer.create 64)

  (* Numbers - real before nat to catch the decimal point *)
  | real ->
      let s = Sedlexing.Utf8.lexeme buf in
      Parser.REAL (float_of_string s)
  | nat ->
      let s = Sedlexing.Utf8.lexeme buf in
      Parser.NAT (int_of_string s)

  (* Identifiers *)
  | upper_ident ->
      let s = Sedlexing.Utf8.lexeme buf in
      Parser.UPPER_IDENT s
  | lower_ident ->
      let s = Sedlexing.Utf8.lexeme buf in
      keyword_or_lower_ident s

  | eof -> Parser.EOF

  | any ->
      let s = Sedlexing.Utf8.lexeme buf in
      failwith (Printf.sprintf "Unexpected character: %s" s)

  | _ -> assert false

(** Token function that takes a lexer record *)
let token lexer =
  try
    let tok = token_impl lexer in
    (* After returning a token, we're no longer at beginning of line *)
    lexer.at_bol <- false;
    tok
  with Failure msg ->
    raise (Lexer_error (current_loc lexer, msg))

(** Interface for menhir: returns (token, start_pos, end_pos) *)
let menhir_token lexer () =
  let tok = token lexer in
  let (startp, endp) = Sedlexing.lexing_positions lexer.buf in
  (tok, startp, endp)
