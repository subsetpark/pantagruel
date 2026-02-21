(** Sedlex-based lexer for Pantagruel *)

exception Lexer_error of Ast.loc * string

type t = {
  buf : Sedlexing.lexbuf;
  mutable filename : string;
  mutable at_bol : bool;  (** At beginning of line (for doc comments) *)
  mutable pending_docs : string list list;
      (** Doc comment groups (each reversed, list reversed) *)
  mutable doc_break : bool;  (** Blank line seen since last doc comment line *)
  mutable last_token : Parser.token option;  (** Last token returned *)
  mutable in_action_label : bool;  (** After ~>, read free-form label *)
}
(** Lexer state *)

let create_from_channel filename channel =
  let buf = Sedlexing.Utf8.from_channel channel in
  Sedlexing.set_filename buf filename;
  Doc_comments.clear ();
  (* Clear doc map for new file *)
  {
    buf;
    filename;
    at_bol = true;
    pending_docs = [];
    doc_break = false;
    last_token = None;
    in_action_label = false;
  }

let create_from_string filename str =
  let buf = Sedlexing.Utf8.from_string str in
  Sedlexing.set_filename buf filename;
  Doc_comments.clear ();
  (* Clear doc map for new file *)
  {
    buf;
    filename;
    at_bol = true;
    pending_docs = [];
    doc_break = false;
    last_token = None;
    in_action_label = false;
  }

(** Take and clear pending doc comments *)
let take_docs lexer =
  let groups = List.rev_map List.rev lexer.pending_docs in
  lexer.pending_docs <- [];
  lexer.doc_break <- false;
  groups

(** Global ref to current lexer for parser access *)
let current_lexer : t option ref = ref None

let set_current lexer = current_lexer := Some lexer

let get_pending_docs () =
  match !current_lexer with None -> [] | Some lexer -> take_docs lexer

(** Get current position as Ast.loc *)
let current_loc lexer =
  let pos, _ = Sedlexing.lexing_positions lexer.buf in
  {
    Ast.file = lexer.filename;
    line = pos.Lexing.pos_lnum;
    col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
  }

(** Get current position as Lexing.position (for menhir compatibility) *)
let lexing_position lexer =
  let pos, _ = Sedlexing.lexing_positions lexer.buf in
  pos

(** Character classes *)
let digit = [%sedlex.regexp? '0' .. '9']

let lower_start = [%sedlex.regexp? 'a' .. 'z']
let upper_start = [%sedlex.regexp? 'A' .. 'Z']

let ident_continue =
  [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_']

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

(** Map identifier strings to keywords *)
let keyword_or_lower_ident = function
  | "module" -> Parser.MODULE
  | "import" -> Parser.IMPORT
  | "where" -> Parser.WHERE
  | "true" -> Parser.TRUE
  | "false" -> Parser.FALSE
  | "and" -> Parser.AND
  | "or" -> Parser.OR
  | "all" -> Parser.FORALL
  | "some" -> Parser.EXISTS
  | "in" -> Parser.IN
  | "subset" -> Parser.SUBSET
  | "context" -> Parser.CONTEXT
  | "initially" -> Parser.INITIALLY
  | "closure" -> Parser.CLOSURE
  | s -> Parser.LOWER_IDENT s

(** Skip line comment *)
let rec skip_line_comment buf =
  match%sedlex buf with
  | newline -> ()
  | eof -> ()
  | any -> skip_line_comment buf
  | _ -> assert false

(** Read doc comment content (> at start of line) *)
let read_doc_comment buf =
  let content = Buffer.create 64 in
  let rec loop () =
    match%sedlex buf with
    | newline -> Buffer.contents content
    | eof -> Buffer.contents content
    | any ->
        Buffer.add_string content (Sedlexing.Utf8.lexeme buf);
        loop ()
    | _ -> assert false
  in
  (* Skip leading space if present *)
  (match%sedlex buf with ' ' -> () | _ -> Sedlexing.rollback buf);
  loop ()

(** Parse string literal (after opening quote) *)
let rec read_string buf acc =
  match%sedlex buf with
  | '"' -> Parser.STRING (Buffer.contents acc)
  | '\\', any ->
      let s = Sedlexing.Utf8.lexeme buf in
      let c =
        match s.[1] with
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
  | eof -> failwith "Unterminated string literal"
  | _ -> assert false

(** Read free-form action label text after ~>. Stops at |, ., //, newline, or
    eof; rolls back the delimiter. *)
let read_action_label buf =
  let content = Buffer.create 64 in
  let rec loop () =
    match%sedlex buf with
    | '|' | '.' ->
        Sedlexing.rollback buf;
        String.trim (Buffer.contents content)
    | "//" ->
        Sedlexing.rollback buf;
        String.trim (Buffer.contents content)
    | newline -> String.trim (Buffer.contents content)
    | eof -> String.trim (Buffer.contents content)
    | any ->
        Buffer.add_string content (Sedlexing.Utf8.lexeme buf);
        loop ()
    | _ -> assert false
  in
  loop ()

(** Main tokenizer - takes the lexer record *)
let rec token_impl lexer =
  let buf = lexer.buf in
  if lexer.in_action_label then begin
    lexer.in_action_label <- false;
    let text = read_action_label buf in
    Parser.ACTION_LABEL text
  end
  else
    match%sedlex buf with
    (* Whitespace and newlines *)
    | Plus whitespace -> token_impl lexer
    | newline ->
        if lexer.at_bol && lexer.pending_docs <> [] then lexer.doc_break <- true;
        lexer.at_bol <- true;
        token_impl lexer
    (* Comments *)
    | "//" ->
        skip_line_comment buf;
        lexer.at_bol <- true;
        token_impl lexer
    (* Section separator *)
    | "---" -> Parser.SEPARATOR
    (* Multi-char operators - check before single chars *)
    | "=>" -> Parser.DARROW
    | "<->" -> Parser.IFF
    | "->" -> Parser.ARROW
    | "!=" -> Parser.NEQ
    | "<=" -> Parser.LE
    | ">=" -> Parser.GE
    | "|->" -> Parser.MAPSTO
    | "~>" ->
        lexer.in_action_label <- true;
        Parser.SQUIG_ARROW
    | '~' -> Parser.NOT
    | '\'' -> Parser.PRIME
    (* Single-char operators *)
    | '=' -> Parser.EQ
    | '<' -> Parser.LT
    | '>' ->
        (* '>' at beginning of line is a doc comment *)
        if lexer.at_bol then begin
          let content = read_doc_comment buf in
          if lexer.doc_break || lexer.pending_docs = [] then
            lexer.pending_docs <- [ content ] :: lexer.pending_docs
          else begin
            match lexer.pending_docs with
            | group :: rest -> lexer.pending_docs <- (content :: group) :: rest
            | [] -> lexer.pending_docs <- [ [ content ] ]
          end;
          lexer.doc_break <- false;
          lexer.at_bol <- true;
          token_impl lexer
        end
        else Parser.GT
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
    | '{' -> Parser.LBRACE
    | '}' -> Parser.RBRACE
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
    (* If there are pending docs, associate them with this token's position *)
    if lexer.pending_docs <> [] then begin
      let pos, _ = Sedlexing.lexing_positions lexer.buf in
      let adjacent = not lexer.doc_break in
      let groups = take_docs lexer in
      Doc_comments.add pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        groups adjacent
    end;
    (* After returning a token, we're no longer at beginning of line *)
    lexer.at_bol <- false;
    lexer.last_token <- Some tok;
    tok
  with Failure msg -> raise (Lexer_error (current_loc lexer, msg))

(** Interface for menhir: returns (token, start_pos, end_pos) *)
let menhir_token lexer () =
  let tok = token lexer in
  let startp, endp = Sedlexing.lexing_positions lexer.buf in
  (tok, startp, endp)

(** Display a token with its actual value (for "unexpected X" messages) *)
let string_of_token = function
  | Parser.LOWER_IDENT s -> Printf.sprintf "'%s'" s
  | Parser.UPPER_IDENT s -> Printf.sprintf "'%s'" s
  | Parser.NAT n -> Printf.sprintf "'%d'" n
  | Parser.REAL r -> Printf.sprintf "'%g'" r
  | Parser.STRING s -> Printf.sprintf "\"%s\"" s
  | Parser.PROJ n -> Printf.sprintf "'.%d'" n
  | Parser.ACTION_LABEL s -> Printf.sprintf "action label '%s'" s
  | Parser.EOF -> "end of input"
  | Parser.MODULE -> "'module'"
  | Parser.IMPORT -> "'import'"
  | Parser.WHERE -> "'where'"
  | Parser.TRUE -> "'true'"
  | Parser.FALSE -> "'false'"
  | Parser.AND -> "'and'"
  | Parser.OR -> "'or'"
  | Parser.NOT -> "'~'"
  | Parser.FORALL -> "'all'"
  | Parser.EXISTS -> "'some'"
  | Parser.IN -> "'in'"
  | Parser.SUBSET -> "'subset'"
  | Parser.SQUIG_ARROW -> "'~>'"
  | Parser.DARROW -> "'=>'"
  | Parser.ARROW -> "'->'"
  | Parser.IFF -> "'<->'"
  | Parser.EQ -> "'='"
  | Parser.NEQ -> "'!='"
  | Parser.LT -> "'<'"
  | Parser.GT -> "'>'"
  | Parser.LE -> "'<='"
  | Parser.GE -> "'>='"
  | Parser.PLUS -> "'+'"
  | Parser.MINUS -> "'-'"
  | Parser.TIMES -> "'*'"
  | Parser.DIVIDE -> "'/'"
  | Parser.CARD -> "'#'"
  | Parser.PRIME -> "'''"
  | Parser.MAPSTO -> "'|->'"
  | Parser.DOT -> "'.'"
  | Parser.COMMA -> "','"
  | Parser.COLON -> "':'"
  | Parser.DCOLON -> "'::'"
  | Parser.PIPE -> "'|'"
  | Parser.SEPARATOR -> "'---'"
  | Parser.LPAREN -> "'('"
  | Parser.RPAREN -> "')'"
  | Parser.LBRACKET -> "'['"
  | Parser.RBRACKET -> "']'"
  | Parser.LBRACE -> "'{'"
  | Parser.RBRACE -> "'}'"
  | Parser.CONTEXT -> "'context'"
  | Parser.INITIALLY -> "'initially'"
  | Parser.CLOSURE -> "'closure'"

(** Describe a token category (for "expected X" messages) *)
let describe_token = function
  | Parser.LOWER_IDENT _ -> "identifier"
  | Parser.UPPER_IDENT _ -> "type name"
  | Parser.NAT _ -> "number"
  | Parser.REAL _ -> "number"
  | Parser.STRING _ -> "string literal"
  | Parser.PROJ _ -> "projection"
  | Parser.ACTION_LABEL _ -> "action label"
  | tok -> string_of_token tok

(** All token constructors (with dummy values for parameterized tokens) *)
let all_tokens =
  [
    Parser.MODULE;
    Parser.IMPORT;
    Parser.WHERE;
    Parser.TRUE;
    Parser.FALSE;
    Parser.UPPER_IDENT "";
    Parser.LOWER_IDENT "";
    Parser.NAT 0;
    Parser.REAL 0.0;
    Parser.STRING "";
    Parser.SQUIG_ARROW;
    Parser.DARROW;
    Parser.ARROW;
    Parser.IFF;
    Parser.EQ;
    Parser.NEQ;
    Parser.LT;
    Parser.GT;
    Parser.LE;
    Parser.GE;
    Parser.PLUS;
    Parser.MINUS;
    Parser.TIMES;
    Parser.DIVIDE;
    Parser.CARD;
    Parser.PRIME;
    Parser.MAPSTO;
    Parser.PROJ 0;
    Parser.AND;
    Parser.OR;
    Parser.NOT;
    Parser.FORALL;
    Parser.EXISTS;
    Parser.IN;
    Parser.SUBSET;
    Parser.DOT;
    Parser.COMMA;
    Parser.COLON;
    Parser.DCOLON;
    Parser.PIPE;
    Parser.SEPARATOR;
    Parser.LPAREN;
    Parser.RPAREN;
    Parser.LBRACKET;
    Parser.RBRACKET;
    Parser.LBRACE;
    Parser.RBRACE;
    Parser.CONTEXT;
    Parser.INITIALLY;
    Parser.CLOSURE;
    Parser.ACTION_LABEL "";
    Parser.EOF;
  ]
