(* @archlint.module core
   @archlint.domain pantagruel.lexer-tokens *)

(** Pure token classification and display helpers. *)

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
  | "each" -> Parser.EACH
  | "in" -> Parser.IN
  | "subset" -> Parser.SUBSET
  | "context" -> Parser.CONTEXT
  | "initially" -> Parser.INITIALLY
  | "closure" -> Parser.CLOSURE
  | "cond" -> Parser.COND
  | "over" -> Parser.OVER
  | "min" -> Parser.MIN
  | "max" -> Parser.MAX
  | "check" -> Parser.CHECK
  | s -> Parser.LOWER_IDENT s

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
  | Parser.EACH -> "'each'"
  | Parser.IN -> "'in'"
  | Parser.SUBSET -> "'subset'"
  | Parser.SQUIG_ARROW -> "'~>'"
  | Parser.DARROW -> "'=>'"
  | Parser.ARROW -> "'->'"
  | Parser.IFF -> "'<->'"
  | Parser.EQ -> "'='"
  | Parser.NEQ -> "'~='"
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
  | Parser.AT -> "'@'"
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
  | Parser.COND -> "'cond'"
  | Parser.OVER -> "'over'"
  | Parser.MIN -> "'min'"
  | Parser.MAX -> "'max'"
  | Parser.CHECK -> "'check'"

(** Describe a token category (for "expected X" messages) *)
let describe_token = function
  | Parser.LOWER_IDENT _ -> "identifier"
  | Parser.UPPER_IDENT _ -> "type name"
  | Parser.NAT _ -> "number"
  | Parser.REAL _ -> "number"
  | Parser.STRING _ -> "string literal"
  | Parser.PROJ _ -> "projection"
  | Parser.ACTION_LABEL _ -> "action label"
  | ( Parser.EOF | Parser.MODULE | Parser.IMPORT | Parser.WHERE | Parser.TRUE
    | Parser.FALSE | Parser.AND | Parser.OR | Parser.NOT | Parser.FORALL
    | Parser.EXISTS | Parser.EACH | Parser.IN | Parser.SUBSET
    | Parser.SQUIG_ARROW | Parser.DARROW | Parser.ARROW | Parser.IFF | Parser.EQ
    | Parser.NEQ | Parser.LT | Parser.GT | Parser.LE | Parser.GE | Parser.PLUS
    | Parser.MINUS | Parser.TIMES | Parser.DIVIDE | Parser.CARD | Parser.PRIME
    | Parser.MAPSTO | Parser.DOT | Parser.COMMA | Parser.COLON | Parser.DCOLON
    | Parser.PIPE | Parser.AT | Parser.SEPARATOR | Parser.LPAREN | Parser.RPAREN
    | Parser.LBRACKET | Parser.RBRACKET | Parser.LBRACE | Parser.RBRACE
    | Parser.CONTEXT | Parser.INITIALLY | Parser.CLOSURE | Parser.COND
    | Parser.OVER | Parser.MIN | Parser.MAX | Parser.CHECK ) as tok ->
      string_of_token tok

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
    Parser.EACH;
    Parser.IN;
    Parser.SUBSET;
    Parser.DOT;
    Parser.COMMA;
    Parser.COLON;
    Parser.DCOLON;
    Parser.PIPE;
    Parser.AT;
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
    Parser.COND;
    Parser.OVER;
    Parser.MIN;
    Parser.MAX;
    Parser.CHECK;
    Parser.ACTION_LABEL "";
    Parser.EOF;
  ]
