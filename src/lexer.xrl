Definitions.

INT           = [0-9_]+
FLOAT         = [-+]?[0-9]*\.?[0-9]+
LITERAL       = (`[^\n]*`|`[^\s\n]+)
SYMBOL        = [^\s\[\]\(\){},\n]+
WHITESPACE    = [\t\s]

Rules.

{INT}         : {token, {int, TokenLine, integer(TokenChars)}}.
{LITERAL}     : {token, {literal, TokenLine, literal(TokenChars)}}.
{FLOAT}       : {token, {float, TokenLine, to_float(TokenChars)}}.
".*\n         : {token, {comment, TokenLine, comment(TokenChars)}}.

,             : {token, {comma, TokenLine}}.
\[            : {token, {leftbracket, TokenLine}}.
\]            : {token, {rightbracket, TokenLine}}.
\(            : {token, {leftparens, TokenLine}}.
\)            : {token, {rightparens, TokenLine}}.
\{            : {token, {leftbrace, TokenLine}}.
\}            : {token, {rightbrace, TokenLine}}.
\n            : {token, {newline, TokenLine}}.
\.\.\.\n      : skip_token.

{SYMBOL}      : {token, keyword(TokenChars, TokenLine)}.

{WHITESPACE}+ : skip_token.

Erlang code.

integer(Chars) ->
    String = string:replace(Chars, "_", ""),
    {Int, []} = string:to_integer(String),
    Int.

to_float(Chars) ->
    {Float, []} = string:to_float(Chars),
    Float.

literal([$`|Chars]) -> literal(Chars, []);
literal(Chars) -> literal(Chars, []).

literal("`", Acc) -> string:reverse(Acc);
literal([], Acc) -> string:reverse(Acc);
literal([C|Chars], Acc) -> literal(Chars, [C|Acc]).

comment(Chars) -> string:trim(Chars, both, "\n\"\s").

keyword("and", TokenLine) -> {'and', TokenLine};
keyword("or", TokenLine) -> {'or', TokenLine};
keyword("from", TokenLine) -> {from, TokenLine};
keyword("in", TokenLine) -> {in, TokenLine};
keyword("exists", TokenLine) -> {exists, TokenLine};
keyword("all", TokenLine) -> {all, TokenLine};
keyword("xor", TokenLine) -> {'xor', TokenLine};
keyword("fn", TokenLine) -> {fn, TokenLine};
keyword(".", TokenLine) -> {dot, TokenLine};
keyword(";", TokenLine) -> {where, TokenLine};
keyword("<->", TokenLine) -> {iff, TokenLine};
keyword("->", TokenLine) -> {implies, TokenLine};
keyword("=>", TokenLine) -> {produces, TokenLine};
keyword("<-", TokenLine) -> {is_refined, TokenLine};
keyword("::", TokenLine) -> {yields, TokenLine};
keyword(Chars, TokenLine) -> {symbol, TokenLine, Chars}.
