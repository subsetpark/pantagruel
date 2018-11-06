Definitions.

INT           = [\d_]+
SYMBOL        = [^\s\[\]\(\){},\n]+
LITERAL       = (`[^\n]*`|`[^\s\n]+)
FLOAT         = [-+]?[0-9]*\.?[0-9]+
WHITESPACE    = [\t\s]

Rules.

{INT}         : {token, {int, TokenLine, to_integer(TokenChars)}}.
{LITERAL}     : {token, {literal, TokenLine, literal(TokenChars)}}.
{FLOAT}       : {token, {float, TokenLine, TokenChars}}.
".*\n         : {token, {comment, TokenLine, comment(TokenChars)}}.

,             : {token, {',', TokenLine}}.
\]            : {token, {']', TokenLine}}.
\[            : {token, {'[', TokenLine}}.
\(            : {token, {'(', TokenLine}}.
\)            : {token, {')', TokenLine}}.
\{            : {token, {'{', TokenLine}}.
\}            : {token, {'}', TokenLine}}.
\n            : {token, {newline, TokenLine}}.
\.\.\.\n      : skip_token.

{SYMBOL}      : {token, keyword(TokenChars, TokenLine)}.

{WHITESPACE}+ : skip_token.

Erlang code.

to_integer(Chars) ->
    String = string:replace(Chars, "_", ""),
    {Int, ""} = string:to_integer(String),
    Int.

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
