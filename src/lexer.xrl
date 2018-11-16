Definitions.

INT           = [0-9_]+
FLOAT         = [-+]?[0-9]*\.?[0-9]+
LITERAL       = (`[^\n]*`|`[^\s\n]+)
OPERATOR      = <>\-=~/\*\+#\.%^:;|&
OPERATOR_CHOICE = (>=|==|!=|->|<->|<-|<=|=>|=|-|>|<|\+|\*|~|#|%|\^|;|::|:|&|\|)
DELIMITER     = \[\]\(\){},\.
SYMBOL        = [^\s\n&&{OPERATOR}&&{DELIMITER}:\"]+
WHITESPACE    = [\t\s]
YIELD_TYPE    = (=>|::)

Rules.

{INT}             : {token, {int, TokenLine, integer(TokenChars)}}.
{LITERAL}         : {token, {literal, TokenLine, literal(TokenChars)}}.
{FLOAT}           : {token, {float, TokenLine, to_float(TokenChars)}}.
\".*\n?           : comment(TokenChars, TokenLine).

\.\.\.\n          : skip_token.
\n                : {token, {newline, TokenLine}}.

YIELD_TYPE        : {token, {yield_type, TokenLine, TokenChars}}.
[{DELIMITER}]     : {token, {list_to_atom(TokenChars), TokenLine, TokenChars}}.
{OPERATOR_CHOICE} : {token, operator(TokenChars, TokenLine)}.
{SYMBOL}          : {token, keyword(TokenChars, TokenLine)}.

{WHITESPACE}+     : skip_token.

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

comment(Chars, TokenLine) ->
    Pushback = case string:find(Chars, "\n", trailing) of
        nomatch -> [];
        _ -> "\n"
    end,
    {token, {comment, TokenLine, string:trim(Chars, both, "\n\"\s")}, Pushback}.

keyword("and", TokenLine) -> {'and', TokenLine};
keyword("or", TokenLine) -> {'or', TokenLine};
keyword("from", TokenLine) -> {from, TokenLine};
keyword("in", TokenLine) -> {in, TokenLine};
keyword("exists", TokenLine) -> {exists, TokenLine};
keyword("all", TokenLine) -> {all, TokenLine};
keyword("xor", TokenLine) -> {binary_operator, TokenLine, 'xor'};
keyword("fn", TokenLine) -> {fn, TokenLine};
keyword(Chars, TokenLine) -> {symbol, TokenLine, Chars}.

operator("::", TokenLine) -> {yield_type, TokenLine, "::"};
operator("=>", TokenLine) -> {yield_type, TokenLine, "=>"};
operator(":", TokenLine) -> {':', TokenLine};
operator(TokenChars, TokenLine) when
    TokenChars == "+";
    TokenChars == "*";
    TokenChars == "-";
    TokenChars == "%";
    TokenChars == "=";
    TokenChars == "<=";
    TokenChars == "!=";
    TokenChars == ">";
    TokenChars == "<";
    TokenChars == ">=";
    TokenChars == "->";
    TokenChars == "<->";
    TokenChars == "^";
    TokenChars == "&";
    TokenChars == "|" ->
        {binary_operator, TokenLine, TokenChars};
operator(TokenChars, TokenLine) when
    TokenChars == "~";
    TokenChars == "#" ->
        {unary_operator, TokenLine, TokenChars};
operator(TokenChars, TokenLine) -> {operator, TokenLine, TokenChars}.
