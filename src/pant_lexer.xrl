Definitions.

INT             = [0-9_]+
DELIMITER       = \[\]\(\){},\.\\
OPERATOR        = <>\-=~/\*\+#\.%^:|&
SP              = \t\s

LITERAL         = `([^{SP}\n,{DELIMITER}]+)

OPERATOR_CHOICE = (>=|==|!=|->|<->|<-|<=|=<|=>|=|-|>|<|\+|\*|~|#|%|\^|;|::|:|&|\|)

SYMBOL          = [^\s\n&&{OPERATOR}&&{DELIMITER}:\"]+
YIELD_TYPE      = (=>|::)
FLOAT           = [-+]?[0-9]*\.?[0-9]+

Rules.

{INT}             : {token, {int, TokenLine, integer(TokenChars)}}.
{LITERAL}         : {token, {literal, TokenLine, literal(TokenChars)}}.
{FLOAT}           : {token, {float, TokenLine, to_float(TokenChars)}}.
\".*\n?           : comment(TokenChars, TokenLine).

\.\.\.\n          : skip_token.
\n[\s\n]*         : {token, {newline, TokenLine}}.

--(-)+\n+         : {token, {sep, TokenLine}}.
[{SP}]*;[{SP}\n]* : {token, {where, TokenLine}}.

YIELD_TYPE        : {token, {yield_type, TokenLine, TokenChars}}.
[{DELIMITER}]     : {token, {list_to_atom(TokenChars), TokenLine}}.
{OPERATOR_CHOICE} : {token, operator(TokenChars, TokenLine)}.
{SYMBOL}          : {token, keyword(TokenChars, TokenLine)}.

[{SP}]+           : skip_token.

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

keyword(Keyword, TokenLine) when
    Keyword == "and";
    Keyword == "xor";
    Keyword == "or"          -> {binary_operator, TokenLine, Keyword};
keyword("exists", TokenLine) -> {quantifier, TokenLine, exists};
keyword("all", TokenLine)    -> {quantifier, TokenLine, all};
keyword("fn", TokenLine)     -> {fn, TokenLine};
keyword("import", TokenLine) -> {import, TokenLine};
keyword("module", TokenLine) -> {module, TokenLine};
keyword("in", TokenLine)   -> {binary_operator, TokenLine, "in"};
keyword(Chars, TokenLine)    -> {symbol, TokenLine, Chars}.

operator("::", TokenLine) -> {yield_type, TokenLine, "::"};
operator("=>", TokenLine) -> {yield_type, TokenLine, "=>"};
operator("<-", TokenLine) -> {refined, TokenLine};
operator(":", TokenLine)  -> {':', TokenLine};
operator("<=", TokenLine) -> {reverse_yield, TokenLine};
operator(TokenChars, TokenLine) when
    TokenChars == "+";
    TokenChars == "*";
    TokenChars == "-";
    TokenChars == "%";
    TokenChars == "=";
    TokenChars == "!=";
    TokenChars == ">";
    TokenChars == "<";
    TokenChars == ">=";
    TokenChars == "=<";
    TokenChars == "->";
    TokenChars == "<->";
    TokenChars == "^";
    TokenChars == "&";
    TokenChars == ":?";
    TokenChars == "|"     -> {binary_operator, TokenLine, TokenChars};
operator(TokenChars, TokenLine) when
    TokenChars == "~";
    TokenChars == "#"     -> {unary_operator, TokenLine, TokenChars}.