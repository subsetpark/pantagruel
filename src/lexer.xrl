Definitions.

INT         = [0-9_]+
LITERAL     = (`[^\n]*`|`[^\b]+)

Rules.

{INT} : {token, {int, TokenLine, to_integer(TokenChars)}}.
{LITERAL} : {token, {literal, TokenLine, string:strip(TokenChars, both, $`)}}.

Erlang code.

to_integer(Chars) ->
    String = string:replace(Chars, "_", ""),
    {Int, ""} = string:to_integer(String),
    Int.
