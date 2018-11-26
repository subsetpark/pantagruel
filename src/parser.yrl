Nonterminals

program
module_line
imports
import_line
chapter
chapters

a_symbol
symbols
bare_symbol
bare_symbols
expression
expressions
maybe_expressions

a_comment

term
list
set
bunch

binding_or_guard
binding_or_guards
binding

head
head_line
body
body_line

declaration
lambda
lambda_body
lambda_args
lambda_yield
alias

bin_operation
un_operation
function_application
object_access
quantification
.


Terminals
'{' '}' '[' ']' '(' ')' '.' ':' ',' '\\'
int literal float fn
module
import
sep
comment
newline
symbol
binary_operator
unary_operator
quantifier
yield_type
reverse_yield
refined
where
.

Rootsymbol program.
Endsymbol '$end'.

Left 100 binary_operator.
Left 300 unary_operator.

%
% RULES
%

program -> module_line imports chapters :
    [{module, '$1'}, {imports, '$2'}, {chapters, '$3'}].
program -> imports chapters : [{imports, '$1'}, {chapters, '$2'}].
program -> module_line chapters : [{module, '$1'}, {chapters, '$2'}].
program -> chapters : [{chapters, '$1'}].
program -> '$empty' : [].
program -> newline : [].

module_line -> module bare_symbol newline : '$2'.

imports -> import_line : parse_imports('$1').
imports -> imports import_line: '$1' ++ parse_imports('$2').

import_line -> import bare_symbols newline : {import, '$2'}.

chapters -> chapter : [{chapter, '$1'}].
chapters -> chapter where chapters : [{chapter, '$1'} | '$3'].

chapter -> head : [{head, '$1'}].
chapter -> head sep body : [{head, '$1'}, {body, '$3'}].

a_comment -> comment : unwrap('$1').
a_comment -> a_comment newline comment : merge_comments('$1', '$3').

%
% head
%

head -> head_line : ['$1'].
head -> head_line head : ['$1' | '$2'].

head_line -> declaration newline : '$1'.
head_line -> alias newline : '$1'.
head_line -> a_comment newline : '$1'.

% i. declarations

declaration -> a_symbol lambda : {decl, [{decl_ident, '$1'}|'$2']}.

lambda_body -> '$empty' : [].
lambda_body -> lambda_args : [{lambda_args, '$1'}].
lambda_body -> lambda_args '\\' expressions :
    [{lambda_args, '$1'}, {lambda_guards, '$3'}].

lambda_args -> symbols ':' expressions : [{args, '$1'}, {doms, '$3'}].

lambda_yield -> '$empty' : [].
lambda_yield -> yield_type term :
    [{yield_type, unwrap('$1')}, {lambda_codomain, '$2'}].

% ii. aliases

alias -> symbols reverse_yield expression :
    {alias, [{alias_name, '$1'}, {alias_expr, '$3'}]}.

%
% body
%

body -> body_line : ['$1'].
body -> body_line body : ['$1' | '$2'].

body_line -> expression newline : [{expr, '$1'}].
body_line -> expression refined expression newline :
    [{refinement, [{pattern, '$1'}, {expr, '$3'}]}].
body_line -> expression '\\' expressions refined expression newline :
    [{refinement, [{pattern, '$1'}, {guard, '$3'}, {expr, '$5'}]}].
body_line -> a_comment newline : '$1'.
body_line -> binary_operator body_line : [{intro_op, unwrap('$1')} | '$2'].

% EXPRESSION PRECEDENCE
% This is a strange area. Here are the precedence levels of Pantagruel:
%
% Expression < Binary Operation < Function Application < Unary Operation < Object Access.
%
% This is represented in the parse by a series of rules which evaluate
% either to themselves or the next most tightly binding level.
% Source: http://journal.stuffwithstuff.com/2008/12/28/fixing-ambiguities-in-yacc/

expression -> bin_operation : '$1'.

bin_operation -> function_application : '$1'.
bin_operation ->
    bin_operation binary_operator bin_operation :
        {appl, [{op, unwrap('$2')}, {x, '$1'}, {y, '$3'}]}.

function_application -> un_operation : '$1'.
function_application -> function_application un_operation :
    {appl, [{f, '$1'}, {x, '$2'}]}.

un_operation -> object_access: '$1'.
un_operation -> unary_operator un_operation :
    {appl, [{op, unwrap('$1')}, {x, '$2'}]}.

object_access -> term : '$1'.
object_access -> object_access '.' term : {dot, [{f, '$3'}, {x, '$1'}]}.

term -> a_symbol : '$1'.
term -> int : unwrap('$1').
term -> float : unwrap('$1').
term -> literal : unwrap('$1').
term -> list : '$1'.
term -> set : '$1'.
term -> bunch : '$1'.
term -> quantification : '$1'.
term -> fn lambda : {lambda, '$2'}.

%
% END EXPRESSION PRECEDENCE
%

bunch -> '(' maybe_expressions ')' : {par, '$2'}.
list -> '[' maybe_expressions ']' : {list, '$2'}.
set -> '{' maybe_expressions '}' : {set, '$2'}.

expressions -> expression : ['$1'].
expressions -> expression ',' expressions : ['$1' | '$3'].

maybe_expressions -> '$empty' : [].
maybe_expressions -> expressions : '$1'.
maybe_expressions -> binding_or_guards '\\' expression :
    [{comprehension, [{bindings, '$1'}, {expr, '$3'}]}].

binding_or_guards -> binding_or_guard : ['$1'].
binding_or_guards -> binding_or_guard ',' binding_or_guards : ['$1' | '$3'].

binding_or_guard -> binding : {binding, '$1'}.
binding_or_guard -> expression : {guard, '$1'}.

binding -> term ':' expression : [{bind_symbol, '$1'}, {bind_domain, '$3'}].

symbols -> a_symbol : ['$1'].
symbols -> a_symbol ',' symbols : ['$1' | '$3'].

bare_symbols -> bare_symbol : ['$1'].
bare_symbols -> bare_symbol ',' bare_symbols : ['$1' | '$3'].

a_symbol -> symbol : unwrap('$1').

bare_symbol -> symbol : bare('$1').

quantification -> quantifier binding_or_guards '\\' expression :
    {quantification, [{quantifier, unwrap('$1')}, {bindings, '$2'}, {expr, '$4'}]}.

lambda -> '(' lambda_body ')' lambda_yield  : '$2' ++ '$4'.

Erlang code.

unwrap({comment, _, Symbol}) -> {comment, Symbol};
unwrap({symbol, _, Symbol}) -> {symbol, Symbol};
unwrap({unary_operator, _, Symbol}) -> list_to_atom(Symbol);
unwrap({binary_operator, _, Symbol}) -> list_to_atom(Symbol);
unwrap({literal, _, Symbol}) -> {literal, Symbol};
unwrap({_, _, Symbol}) -> Symbol;
unwrap({Symbol, _}) -> Symbol.

bare({symbol, _, Symbol}) -> Symbol.

merge_comments({comment, Comment}, {comment, _, Comment2}) ->
    {comment, Comment ++ [16#f8ff] ++ Comment2}.

parse_imports({import, [Import|Rest]}) ->
    parse_imports(Rest, [{import, Import}]).
parse_imports([Import|Rest], Acc) ->
    parse_imports(Rest, [{import, Import}|Acc]);
parse_imports([], Acc) -> lists:reverse(Acc).
