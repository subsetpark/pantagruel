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
container_contents

a_comment

term
sequence
set
bunch

binding_or_guard
binding_or_guards

head
head_line
body
body_line

declaration
lambda
alias

log_operation
bin_operation
un_operation
function_application
object_access
quantification

refinement
guarded_refinements
guarded_refinement

maybe_term
maybe_yield_type
maybe_binding_or_guards.


Terminals
'{' '}' '[' ']' '(' ')' '.' ':' ',' '..'
int literal float fn
module
import
sep
comment
symbol
logical_operator
binary_operator
unary_operator
quantifier
yield_type
reverse_yield
refined
where
fullstop.

Rootsymbol program.
Endsymbol '$end'.

Nonassoc 600 comment.
Nonassoc 503 literal.
Nonassoc 502 float.
Nonassoc 501 int.
Nonassoc 500 symbol.
Nonassoc 499 fn.
Nonassoc 400 '('.
Nonassoc 400 ')'.
Nonassoc 399 '{'.
Nonassoc 399 '}'.
Nonassoc 398 '['.
Nonassoc 398 ']'.
Left 350 '.'.
Unary 300 unary_operator.
Left 200 function_application.
Left 100 binary_operator.
Left 50 logical_operator.
Nonassoc 75 ':'.
Left 60 ','.
Left 50 '..'.
Nonassoc 20 yield_type.
Left 15 quantifier.
Nonassoc 14 import.

%
% RULES
%

program -> module_line imports chapters : {program, ['$1', '$2', '$3']}.
program -> '$empty' : {program, [nil, [], []]}.

module_line -> '$empty' : nil.
module_line -> module bare_symbol fullstop : '$2'.

imports -> '$empty' : [].
imports -> import_line : '$1'.
imports -> imports import_line: '$1' ++ '$2'.

import_line -> import bare_symbols fullstop : '$2'.

chapters -> chapter : ['$1'].
chapters -> chapter where chapters : ['$1' | '$3'].

chapter -> head : {chapter, ['$1', []]}.
chapter -> head sep body : {chapter, ['$1', '$3']}.

a_comment -> comment : unwrap('$1').
a_comment -> a_comment comment : merge_comments('$1', '$2').

%
% head
%

head -> head_line : ['$1'].
head -> head_line head : ['$1' | '$2'].

head_line -> declaration fullstop : '$1'.
head_line -> alias fullstop : '$1'.
head_line -> a_comment : '$1'.

% i. declarations

declaration -> a_symbol lambda : {decl, ['$1'|'$2']}.

% ii. aliases

alias -> symbols reverse_yield expression : {alias, ['$1', '$3']}.

%
% body
%

body -> body_line : ['$1'].
body -> body_line body : ['$1' | '$2'].

body_line -> expression refined refinement fullstop: {refinement, ['$1', '$3']}.
body_line -> a_comment : '$1'.
body_line -> logical_operator expression fullstop: {expr, [unwrap('$1'), '$2']}.
body_line -> expression fullstop: {expr, [nil, '$1']}.

% EXPRESSION PRECEDENCE
% This is a strange area. Here are the precedence levels of Pantagruel:
%
% Expression < Binary Operation < Function Application < Unary Operation < Object Access.
%
% This is represented in the parse by a series of rules which evaluate
% either to themselves or the next most tightly binding level.
% Source: http://journal.stuffwithstuff.com/2008/12/28/fixing-ambiguities-in-yacc/

expression -> log_operation : '$1'.

log_operation -> bin_operation : '$1'.
log_operation -> log_operation logical_operator log_operation :
    {bin_appl, [unwrap('$2'), '$1', '$3']}.

bin_operation -> function_application : '$1'.
bin_operation -> bin_operation binary_operator bin_operation :
    {bin_appl, [unwrap('$2'), '$1', '$3']}.

function_application -> un_operation : '$1'.
function_application -> function_application un_operation :
    {f_appl, ['$1', '$2']}.

un_operation -> object_access: '$1'.
un_operation -> unary_operator un_operation :
    {un_appl, [unwrap('$1'), '$2']}.

object_access -> term : '$1'.
object_access -> object_access '.' term : {dot, ['$3', '$1']}.

term -> a_symbol : '$1'.
term -> int : unwrap('$1').
term -> float : unwrap('$1').
term -> literal : unwrap('$1').
term -> sequence : '$1'.
term -> set : '$1'.
term -> bunch : '$1'.
term -> quantification : '$1'.
term -> fn lambda : {lambda, '$2'}.

%
% END EXPRESSION PRECEDENCE
%

expressions -> expression : ['$1'].
expressions -> expression ',' expressions : ['$1' | '$3'].

bunch -> '(' container_contents ')' : {cont, [par, '$2']}.
sequence -> '[' container_contents ']' : {cont, [sequence, '$2']}.
set -> '{' container_contents '}' : {cont, [set, '$2']}.

quantification -> quantifier binding_or_guards '..' expression :
    {quantification, [unwrap('$1'), '$2', '$4']}.

lambda -> maybe_binding_or_guards maybe_yield_type maybe_term :
    ['$1', '$2', '$3'].

binding_or_guards -> binding_or_guard : ['$1'].
binding_or_guards -> binding_or_guard ',' binding_or_guards : ['$1' | '$3'].

binding_or_guard -> term ':' expression : {binding, ['$1', '$3']}.
binding_or_guard -> expression : {guard, '$1'}.

symbols -> a_symbol : ['$1'].
symbols -> a_symbol ',' symbols : ['$1' | '$3'].

bare_symbols -> bare_symbol : ['$1'].
bare_symbols -> bare_symbol ',' bare_symbols : ['$1' | '$3'].

a_symbol -> symbol : unwrap('$1').

bare_symbol -> symbol : bare('$1').

maybe_term -> '$empty' : nil.
maybe_term -> term : '$1'.

maybe_yield_type -> '$empty' : nil.
maybe_yield_type -> yield_type : unwrap('$1').

maybe_binding_or_guards -> '$empty' : [].
maybe_binding_or_guards -> binding_or_guards : '$1'.

container_contents -> '$empty' : [].
container_contents -> expressions : '$1'.
container_contents -> binding_or_guards '..' expression :
    {comprehension, ['$1', '$3']}.

refinement -> expression : [{case_exp, [nil, '$1']}].
refinement -> guarded_refinement : ['$1'].
refinement -> '(' guarded_refinements ')' : '$2'.

guarded_refinements -> guarded_refinement : ['$1'].
guarded_refinements -> guarded_refinement guarded_refinements : ['$1' | '$2'].

guarded_refinement -> expression '..' expression : {case_exp, ['$1', '$3']}.
guarded_refinement -> expression '..' expression ',' : {case_exp, ['$1', '$3']}.

Erlang code.

unwrap({comment, _, Symbol}) -> {comment, Symbol};
unwrap({symbol, _, Symbol}) -> {symbol, Symbol};
unwrap({unary_operator, _, Symbol}) -> list_to_atom(Symbol);
unwrap({binary_operator, _, Symbol}) -> list_to_atom(Symbol);
unwrap({logical_operator, _, Symbol}) -> list_to_atom(Symbol);
unwrap({literal, _, Symbol}) -> {literal, Symbol};
unwrap({_, _, Symbol}) -> Symbol;
unwrap({Symbol, _}) -> Symbol.

bare({symbol, _, Symbol}) -> Symbol.

merge_comments({comment, Comment}, {comment, _, Comment2}) ->
    {comment, Comment ++ [16#f8ff] ++ Comment2}.
