Nonterminals
a_symbol
symbols
expression
expressions
bin_operation
un_operation
function_application
value
program
section
head
body
declaration
decl_body
decl_args
decl_guard
decl_yield.

Terminals
'{' '}' '[' ']' '(' ')' '.' ':' ','
operator
int literal float
comment
newline
symbol
binary_operator
unary_operator
yield_type.

Rootsymbol program.

Left 100 binary_operator.
Right 200 function_application.

program -> section : '$1'.
program -> section program : '$1'.

section -> head body : [{head, '$1'}, {body, '$2'}].

body -> '$empty' : [].

head -> declaration : ['$1'].
head -> declaration newline head : ['$1' | '$3'].

declaration ->
    a_symbol '(' decl_body ')' decl_yield  : {declaration,
        [{decl_ident, '$1'}|'$3' ++ '$5']
    }.

decl_body -> '$empty' : [].
decl_body -> decl_args : [{decl_args, '$1'}].
decl_body -> decl_args '.' decl_guard : [{decl_args, '$1'}, {decl_guards, '$3'}].

decl_args -> symbols ':' symbols : [{args, '$1'}, {doms, '$3'}].

decl_guard -> '$empty' : [].
decl_guard -> expressions : '$1'.

decl_yield -> '$empty' : [].
decl_yield -> yield_type a_symbol : [{decl_yield, unwrap('$1')}, {decl_domain, '$2'}].

expression -> value : '$1'.
expression -> bin_operation : '$1'.
expression -> un_operation : '$1'.
expression -> function_application : '$1'.

value -> a_symbol : '$1'.
value -> int : unwrap('$1').
value -> float : unwrap('$1').
value -> literal : unwrap('$1').
value -> '(' expression ')' : '$2'.

bin_operation ->
    expression binary_operator expression : {appl, [{op, unwrap('$2')}, {x, '$1'}, {y, '$3'}]}.

un_operation ->
    unary_operator expression : {appl, [{op, unwrap('$1')}, {x, '$2'}]}.

function_application -> value expression : {appl, [{f, '$1'}, {x, '$2'}]}.

expressions -> expression : ['$1'].
expressions -> expression ',' expressions : ['$1' | '$3'].

symbols -> a_symbol : ['$1'].
symbols -> a_symbol ',' symbols : ['$1' | '$3'].

a_symbol -> symbol : unwrap('$1').

Erlang code.

unwrap({_, _, Symbol}) -> Symbol.
