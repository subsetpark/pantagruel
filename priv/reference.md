# Pantagruel Language Reference

Pantagruel is a notation designed for *specifying the behavior of a
computer program*. It consists of a closed set of notational forms,
drawn, where available, from mathematics and logic, designed to allow
the author to express the behavior of some computer program in terms
of **refinements**, where some introduced program is expressed in more
specific and therefore stronger terms, and **propositions**, which are
statements that must be true under some specific implementation of the
program for the implementation to be considered correct.

Unlike some other formal specification methods, Pantagruel expressions
cannot be evaluated in terms of the values they represent. To give a
trivial example, while `1 + 1` is a valid expression in Pantagruel and
represents the application of the `+` operator with 1 on the left side
and 1 on the right side, Pantagruel cannot *evaluate* that expression to
arrive at `2`. By extension, Pantagruel has no capacity to validate that
`1 + 1 = 2` is a true expression or to demonstrate that `1 + 1 = 3`
is a false one. This obviously reduces Pantagruel's capacity for program
correctness verification and validation to effectively nil; however,
hopefully its semantic neutrality means that it is more available to the
programmer or even non-programmer who wants to sketch out their ideas for
themselves or others, waving their hands where it's useful to wave them,
without establishing everything from first principles.

Roughly speaking, Pantagruel can be said to consist of a *syntax*,
which is designed to be convenient to write by hand and to parse by a
computer, and a *semantics*, which is largely not parsed by the Pantagruel
interpreter. Thus the semantics of Pantagruel, while not undefined, are
mostly a set of *suggested readings*, a system of of interpretations
that it would be useful to share among the humans that read and write
Pantagruel programs.

> As Pantagruel evolves, there is the constant threat that any
> semantics which is currently only a suggested reading will get implemented
> in the interpreter if there is a use for it.

## Pantagruel Syntax

A Pantagruel **program** consists of a series of **chapters**. Each
chapter consists of a **head** and an optional **body**.

At the top of the program are two optional sections: the **module**
statement and some number of **import** statements.

### Module statements

The first line of a Pantagruel program is, optionally, a module statement. It looks like this:

```pantagruel
module NUMBERS.
```

This will make the subsequent program available for import under the
module name `NUMBERS`.

The module statement is optional; omitting it will not affect the
semantics of the program, and by the same token the semantics of the
program being opened with the Pantagruel interpreter will not be affected
by the presence of a module statement. It is a syntax error to include
more than one module statement or to include a module statement anywhere
other than the beginning of a program.

### Import statements

Any program that is declared as a module and is in the import path of
the interpreter may be imported. Import statements have one of two forms:

```pantagruel
import NUMBERS.
import USERS.
```

or

```pantagruel
import NUMBERS, USERS.
```

These two forms have the equivalent effect of importing both of the
named modules.

Importing a module brings all of its declared symbols into the execution
environment of the current program. This allows symbols to be reused
without having to define them over and over.

Import statements must all be located after the module statement, if
it's present, and before the beginning of the program chapters. There
may be any number of imports and they can import any non-zero number of
modules per statement.

### Chapter Heads

A Pantagruel chapter head introduces one or more symbols, of two primary
kinds: **procedures** and **domains**. A procedure might be a computer
program, or a function. For instance, `+` is a procedure, as would be
a more abstract behavior like `save_file` or `render_scene`. Most
Pantagruel programs will introduce at least one procedure, which is the
program or business logic they are specifying.

A domain is some set of values which variables will be taken from. For
instance, the natural numbers (abbreviated ℕ) make up a domain, as
do the reals (ℝ). But so could the values ``{`ok, `error}`` or some
business logic-specific concept like `User` or `Post`. In this way
domains are like types, though more flexible.

There are three expression forms possible in a chapter head:

#### Procedure declaration

Here is an example procedure declaration:

```pantagruel
fib n:Nat :: Nat.
```

It introduces a procedure called `fib`, which takes one argument, `n`
in the domain `Nat`. The `::` indicates that this procedure **yields**
a value in some domain, which in this case is also `Nat`.

#### Constructor declaration

**constructors** are a special type of procedure that introduce a
user-defined domain. They are written identically to normal procedures,
except instead of the **yields** symbol, they are written with the
**produces** symbol `=>`:

```pantagruel
user name:String, age:Nat => User.
```

This introduces both the domain `User`, as well as the constructor
`user`. `user` takes two arguments, the first of which is a `String` and
the second of which is a `Nat`, and produces a value in the domain `User`.

#### Advanced procedure syntax

Procedures can be declared with or without arguments, return domains,
and predicates. Here's a declaration of a procedure with no arguments
and an undefined return:

```pantagruel
f.
```

Arguments, as above, are specified by a comma-separated list of
argument/domain pairs, separated by a colon. The comma-separated list may
also contain other arbitrary **predicates**, representing some constraint
on the procedure domain.

Here's a procedure declaration with a predicate:

```pantagruel
f x:Nat, x > 5.
```

The second element in the list indicates that `f` is defined for any
natural number `x` greater than 5.

```pantagruel
f x:Nat, x > 5, x < 10.
```

This declares a procedure `f` that's defined for any natural number `x`
greater than 5 and less than 10.

This list of colon-separated **binding pairs** with optional
**predicates** is a **binding sequence** and will show up elsewhere in
the language.

#### Domain Aliasing

The final type of statement available in a chapter head is a **domain
alias**. This is a simple statement of equivalence between a new domain
and some existing one. It uses the **reversed produces** symbol `<=`,
which could be also seen as a double-stroke version of the **refined**
symbol (as seen below).

Here's an example domain alias:

```pantagruel
Status <= {`ok, `error}.
```

Introduces a domain `Status` which is equivalent to the set of values
`ok` and `error`.

Here's an example chapter head:

```pantagruel
Score <= Nat.
halve(score: Score .. score mod 2 = 0) :: Score.
```

It introduces a procedure, `halve`, which operates on all even
`Score`s. It also clarifies that `Score` in this case is just an alias
for `Nat`.

### Chapter Bodies

Chapter **bodies** are separated from chapter heads with a horizontal
line, consisting of three or more hyphens:

```pantagruel
f.
---
f x = 1.
```

Chapter bodies consist of one or more **statements**. Each statement
expresses either a **refinement** of a procedure or a **proposition**
about a procedure, and is terminated by a period.

The most basic **expression** in any statement is **application**,
represented by separating two values with a space, like this: `f x`.

#### Refinements

A refinement is any expression, followed by the **refined** symbol
`<-`, followed by another expression. Here's an example:

```pantagruel
f x <- x + 2.
```

Which says that `f x` or "`f` of `x`" is *refined by* the more concrete
expression `x + 2`. A more complex example might be

```pantagruel
f x <- x > 5 .. g (x * 2).
```

Which says that `f` of `x` is refined by `g (x * 2)` *when `x` is
greater than five*. The expression between the `<-` and the `..` is
a **guard**, and performs a very similar function to predicates in a
procedure declaration.

#### Propositions

A proposition is just any other expression that should evaluate to true
for an implementation to be correct. Since there are no hard semantics
imposed on expression evaluation, there are no syntactic constraints on
propositions; any valid expression can be a proposition. `f x` by itself
on a line is a synctactically valid body statement, though it might be
hard to gain much insight from it as a reader.

#### Statement Logic

Ordinarily, every statement in a body must be true. Thus they can be
considered as a single expression by reading them with an implicit `and`
between them, referring to the logical operator *∧*. At the beginning
of any statement, there may be written an `and` to make this explicit,
or an `or` to indicate disjunction *∨* rather than conjunction.

### Expressions

The most common syntactic element is the expression; this is anything
that should evaluate to some value. Expressions are found in the predicate
of a procedure or constructor declaration, the guard and right-hand side
of a refinement, and by themselves as propositions. And expressions are
recursive, so a single expression is very often a compound of multiple
expressions.

#### Values

The most basic expressions are bare values, ie, any expression which
evaluates to itself.

##### Integers

Integer values are represented as normal numbers: `1`, `1000`.

##### Floating point values

Floating point values are written with at least one leading digit and
a decimal point: `2.47`, `10.0`.

##### Literals

Literal text values are represented with a backtick: `` `ok``, ``
`error``.

##### Operators

There is a closed set of symbols that are recognized as **operators**,
that are applied infix instead of prefix, eg: `1 + 1`. `x in Y`.

There are also two **unary** operators, `#` and `~`.

##### Symbols

Symbols are identifiers to which values are bound, as in function
declarations. They can contain any alphanumeric character that is not
an operator.

##### Lambdas

Lambdas, or anonymous functions, can be constructed with the same syntax
used in procedure declaration. The only difference is that instead
of the name of the procedure, lambdas have `fn` before the opening
parenthesis. For instance:

```pantagruel
map f:fn z:_A :: _B, x:_A :: _B.
```

This declaration introduces the procedure `map`, which takes two
arguments, `f` and `x`. `f` is itself a lambda that goes from `_A` to
`_B`, and `x` is a `_A`. `map` returns a `_B`.

This also illustrates the use of **generic domains**, which are introduced
with underscores.

#### Containers

There are two **containers** in Pantagruel. Containers are represented by
surrounding a comma-separated list of expressions by a pair of delimiters
which reflects the type of container being represented.

- set: `{}`
- sequence: `[]`

#### Applications

There are three ways to represent **procedure application**
in Pantagruel. Placing any expression after any other expression
separated by a single space is parsed as an application of the first
to the second. So `f x` is parsed as applying `f` to `x`; similarly,
`[1, 2, 3] 0` is parsed as applying `[1, 2, 3]` to `0`; which, if a
sequence is understood as a function from the natural numbers including 0 to
its contents, is a fairly straightforward way to do sequence indexing.

The second case of application is in the case of operators, where `x +
y` is parsed as applying `+` to `x` and `y`.

The third case is the **dot-application** form: words like `foo.bar.baz`
will be parsed as `baz(bar(foo))`, as in method/attribute access syntax
in an object-oriented programming language.

In the normal, prefix procedure application form, all procedure
application is strictly left-associative (there is no order of operations)
and parsed as the successive application of procedures of one argument. In
other words, procedures are implicitly curried: `f x y` is parsed as
`(f(x))(y)`. If `f` is a function of two arguments, then `f x` is a
partial application of `f` to `x`, resulting in a function expecting
one argument `y`.

#### Special forms

There are two recognized special expression forms in Pantagruel beyond
normal function application. These forms are additional bits of syntax
for expression more complex operations.

##### Comprehensions

**set** or **sequence comprehensions** may be formed by following one or
more comma-separated **bindings** or **guards** with some **expression**,
separated by a backslash, like this:

```pantagruel
[x : X .. x ^ 2].
```

The above expression is read to refer to a sequence made up every element
in x, squared.

A binding expression is an expression applying the `:` operator to some
domain or expression, eg., `x : X`, `n : Nat`. Any other normal form
of expression functions as a guard, restricting the values bound out of
the domains with arbitrary predicates.

##### Quantifications

**universal** and **existential** quantification is structured very
similarly to comprehension forms. They differ in that quantification
isn't contained within a sequence or set, and must be preceded by a
**quantifier**: either `all` or `exists`, corresponding to the two types.

```pantagruel
all x : Nat, y : Nat, x > y .. (x - y) > 0.
```

This example says that for any x and y in the natural numbers where x
is greater than y, x minus y is greater than 0. It could also be written
in a slightly more compressed form:

```pantagruel
all (x, y) : Nat, x > y .. (x - y) > 0.
```

## Semantics

The semantics of Pantagruel are largely implicit: designed to be
understood by a human reader rather than a computer. Therefore expressions
don't *evaluate* to anything. This lends a great deal of flexibility when
writing Pantagruel. For instance, simple function application syntax
can be understood to refer to things like indexing into a sequence,
or its reverse operation of providing the index of a sequence element,
according to context.

At the same time, Pantagruel is parseable by computer, and there are other
ways that evaluation of a specification text can assist in modelling. One
in particular is the question of how to ensure a certain level of *rigor*
in a specification: we might venture that a regular syntax guarantees
a certain rigor of form, hopefully resulting in more regular and richer
texts; another aspect of rigor it would be beneficial to promote would
be a guarantee that every symbol used is formally defined.

To this end the Pantagruel interpreter, `pant`, evaluates whether all
symbols in a program have been properly **bound** into scope. The
semantics of binding are at present the only formal semantics that
Pantagruel has. Symbols found in certain positions of certain forms are
bound into the evaluation environment; usage of symbols that are not
properly bound produces an evaluation error. In this way the interpreter
ensures that every symbol has been defined.

Therefore the semantics consist of two halves: that which is formal
and part of the interpreter behavior, and that which is presented as a
"suggested reading", with the intention that conventions of interpretation
should be the same among all users of the notation.

### Binding

The Pantagruel interpreter evaluates a program for the purpose of
enforcing Pantagruel's **binding** rules. To sum them up, they are:

1. Any symbol referred to in a chapter head must be bound by the end of
that head.
2. Any symbol referred to in a chapter body must be bound by the end of
the *next* body.

This structure is crucial in establishing the Pantagruel style of
specification, where new terms are introduced so as to provide refinement
for known terms, eg:

```pantagruel
pred n:Nat.
---
pred n <- is_even? n and (n > 5).

;

is_even? n:Nat  :: Bool.
---
is_even? 0.
~(is_even? 1).
is_even? n <- is_even? (n - 2).
```

That specification describes the behavior of a predicate as checking
`is_even?` and `> 5`. It then goes on in the next chapter to fill in
what `is_even?` involves. This allows it to be defined in context; if
a symbol had to be defined before it was used, as is often the case in
programming languages, the narrative thread of increasing detail would
be lost and specifications would be all preamble.

#### Binding Forms

Symbols are bound into the program environment in one of two ways: either
they're built-in to the language, or they're introduced with one of a
few specific forms. A form position might bind a symbol into the program
environment, into a temporary scope for evaluating a single expression,
or not at all.

##### Procedure declarations

When a procedure is declared, the name of the procedure is introduced
into program scope, as are the names of the variables the procedure takes.

```
f x:Y, x > z  :: a
* *
```

When a constructor procedure is declared, the resulting domain is also
bound into program scope.

```
d x:Y, x > z  => D
* *              *
```

##### Domain aliases

When a domain alias is introduced, the name of the alias is bound into
program scope.

```
D <= X
*
```

In the case of these chapter head statements, all other symbol positions
must be bound by the end of the subchapter.

##### Comprehensions

Set/sequence comprehensions introduce symbols that are bound within the
context of those expressions. However, they are not bound in the scope
of the rest of the program.

```
[n : N, n > m .. f n]
 +
```

##### Quantifications

Expressions within quantifications have similar binding behavior to
comprehensions. However, there is a distinction between the universal
and existential quantifiers: while universal quantifiers have the exact
same behavior as comprehensions:

```
all x : Y, x > z .. f x
    +
```

Existential quantifiers introduce symbols into *program* scope.

```
exists x : Y, x > z .. f x
       *
```

### Suggested Readings

As mentioned, the evaluation semantics of Pantagruel are entirely
implicit. Nevertheless, in order to design a language that would be
useful and efficient when it comes to specifying programs, it's useful
to choose operators with a mind to how they would be used.

#### Operators

These are the operators that the pantagruel interpreter recognizes.

##### Equals

`=` should be read as "is equal to". It expresses the concept of
*equality*, that is, for `x = y` to be true, `x` and `y` should evaluate
to the same value. Sets would have the same members, sequences would have
the same contents, and so on.

##### Not-equals

`!=` should be read as "is not equal to". It expresses the inverse of `=`.

##### Not

`~` should be read as "not". It's a unary operator, applied to any
term. `~x` expresses the negation of the term `x`.

##### Greater than

`>` should be read as "is greater than". `x > y` expresses that `x`
evaluates as a greater value than `y`. For comparable values this might
have an obvious semantics, whereas context might need to be provided
for user-defined domains or container types.

##### Less than

`<` should be read as "is less than".

##### Greater than or equal to

`>=` should be read as "greater than or equal to".

##### Less than or equal to

`=<` should be read as "less than or equal to".

##### Plus

`+` should be read as "plus". It expresses the concept of addition, where
that might be interpreted varyingly for different domains. For instance,
the various number types are straightforward to add together; for sequences
concatenation seems like a reasonable interpretation. For sets, `+`
is sometimes used to express symmetric difference, though it can also
be used to express simple union.

##### Minus

`-` should be read as "minus". It expresses the concept of subtraction,
again, where that is sensibly defined. Subtraction is usefully seen
as the inverse operation of addition, so for sequences that would be the
removal of a subsequence. For sets it can be understood as the removal of
a member or of the members of a second set.

##### Times

`*` should be read as "times". For sets, it might be useful to read `x *
y` as taking the Cartesian product of `x` and `y`.

##### Division

`%` should be read as "divided by". It expresses the concept of
division. Generally speaking the behavior of division in the context
of a programming language must be defined in terms of, for instance,
whether the behavior of the normal division operator applied to two
integers should produce another integer or a float. When dealing with
the domains of mathematics, where there is a useful distinction between
the natural numbers, the integers, the rationals and the reals, it's a
little more straightforward to understand the the codomain of division
of two integers is the rational numbers.

##### Exponentiation

`^` is the power or exponent operator. `x ^ y` should be read as "`x`
to the `y`th power".

##### Membership

`in` should be read as "in". It expresses set, sequence or domain membership;
`x in y` expresses that `x` is in `y`.

##### Conjunction

`and` should be read as "and". It is the Boolean operator expressing the
relation of conjunction. For `x and y` to be true, `x` must be true and
`y` must be true.

##### Disjunction

`or` should be read as "or". It is the Boolean operator expressing the
relation of disjunction. For `x or y` to be true, at least one of `x`
and `y` must be true.

##### Exclusive disjunction

`xor` should be read as "exclusive or". It is the Boolean operator
expressing the relation of exclusive disjunction. For `x xor y` to be
true, exactly one of `x` and `y` must be true.

##### Implication

`->` should be read as "implies". `x -> y` can also be read as "if `x`,
then `y`". For `x -> y` to be true, one of the following must obtain:
`x` and `y` are both true; `y` is true; neither `x` nor `y` is true.

##### Biconditional

`<->` should be read as "if and only if". For `x <-> y` to be true,
one of the following must obtain: `x` and `y` are both true; neither
`x` nor `y` is true.

##### Cardinality

`#` should be read as "size of". `#` is a unary operator, expressing the
size of a sequence or set. `#x` expresses the number of elements within `x`.

##### Union

`|` is the set union operator. `x | y` should be read as "the union of
`x` and `y`".

##### Intersection

`&` is the set intersection operator. `x & y` should be read as "the
intersection of `x` and `y`".

#### Domains

Several domains are recognized by the pantagruel interpreter by default:

- `Bool`: the domain of true and false.
- `Nat`: the domain of natural numbers.
- `Nat0`: the domain of natural numbers, including 0.
- `Int`: the domain of integers.
- `Rat`: the domain of rational numbers.
- `Real`: the domain of real numbers.
- `String`: the domain of strings.
- `Any`: the top/universal domain.
- `Nil`: the empty set.
