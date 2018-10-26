# Pantagruel Language Reference

Pantagruel is a notation designed for *specifying the behavior of a
computer program*. It consists of a closed set of notational forms,
drawn, where available, from mathematics and logic, designed to allow
the writer to express the behavior of some computer program in terms
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
Pantagruel programs.[^1]

[^1] As Pantagruel evolves, there is the constant threat that any
semantics which is currently only a suggested reading will get implemented
in the interpreter if there is a use for it.

## Pantagruel Syntax

A Pantagruel **program** consists of a series of **sections**. Each
section consists of a **head** and an optional **body**.

### Section Heads

A Pantagruel section head introduces one or more symbols, of two
primary kinds: **procedures** and **domains**. A procedure might be
a computer program, or a function. For instance, `+` is a procedure,
and most Pantagruel programs will introduce at least one procedure,
which is the program or business logic they are specifying.

A domain is some set of values, which variables will be taken from. For
instance, the natural numbers (abbreviated ℕ) make up a domain, as
do the reals (ℝ). But so could the values ``{`ok, `error}`` or some
business logic-specific concept like `User` or `Post`. In this way
domains are like types, though more flexible.

There are three expression forms possible in a section head:

#### Procedure declaration

Here is an example procedure declaration:

`fib(n : Nat) :: Nat`

It introduces a procedure called `fib`, which takes one argument, `n`
in the domain `Nat`. The `::` indicates that this procedure **yields**
a value in some domain, which in this case is also `Nat`.

#### Constructor declaration

**constructors** are a special type of procedure that introduce a
user-defined domain. They are written identically to normal procedures,
except instead of the **yields** symbol, they are written with the
**produces** symbol `=>`:

`user(name, age : String, Nat) => User`

This introduces both the domain `User`, as well as the constructor
`user`. `user` takes two arguments, the first of which is a `String` and
the second of which is a `Nat`, and produces a value in the domain `User`.

#### Advanced procedure syntax

Procedures can be declared with or without arguments, return domains,
and predicates. Here's a declaration of a procedure with no arguments
and an undefined return:

`f()`

Arguments, as above, are specified by a comma separated list of argument
names, followed by a colon, followed by a comma separated list of
argument domains.

Finally, procedures can be declared with a comma separated list
of **predicates** representing some constraint on the procedure
domain. Here's a procedure declaration with a predicate:

`f(x:Nat . x > 5)`

The expression after the `.` indicates that `f` is defined for any
natural number `x` greater than 5.  `f(x:Nat . x > 5, x < 10)`

This declares a procedure `f` that's defined for any natural number `x`
greater than 5 and less than 10.

#### Domain Aliasing

The final type of expression available in a section head is a **domain
alias**. This is a simple statement of equivalence between a new domain
and some existing one. It uses the **reversed produces** symbol `<=`.

Here's an example domain alias:

``Status <= {`ok, `error}``

Introduces a domain `Status` which is equivalent to the set of values
`ok` and `error`.

Here's an example section head:

```
Score <= Nat
halve(score: Score . score mod 2 = 0) :: Score
```

It introduces a procedure, `halve`, which operates on all even
`Score`s. It also clarifies that `Score` in this case is just an alias
for `Nat`.

### Section Bodies

Section **bodies** consist of one or more **statements**. Each statement
is a single line expressing either a **refinement** of a procedure or
a **proposition** about a procedure.

The most basic **expression** in any statement is **application**,
represented by separating two values with a space, like this: `f x`.

#### Refinements

A refinement is any expression, followed by the refinement operator
`<-`, followed by another expression. Here's an example:

`f x <- x + 2`

Which says that `f x` or "`f` of `x`" is *refined by* the more concrete
expression `x + 2`. A more complex example might be

`f x . x > 5 <- g (x * 2)`

Which says that `f` of `x` is refined by `g (x * 2)` *when `x` is
greater than five*. The expression between the `.` and the `<-` is
a **guard**, and performs a very similar function to predicate in a
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
`error``. If the text has a space in it, it should be surrounded by
backtickets: `` `arbitrarily long text value` ``.

##### Operators

There is a closed set of symbols that are recognized as **operators**,
that are applied infix instead of prefix, eg: `1 + 1`. `x : Y`.

##### Symbols

Symbols are identifiers to which values are bound, as in function
declarations. They can contain any alphanumeric character that is not
an operator.

##### Lambdas

Lambdas, or anonymous functions, can be constructed with the same syntax
used in procedure declaration. The only difference is that instead
of the name of the procedure, lambdas have `fn` before the opening
parenthesis. For instance:

```
map(f, x: fn(z:_A)::_B, _A) :: _B
```

This declaration introduces the procedure `map`, which takes two
arguments, `f` and `x`. `f` is itself a lambda that goes from `_A` to
`_B`, and `x` is a `_A`. `map` returns a `_B`.

This also illustrates the use of *generic domains*, which are introduced
with underscores.

#### Containers

There are two **containers** in Pantagruel. Containers are represented by
surrounding a comma separated list of expressions by a pair of delimiters
which reflects the type of container being represented.

- set: `{}`
- list: `[]`

#### Applications

There are three ways to represent **procedure application**
in Pantagruel. Placing any expression after any other expression
separated by a single space is parsed as an application of the first
to the second. So `f x` is parsed as applying `f` to `x`; similarly,
`[1, 2, 3] 0` is parsed as applying `[1, 2, 3]` to `0`; which, if a
list is understood as a function from the natural numbers including 0 to
its contents, is a fairly straightforward way to do list indexing.

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

- comprehension
- quantifier
- reduction
