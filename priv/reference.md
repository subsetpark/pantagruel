# Pantagruel Language Reference

A Pantagruel document consists of a series of definitions and statements about
*domains* and *procedures*. The Pantagruel language is parseable by the
Pantagruel document checker, which will parse the document and check it for
errors. 

## Pantagruel Syntax

A Pantagruel **document** consists of a series of **chapters**. Each chapter
consists of a **head** and an optional **body**.

At the top of a document, in a chapter head, and in a chapter body, are written
a series of **statements**. Every statement is finished with a period: `.`.
Because every statement is finished with a period, statements can cover
multiple lines of text.

At the top of the program are optional **directives**. Currently, there are two
recognized directives: 

- `module`
- `import`

### Module Statements

The first line of a Pantagruel program is, optionally, a module directive. It looks like this:

```pantagruel
module NUMBERS.
```

This will make the subsequent program available for import under the module
name `NUMBERS`.

### Import Statements

Import directives are used to introduce bindings defined by another module (see
`module`, above) into the evaluation environment for the current document.

### Chapter Heads

A Pantagruel chapter head introduces one or more terms, of two kinds:
**domains** and **procedures**. 

Domains are *sets* or *types* of things. Some domains are built in, as: the
natural numbers, the booleans (true and false), et cetera. Most documents will
introduce some additional domains that they're concerned with describing:
`User`, `File`, `Card`.

Procedures are everything that isn't a domain: procedures are individual
processes, behaviours, actions, functions, programs, etc., that act on domains.

Some procedures *produce* or *go to* some domain: addition, for instance, can
be seen as a procedure that goes from two natural numbers to some other natural
number.

Some procedures don't produce any additional values, but instead are understood
to have *side effects*: they are understood to effect some change in the state
of the world.

There are two expression forms possible in a chapter head:

#### Domain alias

The simplest type of statement available in a chapter head is a **domain alias**.
This is a simple statement of equivalence between a new domain and some
existing one. It uses the **equals** symbol `=`.

Here's an example domain alias:

```pantagruel
Status = {"ok", "error"}.
```

which introduces a domain `Status` which is equivalent to the set of values
`ok` and `error`.

#### Procedure declaration

Procedure declarations are more complex, as they tend to represent the "meat"
of a Pantagruel document: domains by themselves are static things, and we
introduce one or more procedure as individual instances of change in our world.

Here is an example procedure declaration:

```pantagruel
fib n: Nat => Nat.
```

It introduces a procedure called `fib`, which takes one argument, `n` in the
domain `Nat`. The `=>` indicates that this procedure *yields*, *produces*, or
*goes to* a value in some domain (known in mathematics as the procedure's
*codomain*), which in this case is also `Nat`.

It is a simple description of the name and type of a mathematical function that
produces Fibonacci numbers.

##### Procedure declaration forms

Procedures can be declared with or without:

- arguments, 
- codomains,
- and predicates. 

The simplest syntactically valid form to introduce a procedure is to simply
write the name of the procedure on a single line in a chapter head:

```pantagruel
f.
```

This establishes that there is some procedure named `f`, which takes no
arguments and yields nothing. 

Procedure arguments are specified by a comma-separated list of argument
**bindings**. On the left side of the binding is the name of the argument, and
the right side is the domain of the argument. In the example of `fib`, there
was a single argument, named `n`, in the domain `Nat`.

The comma-separated list may also contain other arbitrary **predicates**,
representing some constraint on the procedure domain.

Here's a procedure declaration with a predicate:

```pantagruel
f x: Nat, x > 5.
```

The second element in the list indicates that `f` is defined for any
natural number `x` greater than 5.

```pantagruel
f x: Nat, x > 5, x < 10.
```

This declares a procedure `f` that's defined for any natural number `x`
greater than 5 and less than 10.

This list of colon-separated bindings with optional predicates is a **binding
sequence** and will show up elsewhere in the language.

Any procedure can also yield some domain, as above.

##### Procedures with side effects

A procedure that has no codomain is understood to cause some change in the
world. Therefore, it makes sense to talk about the state of things *before* and
*after* the procedure takes place. 

When a procedure is declared that yields no values, each argument name is
introduced in two ways: as written, and appended with a `'` (for instance, `x`
and `x'`). We can use the value with a `'` to refer to the state of the
argument after the procedure has taken place.

We can see an example:

```pantagruel
check_out u:User, d:Document.
---
owner d = nobody and has_perm? u d -> owner d' = u.
```

It introduces a procedure, `check_out`, which takes some `User` and some
`Document`. To check out a document is to update who its owner currently is;
thus, we can refer to `d`---the document being checked out---as well as
`d'`---the same document, after the `check_out` procedure takes place.

#### Values

We can use the keyword `val` in chapter heads to provide a subtle but useful
variation on procedures. 

Most of the time, individual things in Pantagruel don't have names; because we
want to describe things that are always true, we will usually use
quantification (see below) to talk about some generic instance of a domain.

At times, however, it's useful to introduce individual objects that can be
referred to in the same way we refer to procedures. For instance, describing a
card game, we might want to refer to the cards currently on the table, and
describe how they change over time. Using the keyword `val` gives a name to a
particular thing, and lets us describe how it changes over time.

To illustrate the example of cards on the table, for instance:

```
val table => {Card}.
---
```

We can also understand values as *tables* associating arguments with some
value. For instance:

```
val scores p: Player => Nat0.
score_goal p: Player.
---
score_goal p -> scores' = update scores ... p => (scores p) + 1.
```

In this example, `scores` is a mapping from `Player`s to `Nat0`s, and so
`(scores p)` is the score for a particular player at a particular point in
time.

### Chapter Bodies

Chapter **bodies** are separated from chapter heads with a horizontal
line, consisting of three or more hyphens (`---`):

```pantagruel
f.
---
f x = 1.
```

Chapter bodies consist of one or more **statements**. Each statement expresses
some **proposition** about a procedure or domain and is terminated by a period.

A top-level statement can be made of any valid **expression**. 

### Multiple Chapters

Multiple chapters are separated by the symbol `where`. The chapter separator
begins a new chapter head.

### Expressions

The stuff that Pantagruel statements are made up of is not natural language.
While one can---and should---use comments (any line beginning with `//`) to
give a natural-language gloss on the statements being put down, the statements
themselves are written in a formal, logical notation. 

Like a programming language, this syntax can be completely and unambiguously
parsed by a computer program. Like mathematical notation, it's designed to be
compressed and expressive, easily written by hand, easily edited and updated.

#### Variables

Most of the terms in a Pantagruel document will be those introduced by the
author. These are *variables*---their meaning is contextual. They are analogous
to mathematical variables, which allow us to express universal truths: `x + x =
x * 2`, for instance, regardless of the value given for `x`.

To give a trival example, we can return to our Fibonacci function:

```pantagruel
fib n: Nat => Nat.
---
fib n = fib (n - 1) + fib (n - 2).
```

Having introduced `fib` and `n` as terms above the line, we can use them in
expressions below the line.

We can author Pantagruel documents that are about things entirely other than
mathematics, too. Returning to a previous example: 

```pantagruel
User.
Document.
check_out u:User, d:Document.
---
owner d = nobody and has_perm? u d -> owner d' = u.
```

We see that all the terms in use are purely symbolic; what's useful is the
logical and symbolic relations between them.

#### Values

That said: we need not establish every domain entirely symbolically. We can use
*literal* syntax---that is, things we recognize as values rather than symbolic
names that stand for something else---for data that it would be useful to
manipulate.

##### Integers

Integer values are represented as normal numbers: `1`, `1000`. 

##### Real numbers

Real numbers are written with a decimal point: `2.47`, `10.0`.

##### Text

Literal text values are represented with quotation marks: `"ok"`, `"error"`.

##### Operators

There is a closed set of symbols that are recognized as **operators**,
that are applied infix instead of prefix, eg: `1 + 1`. `x in Y`.

###### Binary operators

Binary operators take two arguments.

- `+`
- `-`
- `*`
- `/`
- `^`
- `mod`
- `|`
- `&`
- `->`
- `<->`
- `=`
- `>`
- `<`
- `=<`
- `>=`
- `!=`
- `and`
- `or`
- `xor`
- `in`

###### Unary operators

Unary operators take one argument.

- `#`
- `~`

###### Sum and product types

`+` and `*` can be applied to domains as well as values. 

`+` produces a sum type; for instance, `String + Nat + Bool` denotes the domain
consisting of all strings, natural numbers, and boolean values. 

`*` produces a product type; for instance, `String * Nat` denotes the domain of
all *pairs* of strings and natural numbers.

##### Symbols

Symbols are identifiers to which values are bound, as in function
declarations. They can contain any alphanumeric character that is not
an operator.

#### Procedure application

Application of `f` to `x` is represented by the syntax `f x`.

Procedure application can be performed on any number of arguments, eg.: `f x y
z`. 

#### Quantification

`all` and `some` Represent the logical quantifications "for all..." and "there
is some...", respectively. They have the form of a **quantifier**, followed by
a comma-separated list of **binding** or **expression** forms, followed by a
**yields** sign, followed by a statement about the bound variables.

```pantagruel
all x: Nat, y: Nat, x > y ... (x - y) > 0.
```

This example says that for any x and y in the natural numbers where x is
greater than y, x minus y is greater than 0. It could also be written in a
slightly more compressed form, binding multiple variables from the same domain:

```pantagruel
all (x, y): Nat, x > y ... (x - y) > 0.
```

#### Containers

There are three **containers** in Pantagruel. Containers are represented by
surrounding an expression or comma-separated list of expressions by a pair of
delimiters which reflects the type of container being represented.

- parens: `()`
- set: `{}`
- sequence: `[]`

##### Parentheses

Any expression can be wrapped in parentheses to bind more tightly. For
instance, whereas `f x y z a` denotes the application of `f` to the four
arguments `x`, `y`, `z`, and `a`, `f x (y z a)` denotes the application of `f`
to two arguments: `x` and the result of the application of `y` to `z` and `a`.

##### Sets and sequences

Sets and sequences represent groups of values or domains. 

The notation `{1, 2, 3}` represents the unordered set of the natural numbers 1,
2, 3. The notation `[1, 2, 3]` represents the ordering of those same values in
that order.

Set and sequence notation, when applied to domains, denotes a "set of" or
"sequence of" domain. For instance, `[String]` is the domain of sequences of
strings.

**sequence comprehensions** may be formed by wrapping a quantification in
square brackets. For instance, 

```pantagruel
[all x : X ... x ^ 2].
```

denotes a sequence made up every element in the domain X, squared.

#### Case

A case expression consists of the symbol `case`, an optional expression, and a
series of **mappings** of expressions to expressions. Each mapping is separated
by a comma, and the left side is mapped to the right side with a `=>`. 

For instance:

```
fib x = case ...
    x > 2 => fib (x - 1) + fib (x - 2),
    x = 1 => 1,
    x = 2 => 1.
```

If there's no expression between `case` and `...`, then the left-hand side of
each mapping clause is typed as a statement that might be true or false. If
there's an expression between `case` and `...`, it will be type-checked against
the left-hand sides of the mapping clauses. For instance: 

```
fib x = case x ...
    1 => 1,
    2 => 1,
    _ => fib (x - 1) + fib (x - 2).
```

While this resembles the sort of pattern-matching found in some programming
languages, it's simpler than that: the "pattern" side of a mapping clause here
doesn't introduce a new symbol to the scope of the expression side. However,
within `case` syntax, we *can* use the special symbol `_`, as in the above
example, to denote an "else" branch that always evaluates to true if none of
the earlier branches match.

#### Update

An update expression consists of the symbol `update`, an expression, and
mapping clauses.

For instance:

```
fib' = update fib ...
    5 => 100.
```

Represents a procedure which behaves exactly like `fib`, except when it is
called on `5`.

#### Do

A `do` expression consists of the symbol `do` followed by an arbitrary sequence
of expressions separated by semicolons. 

For instance:

```
do 
  alert "ok";
  true.
```

The expressions in the sequence need have no typing relationship to each other
and the type of the whole expression is simply the type of the last expression
in the sequence.

This can be useful when describing a sequence of events or effectful procedures
that need to happen, without (for instance) constructing a boolean expression
by joining that sequence with `and`s.

## Binding

The Pantagruel interpreter evaluates a program for the purpose of
enforcing Pantagruel's **binding** rules. To sum them up, they are:

1. Any symbol referred to in a chapter head must be bound by the end of that
   head.
2. Any symbol referred to in a chapter body must be bound by the end of the
   body of the *next* chapter.

This structure is crucial in establishing the Pantagruel style of
specification, where new terms are introduced so as to provide refinement for
known terms, eg:

```pantagruel
pred n:Nat.
---
pred n = is_even? n and n > 5.

where

is_even? n:Nat => Bool.
---
is_even? 0.
~(is_even? 1).
is_even? n <-> is_even? (n - 2).
```

This example consists of two chapters, the first introducing a procedure `pred`
and establishing some facts about it; the second glosses the terminology
referred to in the facts.

It describes the behavior of a predicate as checking `is_even?` and `> 5`. It
then goes on in the next chapter to fill in what `is_even?` involves. This
allows it to be defined in context; if a symbol had to be defined before it was
used, as is often the case in programming languages, the narrative thread of
increasing detail would be lost and specifications would be all preamble.

### Binding Forms

Symbols are bound into the program environment in one of two ways: either
they're built into the language, or they're introduced with one of a
few specific forms.

#### Procedure declarations

When a procedure is declared, the name of the procedure is bound into the
environment, as are the names of the variables the procedure takes.

```
f x:Y, x > z => a
* *
```

#### Domain aliases

When a domain alias is introduced, the name of the alias is bound into the
environment.
```
D = X
*
```

In the case of these chapter head statements, all other symbol positions
must be bound by the end of the subchapter.

#### Quantifications

Expressions within quantifications have similar binding behavior as procedures.

```
all x: Y, x > z ... f x
    *
```

#### Binding Operators

A symbol can be bound with `:` to give it the type of the domain on the right
of the operator:

```
all x: Nat ... x.
```

`x` is of the type `Nat`.

## Types

Every expression in a Pantagruel document has a type. The type of an expression
is the domain to which the values it produces belong.

### Static forms

#### Sets and sequences

The type of any expression `[e]` is `sequence of (type of e)`, and the type of
`{e}` is `set of (type of e)`.

The type of `{v1, v2, v3}` is the sum of the types of values `v1, v2, v3`.

Similarly, the type of `D1 + D2` is the sum of the domains `D1` and `D2`.

#### Declarations

The type of a procedure declaration is a procedure type, typed by all the
arguments and the codomain of the function (the part to the right of the `=>`).

If a procedure has no `=>`, its codomain is `Void`.

For bare declarations, with no arguments or `=>`: 

If the symbol begins with a lower-case letter, it will be typed as a 0-argument
`Void` function.

If it begins with an upper case letter, it will be typed as a domain.

### Expressions

#### Singletons

If a procedure is declared with some codomain and no arguments, then a
reference to that procedure is typed as its codomain. This lets us denote
singleton values as procedures. For instance:

```
User.
nobody => User.
---
nobody.
```

The type of `nobody` in this chapter body is `User`.

#### Application

##### Procedure application

The type of the application of a procedure to its arguments is the codomain of
that procedure.

##### Sequence application

The application of a sequence to a value of the type contained by the sequence
is typed as getting the index of that value. For instance:

```
User.
users => [User].
admin => User.
---
users admin.
```

This is interpreted as getting the index of `admin` within `users` and so is
typed as `Nat0`.

The application of a sequence to some integer is typed as indexing within that
sequence. For instance:

```
User.
users => [User].
---
users 0.
```

This is interpreted as getting the 0th element of `users`.

The above applies to strings as well, where the element domain can be either
`String` or `Char`.

#### Booleans

The type of a boolean operation is `Bool`. Boolean arguments are checked for
unification (see below).

#### Comparisons

The type of a comparison operation is `Bool`. Comparison arguments are checked
for unification (see below).

#### `in`

The type of the `in` operator is `Bool`. The righthand operand must be either a
set or sequence (including strings), and the lefthand operator must be
unifiable (see below) with the inner type of the right.

#### `#`

The type of the `#` unary operator is `Nat0`. The operand must be either a set
or sequence.

#### Other Binary Operators

The type of an arithmetic binary operation is the unification of the two sides
(see below).

#### Cases

The type of a case expression is the unification of the types of all its
expressions all its branches (see below).

If there is expression between `case` and `...`, then the document checker will
additionally check that that expression's type can be unified with the types of
the patterns of all its branches.

### Type Unification

Pantagruel has a type system that is somewhat more lenient than those found in
ordinary programming languages. Simply put, the *unification* of any two types
is the **nearest common ancestor** they share in their type hierarchies.

For instance:

The unification of `Nat` and `Nat` is `Nat`.

The unification of `Nat` and `Nat0` is `Nat0`; `Nat0` contains all the values
in `Nat0`. 

The unification of `Bool` and `Char` is `Nat0`. See the full type hierarchy
diagram below.

The domain `Any` contains all other types. Therefore:

The unification of `Bool` and `Any` is `Any`.

The unification of some user-declared domain `Foo` and `Any` is `Any`.

However: non-Any types which only share `Any` as an ancestor type are *not*
unifiable. Thus:

There is no unification of `Real` and `String`.

There is no unification of some user-declared domain `Foo` and `Nat`.

It's important to note that these rules allow operations which would be
disallowed in standard programming language type systems. For instance,

```
inc: Nat => Nat.
---
inc 0.
```

is a valid Pantagruel document. While 0 is not a member of the set of natural
numbers, it is unifiable with them. On the other hand, 

```
inc: Nat => Nat.
---
inc "ok".
```

produces a type error: the only shared ancestor between `String` and `Nat` is
`Any`. 

These rules have been chosen to produce the greatest number of helpful type
errors while making sure to err on the side of unintrusiveness. The purpose of
the Pantagruel type system is not to prevent illegal runtime operations; thus,
we don't want document authors to ever feel that they are "fighting the type
system" in order to express themselves.

### Type Hierarchy

The following domains are included in the base environment.

```
     Any__________________
     | \      \      \    \
  Real  Domain String Date Void
     | 
   Rat
     | 
   Int
     |
  Nat0
  /  | 
Bool Nat 
     |
     Char
```

### Type Errors

To type-check a document, the Pantagruel checker attempts to determine the type
of each top-level statement in order. If any expression either:

- Can't be fully resolved due to a type unification failure anywhere inside the
  expression;
- Fails one of the special-case type checks noted above (eg., checking that
  the operand of `#` is a container type)

Then the checker will emit a type error and the document will fail the check.
