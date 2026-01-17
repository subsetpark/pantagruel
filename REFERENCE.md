# Pantagruel Language Reference

## Document Structure

A Pantagruel document has this structure:

```
module NAME.

import MODULE1.
import MODULE2.

<declarations>
---
<propositions>

where

<declarations>
---
<propositions>
```

- **Module declaration**: Required. `module NAME.` where NAME is uppercase.
- **Imports**: Optional. Import other modules with `import NAME.`
- **Chapters**: One or more. Each has a head (declarations) and body (propositions) separated by `---`.
- **Multiple chapters**: Separated by `where`.

## Lexical Elements

### Identifiers

| Type | Pattern | Examples |
|------|---------|----------|
| Upper | Starts uppercase, then letters/digits/hyphens/underscores | `User`, `Document`, `Point3D` |
| Lower | Starts lowercase, then letters/digits/hyphens/underscores, optionally ends with `?` or `!` | `owner`, `check-out`, `valid?`, `update!` |

### Keywords

```
module  import  where  true  false
and     or      not    all   some
in      subset
```

### Literals

| Type | Examples |
|------|----------|
| Natural numbers | `0`, `42`, `123` |
| Real numbers | `3.14`, `0.5`, `100.0` |
| Strings | `"hello"`, `"with \"escapes\""` |
| Booleans | `true`, `false` |

String escape sequences: `\\`, `\"`, `\n`, `\t`, `\r`

### Operators

| ASCII | Unicode | Meaning |
|-------|---------|---------|
| `=>` | `⇒` | Procedure return type |
| `->` | `→` | Implication |
| `and` | `∧` | Conjunction |
| `or` | `∨` | Disjunction |
| `not` | `¬` or `~` | Negation |
| `all` | `∀` | Universal quantifier |
| `some` | `∃` | Existential quantifier |
| `in` | `∈` | Membership |
| `subset` | `⊆` | Subset |
| `=` | | Equality |
| `!=` | `≠` | Inequality |
| `<` | | Less than |
| `>` | | Greater than |
| `<=` | `≤` | Less or equal |
| `>=` | `≥` | Greater or equal |
| `+` | | Addition / Sum type |
| `-` | | Subtraction / Negation |
| `*` | `×` | Multiplication / Product type |
| `/` | | Division |
| `#` | | Cardinality |
| `'` | `′` | Prime (post-state) |
| `\|->` | `↦` | Maps-to (in overrides) |

### Punctuation

| Symbol | Meaning |
|--------|---------|
| `.` | Statement terminator |
| `,` | Parameter/binding separator |
| `:` | Type annotation |
| `::` | Module qualification |
| `\|` | Quantifier body separator |
| `---` | Head/body separator |
| `( )` | Grouping, tuples |
| `[ ]` | List types, overrides |
| `.N` | Tuple projection (e.g., `.1`, `.2`) |

### Comments

```
// Line comment (to end of line)

> Doc comment (must start at beginning of line)
```

## Types

### Built-in Types

| Type | Description |
|------|-------------|
| `Bool` | Boolean values (`true`, `false`) |
| `Nat` | Positive integers (1, 2, 3, ...) |
| `Nat0` | Non-negative integers (0, 1, 2, ...) |
| `Int` | All integers (..., -1, 0, 1, ...) |
| `Real` | Real numbers |
| `String` | Text strings |
| `Nothing` | Empty/null type |

**Numeric hierarchy**: `Nat < Nat0 < Int < Real`

A `Nat` can be used where `Nat0`, `Int`, or `Real` is expected.

### Product Types (Tuples)

```
Point = Nat * Nat.
Triple = Nat * Nat * Nat.
```

Constructed with parentheses: `(1, 2)`, `(x, y, z)`

Accessed with projection: `p.1`, `p.2` (1-indexed)

### Sum Types (Unions)

```
Result = Value + Error.
Maybe = Something + Nothing.
```

A value of sum type belongs to one of the component types.

### List Types

```
Users = [User].
Matrix = [[Nat]].
```

## Declarations

### Domain Declaration

Declares a new domain type:

```
User.
Document.
```

### Type Alias

Creates a named alias for a type expression:

```
Point = Nat * Nat.
Result = Value + Nothing.
Users = [User].
```

### Procedure Declaration

Declares a procedure with typed parameters and optional return type:

```
// With return type
owner d: Document => User.
distance p: Point, q: Point => Real.

// Nullary (no parameters)
nobody => User.
origin => Point.

// Void (no return type) - for state transitions
check-out u: User, d: Document.
deposit a: Account, amount: Nat.
```

**Syntax**: `name [params] [=> ReturnType].`

- Parameters: `name: Type` separated by commas
- Return type: Optional, preceded by `=>`
- Void procedures have no return type and enable primed expressions

### Procedure with Guards

Guards constrain when a procedure applies:

```
withdraw a: Account, amount: Nat, balance a >= amount.
```

## Expressions

### Variables and Domains

```
x           // Variable reference
User        // Domain reference
```

### Literals

```
true false          // Booleans
0 42 123            // Natural numbers
3.14 0.5            // Real numbers
"hello"             // Strings
```

### Tuples

```
(1, 2)              // Pair
(x, y, z)           // Triple
p.1                 // First projection
p.2                 // Second projection
```

### Procedure Application

```
owner d             // Single argument
distance p q        // Multiple arguments (juxtaposition)
f x y z             // Curried application
```

### Arithmetic

```
x + y               // Addition
x - y               // Subtraction
x * y               // Multiplication
x / y               // Division
-x                  // Negation
```

### Comparison

```
x = y               // Equality
x != y              // Inequality
x < y               // Less than
x > y               // Greater than
x <= y              // Less or equal
x >= y              // Greater or equal
```

### Logical

```
p and q             // Conjunction
p or q              // Disjunction
not p               // Negation
p -> q              // Implication
```

### Membership and Sets

```
x in Domain         // Membership
xs subset ys        // Subset relation
#xs                 // Cardinality
```

### Quantifiers

```
all x: T | P            // Universal: for all x of type T, P holds
some x: T | P           // Existential: there exists x of type T where P holds

// Multiple bindings
all x: T, y: U | P

// With guards
all x: T, x > 0 | P     // For all x > 0, P holds
some x: T, f x | P      // There exists x where f x is true and P holds
```

### Primed Expressions (State Transitions)

In chapters with a void procedure, primed expressions refer to post-state values:

```
User.
balance a: User => Int.
deposit a: User, amount: Nat.
---
balance' a = balance a + amount.    // balance after deposit
```

Only procedures can be primed, not variables.

### Function Overrides

Create a modified function:

```
mapping[k |-> v]        // mapping with k mapped to v
f[x |-> 0, y |-> 1]     // Multiple overrides
```

```
all k: Key, v: Value | mapping[k |-> v] k = v.
```

### Qualified Names

Reference entities from imported modules:

```
Module::name
Module::Type
```

## Propositions

The body of a chapter contains propositions that must be boolean expressions:

```
---
true.
all u: User | u in User.
#User >= 0.
balance a >= 0.
```

Each proposition ends with `.`

## Operator Precedence

From lowest to highest:

1. `->` (implication, right associative)
2. `or` (disjunction)
3. `and` (conjunction)
4. `not` (negation)
5. `= != < > <= >= in subset` (comparison/membership)
6. `+ -` (addition/subtraction)
7. `* /` (multiplication/division)
8. `#` (cardinality)
9. `-` (unary minus)
10. Application, projection

## Module System

### Imports

```
module MAIN.

import UTIL.
import TYPES.

// Can now use UTIL::name and TYPES::Type
```

### Module Path

Use `--module-path DIR` to specify where to find `.pant` files for imports.

### Visibility

All declarations in an imported module are available via qualified names.

## Complete Grammar

```
document    ::= 'module' UPPER '.' import* chapter ('where' chapter)*

import      ::= 'import' UPPER '.'

chapter     ::= declaration* '---' proposition*

declaration ::= UPPER '.'                              // Domain
              | UPPER '=' type '.'                     // Type alias
              | LOWER param* guard* ['=>' type] '.'   // Procedure

param       ::= LOWER ':' type

type        ::= type '+' type                          // Sum
              | type '*' type                          // Product
              | '[' type ']'                           // List
              | UPPER                                  // Named type
              | '(' type ')'                           // Grouped

proposition ::= expr '.'

expr        ::= 'all' bindings '|' expr               // Universal
              | 'some' bindings '|' expr              // Existential
              | expr '->' expr                         // Implication
              | expr 'or' expr                         // Disjunction
              | expr 'and' expr                        // Conjunction
              | 'not' expr                             // Negation
              | expr cmp expr                          // Comparison
              | expr '+' expr | expr '-' expr          // Additive
              | expr '*' expr | expr '/' expr          // Multiplicative
              | '#' expr                               // Cardinality
              | '-' expr                               // Negation
              | expr+                                  // Application
              | atom

cmp         ::= '=' | '!=' | '<' | '>' | '<=' | '>=' | 'in' | 'subset'

atom        ::= LOWER                                  // Variable
              | UPPER                                  // Domain
              | UPPER '::' (LOWER | UPPER)            // Qualified
              | NAT | REAL | STRING                   // Literals
              | 'true' | 'false'                      // Booleans
              | LOWER "'"                             // Primed
              | LOWER '[' override (',' override)* ']' // Override
              | '(' expr ')'                          // Grouped
              | '(' expr ',' expr (',' expr)* ')'     // Tuple
              | atom '.N'                             // Projection

bindings    ::= binding (',' (binding | guard))*
binding     ::= LOWER ':' type
guard       ::= expr  // Must be boolean
```
