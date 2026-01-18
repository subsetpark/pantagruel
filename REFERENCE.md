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
| `<->` | `↔` | Biconditional (iff) |
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

Guards constrain when a procedure applies. Guards must be boolean expressions and are type-checked with the procedure's parameters in scope:

```
withdraw a: Account, amount: Nat, balance a >= amount.
```

Guards can reference the procedure's parameters (`a`, `amount` in this example) to express preconditions.

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
p <-> q             // Biconditional (if and only if)
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

// With guards (boolean conditions)
all x: T, x > 0 | P     // For all x > 0, P holds
some x: T, f x | P      // There exists x where f x is true and P holds

// Membership bindings: x in xs where xs: [T] binds x: T
all i in items | price i > 0.     // For all i in the list items
some i in items | price i < 100.  // There exists i in items
```

The `x in xs` binding form infers the type of `x` from the element type of the list `xs`. If `xs: [T]`, then `x: T`.

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

1. `<->` (biconditional, non-associative)
2. `->` (implication, right associative)
3. `or` (disjunction)
4. `and` (conjunction)
5. `not` (negation)
6. `= != < > <= >= in subset` (comparison/membership)
7. `+ -` (addition/subtraction)
8. `* /` (multiplication/division)
9. `#` (cardinality)
10. `-` (unary minus)
11. Application, projection

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

## Forward Declaration Rules

Pantagruel supports forward declaration with specific visibility rules based on chapter structure.

### Chapter Numbering

Chapters are numbered starting from 0. A document with 3 chapters has chapters 0, 1, and 2.

### Visibility Rules

Symbols declared in chapter N have different visibility rules:

| Symbol Type | Visible in Heads | Visible in Bodies |
|-------------|------------------|-------------------|
| **Domains/Aliases** | M where M ≥ N | M where M ≥ N - 1 |
| **Procedure names** | M where M ≥ N | M where M ≥ N - 1 |
| **Procedure parameters** | M where M ≥ N | M where M ≥ N |

This means:
- A declaration in chapter 2 is visible in heads of chapters 2, 3, 4, ...
- A declaration in chapter 2 is visible in bodies of chapters 1, 2, 3, 4, ...
- Bodies can "look ahead" one chapter for procedure names
- But procedure parameters are only visible in the same chapter's body

For a procedure `foo x: T, y: U => R` declared in chapter N:
- `foo` is visible starting from chapter N-1 body (one chapter look-ahead)
- `x` and `y` are only visible in chapter N body (same chapter, no look-ahead)

### Example

```
module EXAMPLE.

// Chapter 0
User.
---
all u: User | u in User.        // OK: User declared in chapter 0

where

// Chapter 1
owner d: Document => User.      // OK: User visible in head (chapter 0 ≤ 1)
Document.                       // OK: forward declaration
---
all d: Document | owner d in User.  // OK: Document visible (declared in chapter 1, body is chapter 1)
```

### Correctness

A program is **correct** if:
1. Every symbol reference is visible according to the rules above
2. All type constraints are satisfied
3. Each chapter head contains at least one declaration
4. Each chapter has at most one void procedure

## Normal Form

A document can be transformed into **normal form**, where declarations are organized by dependency levels and propositions are placed at their earliest valid position.

### Definition

A document is in normal form when:
1. Declarations are organized by **topological level** (based on type dependencies)
2. Each level becomes a chapter
3. Propositions are placed in the **earliest** chapter where all their dependencies are visible
4. Void procedures and their tied propositions stay together
5. At most one void procedure per chapter

### Dependency Levels

Declarations are assigned levels based on their type dependencies:

| Level | Description |
|-------|-------------|
| 0 | Declarations with no type dependencies (domains) |
| 1 | Declarations that only depend on level 0 types |
| 2 | Declarations that depend on level 1 types |
| ... | And so on |

### Proposition Placement

Propositions are placed at the **earliest** valid chapter:

| Proposition type | Placement rule |
|------------------|----------------|
| **Void-tied** | Same chapter as the void procedure |
| **Independent** | Earliest chapter where all dependencies are visible |

A proposition is **void-tied** if it:
- Uses primed expressions (e.g., `balance' a`), OR
- References the void procedure's parameters

### Void Procedure Handling

If multiple void procedures end up at the same dependency level, they are spread across consecutive chapters (since at most one void procedure per chapter is allowed).

### Normalization Algorithm

1. **Collect declarations** and compute their type dependencies
2. **Compute topological levels** for all declarations
3. **Create chapters** - one per level (plus extra for void proc conflicts)
4. **Place declarations** at their computed level
5. **Place void-tied propositions** with their void procedures
6. **Place independent propositions** at earliest valid chapter

### Example

**Original (3 chapters with void procedures):**
```
module STATE.

User.
Account.
balance a: Account => Int.
owner a: Account => User.
deposit a: Account, amount: Nat.
---
balance' a = balance a + amount.
all a: Account, amt: Nat | true.

where

withdraw a: Account, amount: Nat.
---
balance' a = balance a - amount.

where

transfer from: Account, to: Account, amount: Nat.
---
balance' from = balance from - amount.
```

**Normal form (4 chapters):**
```
module STATE.

// Level 0: root domains
Account.
User.
---
all a: Account, amt: Nat | true.   // Only depends on Account, Nat

where

// Level 1: depends on Account, User
balance a: Account => Int.
owner a: Account => User.
deposit a: Account, amount: Nat.   // Void proc #1
---
balance' a = balance a + amount.   // Tied to deposit

where

// Void proc #2 (separate chapter)
withdraw a: Account, amount: Nat.
---
balance' a = balance a - amount.

where

// Void proc #3 (separate chapter)
transfer from: Account, to: Account, amount: Nat.
---
balance' from = balance from - amount.
```

The independent proposition `all a: Account, amt: Nat | true` moved to chapter 0 because it only depends on level 0 types. Void procedures were spread across separate chapters.

### CLI Usage

```bash
pantagruel --normalize file.pant
```

Outputs the N-normal form of the document.

## Complete Grammar

```
document    ::= 'module' UPPER '.' import* chapter ('where' chapter)*

import      ::= 'import' UPPER '.'

chapter     ::= declaration+ '---' proposition*    // Head must be non-empty

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
              | expr '<->' expr                        // Biconditional
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
binding     ::= LOWER ':' type              // Type annotation
              | LOWER 'in' expr             // Membership binding (expr must be a list)
guard       ::= expr  // Must be boolean
```
