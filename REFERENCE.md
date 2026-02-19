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
module  import  where  context  true  false
and     or      not    all      some
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

| Operator | Meaning |
|----------|---------|
| `=>` | Procedure return type |
| `->` | Implication |
| `<->` | Biconditional (iff) |
| `and` | Conjunction |
| `or` | Disjunction |
| `not`, `~` | Negation |
| `all` | Universal quantifier |
| `some` | Existential quantifier |
| `in` | Membership |
| `subset` | Subset |
| `=` | Equality |
| `!=` | Inequality |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |
| `+` | Addition / Sum type |
| `-` | Subtraction / Negation |
| `*` | Multiplication / Product type |
| `/` | Division |
| `#` | Cardinality |
| `'` | Prime (post-state) |
| `\|->` | Maps-to (in overrides) |

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

## Type System

### Type Universe

Every expression in Pantagruel is assigned a type from the following universe.

**Primitive types.** Seven built-in types are predefined. Their names are reserved and cannot be redeclared.

| Type | Description |
|------|-------------|
| `Bool` | Boolean values (`true`, `false`) |
| `Nat` | Positive integers (1, 2, 3, ...) |
| `Nat0` | Non-negative integers (0, 1, 2, ...) |
| `Int` | All integers (..., −1, 0, 1, ...) |
| `Real` | Real numbers |
| `String` | Text strings |
| `Nothing` | The empty type (uninhabited — no values) |

**Domain types.** Each domain declaration `D.` introduces a distinct type `D`. Domain types are pairwise unrelated, and unrelated to all built-in types.

**Type aliases.** A declaration `A = T.` defines `A` as a synonym for `T`. Aliases are fully expanded during checking; `A` and `T` are interchangeable everywhere.

**Compound types.** Three constructors build composite types:

| Constructor | Syntax | Example |
|-------------|--------|---------|
| List | `[T]` | `[User]`, `[[Nat]]` |
| Product | `T * U`, `T * U * V`, ... | `Nat * Nat`, `Point * Color` |
| Sum | `T + U`, `T + U + V`, ... | `Value + Nothing` |

Product and sum types must have at least two components. They are **positional**: `Nat * Bool` and `Bool * Nat` are distinct types.

Products are constructed with parentheses — `(1, 2)` — and accessed with projection — `p.1`, `p.2` (1-indexed).

**Function types.** Procedure declarations give rise to function types *(T₁, ..., Tₙ) → R* (returning) or *(T₁, ..., Tₙ) → Void* (void). Function types are internal to the checker and cannot appear in user-written type expressions.

### Subtyping

The **subtype** relation *S* ≤ *T*, read "*S* fits where *T* is expected," is a partial order on types. It is defined by the following rules, closed under reflexivity and transitivity.

**Reflexivity.** Every type is a subtype of itself: *T* ≤ *T*.

**Bottom.** `Nothing` is a subtype of every type: `Nothing` ≤ *T* for all *T*. Since `Nothing` is uninhabited, this is vacuously safe — there is no `Nothing` value that could violate the expectations of *T*.

**Numeric chain.** The numeric types form a chain of strict inclusions:

    Nat  <  Nat0  <  Int  <  Real

Each `Nat` value is also a `Nat0` value, each `Nat0` value is also an `Int` value, and each `Int` value is also a `Real` value. Transitivity gives `Nat` ≤ `Int`, `Nat` ≤ `Real`, and `Nat0` ≤ `Real`.

**Covariance.** Subtyping lifts through compound types, position by position:

- `[S]` ≤ `[T]`  when  *S* ≤ *T*
- `S₁ * ··· * Sₙ` ≤ `T₁ * ··· * Tₙ`  when  *Sᵢ* ≤ *Tᵢ* for each *i* (same *n* on both sides)
- `S₁ + ··· + Sₙ` ≤ `T₁ + ··· + Tₙ`  when  *Sᵢ* ≤ *Tᵢ* for each *i* (same *n* on both sides)

**No other subtyping holds.** In particular:

- `Bool`, `String`, and each domain type are pairwise incomparable.
- Numeric and non-numeric types are incomparable (e.g. `Nat` ≰ `Bool`).
- Function types do not participate in subtyping.

#### Subtyping examples

| Statement | Holds? | Reason |
|-----------|--------|--------|
| `Nat` ≤ `Int` | Yes | Numeric chain (transitive) |
| `[Nat]` ≤ `[Real]` | Yes | `Nat` ≤ `Real`, covariance |
| `Nat * Bool` ≤ `Int * Bool` | Yes | `Nat` ≤ `Int` and `Bool` ≤ `Bool` |
| `Int` ≤ `Nat` | No | `Int` is above `Nat` in the chain |
| `Bool` ≤ `Nat` | No | Incomparable |
| `Nothing` ≤ `[User]` | Yes | Bottom type |
| `User` ≤ `String` | No | Incomparable domain types |

### Least Upper Bound (Join)

The **join** of two types, written *S* ⊔ *T*, is the smallest type that both *S* and *T* are subtypes of. When the join exists, the two types are **compatible**. When no join exists, the types are **incompatible** — they cannot be meaningfully combined.

**Equal types.** *T* ⊔ *T* = *T*.

**Subtype pairs.** When *S* ≤ *T*, then *S* ⊔ *T* = *T*. This covers the numeric chain (`Nat` ⊔ `Int` = `Int`, `Nat0` ⊔ `Real` = `Real`) and the bottom type (`Nothing` ⊔ *T* = *T*).

**Compound types.** The join is computed position by position:

- `[S]` ⊔ `[T]` = `[`*S* ⊔ *T*`]`
- (*S₁* `*` ··· `*` *Sₙ*) ⊔ (*T₁* `*` ··· `*` *Tₙ*) = (*S₁* ⊔ *T₁*) `*` ··· `*` (*Sₙ* ⊔ *Tₙ*)
- (*S₁* `+` ··· `+` *Sₙ*) ⊔ (*T₁* `+` ··· `+` *Tₙ*) = (*S₁* ⊔ *T₁*) `+` ··· `+` (*Sₙ* ⊔ *Tₙ*)

Both sides must have the same number of components, and each component pair must itself have a join.

**Undefined.** Examples of incompatible types (no join): `Bool` and `Nat`; `User` and `String`; `[Bool]` and `[User]`.

### Typing Rules

The rules below assign a type to each expression, or produce a type error when no rule applies. They rely on the three relations defined above:

| Relation | Notation | Meaning | Symmetry |
|----------|----------|---------|----------|
| Subtype | *S* ≤ *T* | *S* fits where *T* is expected | Directional |
| Join | *S* ⊔ *T* | Smallest common supertype | Symmetric |
| Compatibility | *S* ~ *T* | *S* ⊔ *T* exists | Symmetric |

Summary of which relation governs each context:

| Relation | Where used |
|----------|------------|
| *S* ≤ *T* (subtype) | Argument passing, `in`, `subset`, overrides, variable shadowing |
| *S* ⊔ *T* (join) | Arithmetic result types |
| *S* ~ *T* (compatible) | `=` and `!=` operands |

#### Literals

| Expression | Type | Note |
|------------|------|------|
| `true`, `false` | `Bool` | |
| `0` | `Nat0` | Zero is non-negative, not positive |
| *n* (integer, *n* > 0) | `Nat` | |
| *r* (real literal) | `Real` | |
| *s* (string literal) | `String` | |

#### Names

**Variable.** If *x* is bound to type *T* in the current scope, then *x* : *T*.

**Nullary procedure.** If *f* is declared with no parameters and return type *R*, then *f* : *R*. Nullary procedures are applied automatically on reference.

**Non-nullary procedure.** If *f* has parameters, referencing *f* yields its function type. It must be applied to arguments to produce a value.

**Domain in expression position.** A domain name `D` used as an expression has type `[D]` — the list of all values of that domain. A type alias `A = T` in expression position has type `[T]`.

#### Tuples and Projection

**Construction.** `(`*e₁*`,` ... `,` *eₙ*`)` has type *T₁* `*` ··· `*` *Tₙ* where each *eᵢ* : *Tᵢ*.

**Projection.** *e*`.`*k* has type *Tₖ* where *e* : *T₁* `*` ··· `*` *Tₙ* and 1 ≤ *k* ≤ *n*.

#### Procedure Application

*f e₁ ··· eₙ* where *f* : (*T₁*, ..., *Tₙ*) → *R*:

1. The argument count must equal the parameter count.
2. Each argument type must be a **subtype** of its parameter: if *eᵢ* : *Sᵢ*, then *Sᵢ* ≤ *Tᵢ*.
3. The return type must not be Void — void procedures cannot appear in expression position.
4. Result type: *R*.

#### List Application

*xs e* where *xs* : `[T]` (exactly one argument):

- **Indexing.** If *e* : *S* with *S* ≤ `Nat`, result type is *T*. Lists are 1-indexed, so the index type is `Nat` (positive integers only — `Nat0` is not accepted).
- **Search.** If *T* is not numeric and *e* : *S* with *S* ≤ *T*, result type is `Nat + Nothing` — the 1-based position of the element if found, or `Nothing` if absent.
- When *T* is numeric, only indexing is available. Search is disallowed because the argument would be ambiguous between an index and an element to search for.

#### Arithmetic

*e₁* `op` *e₂* where `op` is `+`, `-`, `*`, or `/`:

1. Both operands must be numeric.
2. Result type: *T₁* ⊔ *T₂*. For example, `Nat + Int` yields `Int`; `Nat * Real` yields `Real`.

#### Comparison

*e₁* `op` *e₂* where `op` is `<`, `>`, `<=`, or `>=`:

1. Both operands must be numeric (but need not be related to each other).
2. Result type: `Bool`.

#### Equality and Inequality

*e₁* `=` *e₂* and *e₁* `!=` *e₂*:

1. The operand types must be **compatible**: *T₁* ⊔ *T₂* must exist.
2. Result type: `Bool`.

This is a symmetric check. `Nat = Int` is valid (both numeric, join is `Int`), but `Bool = Nat` is a type error (no join exists).

#### Logical Operators

*e₁* `op` *e₂* where `op` is `and`, `or`, `->`, or `<->`: both operands must be `Bool`; result is `Bool`.

`not` *e*: operand must be `Bool`; result is `Bool`.

#### Membership

*e₁* `in` *e₂*:

1. *e₂* must have type `[T]`.
2. *e₁* must have type *S* with *S* ≤ *T* (**directional** — a `Nat` may be tested for membership in `[Int]`, but an `Int` may not be tested for membership in `[Nat]`).
3. Result type: `Bool`.

#### Subset

*e₁* `subset` *e₂*:

1. *e₁* : `[S]` and *e₂* : `[T]`.
2. *S* ≤ *T* (directional).
3. Result type: `Bool`.

#### Cardinality

`#`*e*: *e* must have type `[T]`; result type is `Nat0` (since the empty list has cardinality zero).

#### Unary Minus

`-`*e*:

1. *e* must be numeric.
2. If *e* : *T* with *T* ≤ `Int`, result type is `Int` — negating a natural or non-negative integer may produce a negative number.
3. If *e* : `Real`, result type is `Real`.

#### Quantifiers

`all` *bindings* `|` *body* (and `some` *bindings* `|` *body*):

1. Each typed binding *x* `:` *T* introduces *x* with type *T*.
2. Each membership binding *x* `in` *e*, where *e* : `[T]`, introduces *x* with type *T*.
3. Guard expressions (bare boolean conditions in the binding list) must have type `Bool`.
4. Bindings and guards are processed left to right; each may reference names introduced earlier in the list.
5. The body must have type `Bool`.
6. Result type: `Bool`.

#### Primed Expressions

*f*`'` (a primed name):

1. Must occur in a chapter whose head declares a void procedure (a *void context*).
2. *f* must name a procedure, not a variable.
3. Result type: same as the type of *f*.

Primed expressions denote the post-state value of a procedure in a state transition.

#### Function Overrides

*f*`[`*k₁* `|->` *v₁*`,` ... `,` *kₙ* `|->` *vₙ*`]`:

1. *f* must be a procedure of arity 1 with a return type: *f* : (*T*) → *R*.
2. Each key must be a **subtype** of the parameter type: *kᵢ* : *Sᵢ* with *Sᵢ* ≤ *T*.
3. Each value must be a **subtype** of the return type: *vᵢ* : *Uᵢ* with *Uᵢ* ≤ *R*.
4. Result type: (*T*) → *R*.

### Additional Constraints

**Propositions.** Every top-level expression in a chapter body must have type `Bool`.

**Variable shadowing.** When a binding introduces *x* : *S* and *x* is already in scope with type *T*, the new type must be a subtype of the existing type: *S* ≤ *T*. This permits rebinding at the same or a narrower type, but forbids rebinding at a wider or unrelated type.

**Void procedure uniqueness.** Each chapter may declare at most one void procedure.

**Procedure guards.** Guard expressions on procedure declarations are type-checked with the procedure's parameters in scope and must have type `Bool`.

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

**Syntax**: `name [params] [guards] [=> ReturnType].`

- Parameters: `name: Type` separated by commas
- Guards: boolean expressions, separated by commas after the parameters
- Return type: Optional, preceded by `=>`
- Void procedures have no return type and enable primed expressions

### Procedure with Guards

Guards constrain when a procedure applies. Guards must be boolean expressions and are type-checked with the procedure's parameters in scope:

```
withdraw a: Account, amount: Nat, balance a >= amount.
```

Guards can reference the procedure's parameters (`a`, `amount` in this example) to express preconditions.

### Context Declaration

Declares a named context at the module level, after imports:

```
module BANKING.
context Accounts.
```

Context names are uppercase identifiers. They define write-permission boundaries: procedures declare which contexts they belong to, and void procedures declare which context they operate within.

### Context Footprint

Non-void procedures declare context membership with a `{Ctx, ...}` prefix:

```
{Accounts} balance a: Account => Nat.
{Accounts} owner a: Account => User.
```

A procedure may belong to multiple contexts: `{Accounts, Audit} balance a: Account => Nat.`

Context footprint is closed within module scope — you can only add a procedure to a context declared in the same module.

### Context Annotation (Void Procedures)

Void procedures declare which context they operate within using `in Ctx`:

```
withdraw a: Account, amount: Nat in Accounts.
```

This means `withdraw` may modify (prime) any procedure that belongs to `Accounts`. Context references work across module boundaries — a void procedure can reference an imported context.

Only void procedures may have `in Ctx`. Non-void procedures cannot operate within a context (this is enforced at the syntax level).

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
f x y z             // Multiple arguments
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

Reference entities from imported modules (not yet implemented in the checker):

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

### Visibility

Imported declarations are merged into the importing module's namespace and can be referenced directly by name.

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

## Complete Grammar

```
document    ::= 'module' UPPER '.' import* context_decl* chapter ('where' chapter)*

import      ::= 'import' UPPER '.'

context_decl ::= 'context' UPPER '.'

chapter     ::= declaration+ '---' proposition*    // Head must be non-empty

declaration ::= UPPER '.'                                              // Domain
              | UPPER '=' type '.'                                     // Type alias
              | '{' UPPER (',' UPPER)* '}' LOWER param* guard* '=>' type '.'  // Proc with context footprint
              | LOWER param* guard* '=>' type '.'                      // Proc with return type
              | LOWER param* guard* ['in' UPPER] '.'                   // Void proc (optional context)

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
