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
module  import  where  context  initially  closure
true    false   and    or
all     some    in     subset
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
| `=>` | Rule return type |
| `~>` | Action |
| `->` | Implication |
| `<->` | Biconditional (iff) |
| `and` | Conjunction |
| `or` | Disjunction |
| `~` | Negation |
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

**Function types.** Rule declarations give rise to function types *(T₁, ..., Tₙ) → R*. Actions are not in the term namespace and have no function type. Function types are internal to the checker and cannot appear in user-written type expressions.

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

**Nullary rule.** If *f* is declared with no parameters and return type *R*, then *f* : *R*. Nullary rules are applied automatically on reference.

**Non-nullary rule.** If *f* has parameters, referencing *f* yields its function type. It must be applied to arguments to produce a value.

**Domain in expression position.** A domain name `D` used as an expression has type `[D]` — the list of all values of that domain. A type alias `A = T` in expression position has type `[T]`.

#### Tuples and Projection

**Construction.** `(`*e₁*`,` ... `,` *eₙ*`)` has type *T₁* `*` ··· `*` *Tₙ* where each *eᵢ* : *Tᵢ*.

**Projection.** *e*`.`*k* has type *Tₖ* where *e* : *T₁* `*` ··· `*` *Tₙ* and 1 ≤ *k* ≤ *n*.

#### Rule Application

*f e₁ ··· eₙ* where *f* : (*T₁*, ..., *Tₙ*) → *R*:

1. The argument count must equal the parameter count.
2. Each argument type must be a **subtype** of its parameter: if *eᵢ* : *Sᵢ*, then *Sᵢ* ≤ *Tᵢ*.
3. Result type: *R*.

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

`~`*e*: operand must be `Bool`; result is `Bool`.

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
5. The body must have type `Bool`. If the body is non-`Bool`, a type error is raised suggesting `each` for comprehensions.

#### Comprehensions (`each`)

`each` *bindings* `|` *body*:

The `each` keyword produces a **comprehension** (list). Its body may have any type *U*; the result type is always `[U]`.

`each x: D | f x` where `f : D → U`:
- Type: `[U]` — the list of values `f x` for every `x` in `D`.
- Usable anywhere a list is expected: as arguments, with `in` (membership), `#` (cardinality), `subset`, etc.
- Example: `r in (each u: User | role u)` — is `r` one of the roles assigned to any user?

`each x in xs, guard | f x` where `xs : [D]`, `f : D → U`:
- Type: `[U]` — the list of values `f x` for every `x` in `xs` satisfying the guard.
- Example: `#(each c in chapters, is-empty? c | c) = 0` — no chapters are empty.

Comprehensions cannot appear as standalone propositions (the body of a chapter must be `Bool`). They can appear as expressions anywhere a list is expected — as arguments, inside `in`, `#`, `subset`, etc. In SMT translation, they are expanded over finite domain elements.

#### Primed Expressions

*f*`'` (a primed name):

1. Must occur in a chapter whose head contains an action (an *action context*).
2. *f* must name a rule, not a variable.
3. Result type: same as the type of *f*.

Primed expressions denote the post-state value of a rule in a state transition.

#### Function Overrides

*f*`[`*k₁* `|->` *v₁*`,` ... `,` *kₙ* `|->` *vₙ*`]`:

1. *f* must be a rule of arity 1 with a return type: *f* : (*T*) → *R*.
2. Each key must be a **subtype** of the parameter type: *kᵢ* : *Sᵢ* with *Sᵢ* ≤ *T*.
3. Each value must be a **subtype** of the return type: *vᵢ* : *Uᵢ* with *Uᵢ* ≤ *R*.
4. Result type: (*T*) → *R*.

### Additional Constraints

**Propositions.** Every top-level expression in a chapter body must have type `Bool`.

**Variable shadowing.** When a binding introduces *x* : *S* and *x* is already in scope with type *T*, the new type must be a subtype of the existing type: *S* ≤ *T*. This permits rebinding at the same or a narrower type. Rebinding at a wider or unrelated type produces a **warning** (not a hard error), since different chapters may legitimately reuse variable names at different types.

**Action uniqueness.** Each chapter may have at most one action.

**Rule guards.** Guard expressions on rule declarations are type-checked with the rule's parameters in scope and must have type `Bool`.

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

### Rule Declaration

Declares a rule with typed parameters and a return type:

```
// With return type
owner d: Document => User.
distance p: Point, q: Point => Real.

// Nullary (no parameters)
nobody => User.
origin => Point.
```

**Syntax**: `name [params] [guards] => ReturnType.`

- Parameters: `name: Type` separated by commas
- Guards: boolean expressions, separated by commas after the parameters
- Return type: required, preceded by `=>`

### Closure Declaration

Declares a rule as the non-reflexive transitive closure of another rule:

```
parent b: Block => Block + Nothing.
ancestor b: Block => [Block] = closure parent.
```

**Syntax**: `name param => [T] = closure target.`

- The closure always returns `[T]` — the set of all elements reachable from the parameter by one or more applications of the target rule.
- The target rule must have one of these shapes:
  - `T => T + Nothing` — a partial endorelation (single parent, may be absent)
  - `T => [T]` — a multi-valued endorelation (multiple children)
- The parameter type must match the domain type `T` of the target.
- The closure is **non-reflexive**: `ancestor b` does not include `b` itself (unless `b` is reachable from itself through a cycle).
- Closure rules are **derived** — they do not receive frame conditions in SMT verification. Their post-state (`ancestor'`) is automatically derived from the target's post-state (`parent'`).
- Closure rules can be primed in action chapters without requiring context membership.

**Example** — acyclicity invariant:

```
Block.
parent b: Block => Block + Nothing.
ancestor b: Block => [Block] = closure parent.
---
all b: Block | ~(b in ancestor b).
```

### Actions

An action (state transition) is introduced with `~>`. Actions have no return type and enable primed expressions for rules in the chapter body. Action labels are free-form text (spaces, capitals, keywords all allowed), separated from parameters by `|`:

```
// Action (no context)
~> Check out | u: User, d: Document.
~> Deposit | a: Account, amount: Nat.
~> Do something.

// Action with context
Accounts ~> Withdraw | a: Account, amount: Nat.
```

**Syntax**: `[Context] ~> label [| params [, guards]].`

- Action labels are purely human-readable annotations — they are not in the term namespace
- Actions must appear **last** in a chapter head
- Each chapter may have at most one action

### Rule and Action Guards

Guards constrain when a rule or action applies. Guards must be boolean expressions and are type-checked with the rule's parameters in scope:

```
~> Withdraw | a: Account, amount: Nat, balance a >= amount.
```

Guards can reference the rule's parameters (`a`, `amount` in this example) to express preconditions.

### Context Declaration

Declares a named context at the module level, after imports:

```
module BANKING.
context Accounts.
```

Context names are uppercase identifiers. They define write-permission boundaries: rules declare which contexts they belong to, and actions specify which context they operate within.

### Context Footprint

Rules declare context membership with a `{Ctx, ...}` prefix:

```
{Accounts} balance a: Account => Nat.
{Accounts} owner a: Account => User.
```

A rule may belong to multiple contexts: `{Accounts, Audit} balance a: Account => Nat.`

Context footprint is closed within module scope — you can only add a rule to a context declared in the same module.

### Context Annotation (Actions)

Actions specify which context they operate within using a `Ctx ~>` prefix:

```
Accounts ~> Withdraw | a: Account, amount: Nat.
```

This means the `Withdraw` action may modify (prime) any rule that belongs to `Accounts`. Context references work across module boundaries — an action can reference an imported context.

Only actions may have a context prefix. Rules (with `=>`) cannot operate within a context.

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

### Rule Application

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
~p                  // Negation
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

### Comprehensions (`each`)

The `each` keyword produces a list from bindings and a body expression:

```
// List comprehension: [Role]
each u: User | role u

// Filtered comprehension: [User]
each u in users, active u | u

// Used with membership, cardinality, subset
r in (each u: User | role u).           // Is r a role of any user?
#(each u in users, active u | u) > 0.   // At least one active user
(each u: User | role u) subset Role.    // All assigned roles are valid
```

### Primed Expressions (State Transitions)

In chapters with an action, primed expressions refer to post-state values:

```
User.
balance a: User => Int.
~> Deposit | a: User, amount: Nat.
---
balance' a = balance a + amount.    // balance after deposit
```

Only rules can be primed, not variables.

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

### Initial-State Propositions

A proposition prefixed with `initially` declares a constraint on the initial state:

```
---
all a: Account | balance a >= 0.
initially all a: Account | balance a = 0.
```

Initial-state propositions are **not** invariants — actions do not need to preserve them. They serve as the base case of an inductive argument: the `initially` propositions define what the initial state looks like, and the checker verifies that this initial state satisfies all invariants.

When `--check` is used with a document containing `initially` propositions, two additional checks are performed:

1. **Init consistency**: The initial-state propositions together with type constraints are satisfiable (i.e., a valid initial state exists).
2. **Init satisfies invariants**: For each invariant, the initial state satisfies it (i.e., the base case holds).

Initial-state propositions are collected across all chapters, just like invariants.

## Operator Precedence

From lowest to highest:

1. `<->` (biconditional, non-associative)
2. `->` (implication, right associative)
3. `or` (disjunction)
4. `and` (conjunction)
5. `~` (negation)
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
| **Rule names** | M where M ≥ N | M where M ≥ N - 1 |
| **Rule parameters** | M where M ≥ N | M where M ≥ N |

This means:
- A declaration in chapter 2 is visible in heads of chapters 2, 3, 4, ...
- A declaration in chapter 2 is visible in bodies of chapters 1, 2, 3, 4, ...
- Bodies can "look ahead" one chapter for rule names
- But rule parameters are only visible in the same chapter's body

For a rule `foo x: T, y: U => R` declared in chapter N:
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
4. Each chapter has at most one action

## Normal Form

The `--normalize "root term"` flag transforms a document into **top-down normal form** with respect to a chosen root term.

### Principles

Normal form is governed by two reinforcing principles:

1. **Progressive disclosure.** The reader encounters the high-level story first. Each chapter body previews terms that the next chapter's head will define, so the reader always sees *why* a term matters before seeing *what* it is.

2. **Minimality.** Each chapter contains no more than it has to. A proposition appears in a chapter only if it cannot appear earlier (its dependencies aren't yet visible) and should not appear later (that would give a later chapter more than it needs).

These two principles converge on a unique placement for every proposition: the earliest chapter where all of its dependencies are visible. This is the chapter *before* the one declaring the proposition's latest dependency, since bodies can see one chapter ahead via forward declaration (see [Forward Declaration Rules](#forward-declaration-rules)).

### Definition

A document is in top-down normal form with respect to a root term when:

1. **Chapter 0 head** contains the root term and the transitive closure of its declaration-level dependencies
2. **Chapter *N* body** contains the propositions whose latest dependency is declared in chapter *N*+1 (or *N*, if all dependencies are at level *N* or earlier)
3. **Chapter *N*+1 head** declares the symbols referenced in chapter *N*'s body but not yet declared, plus their transitive declaration dependencies for well-formedness
4. Actions and their tied propositions (postconditions, frame conditions) stay together in the same chapter
5. At most one action per chapter
6. Unreachable declarations (not reachable from the root term) appear in a final appendix chapter

The body of each chapter thus serves a dual role: it makes statements about the current chapter's terms, and it motivates the next chapter's existence by referencing terms the reader hasn't seen yet.

### Level Assignment (BFS)

Starting from the root term, declaration levels are assigned by breadth-first search:

| Level | Contents |
|-------|----------|
| 0 | Root term + transitive closure of its declaration-level dependencies (types in signature, terms in guards) |
| 1 | Symbols referenced in level-0 propositions but not yet assigned, plus their transitive declaration deps |
| 2 | Symbols referenced in level-1 propositions but not yet assigned, plus their transitive declaration deps |
| ... | Continues until no new symbols are discovered |
| *N*+1 | Appendix: declarations not reachable from the root term |

**Transitive declaration dependencies** ensure head well-formedness. If the root term's signature references type `Card` and `Card = Nat * CardStatus`, then `CardStatus` is also included at level 0.

The BFS guarantees that any proposition discovered at level *N* references only terms at levels *N* and *N*+1. This is exactly the gap that forward declaration covers, ensuring every proposition has a valid placement.

### Proposition Placement

For each proposition, let *L* be the maximum level among the terms it references.

| Proposition type | Placement rule |
|------------------|----------------|
| **Action-tied** | Same chapter as its action |
| **Independent** | Chapter max(0, *L* − 1) |

A proposition is **action-tied** if it uses primed expressions (e.g., `balance' a`) or references the action's parameters.

An independent proposition goes in the body of the chapter *before* the one declaring its latest dependency, exploiting forward declaration so the body previews terms glossed in the next head. When all dependencies are at level 0, the proposition goes in chapter 0.

### Action Handling

If multiple actions end up at the same level, they are spread across consecutive chapters (since at most one action per chapter is allowed).

### Normalization Algorithm

1. **Flatten** all declarations and propositions from all chapters
2. **Compute transitive declaration deps** for the root term → level 0
3. **BFS loop**: for each level *N*, find unscanned propositions referencing level-*N* symbols; collect new symbols from those propositions + their transitive deps → level *N*+1; stop when no new symbols appear
4. **Assign unreachable** declarations to an appendix chapter
5. **Spread actions** across consecutive chapters where needed
6. **Place propositions**: tied props with their action, independent props at chapter max(0, *L* − 1) where *L* is the max level of referenced terms
7. **Build chapters**: within each level, declarations sorted by source position, action last; filter empty chapters

### Example

Given `samples/02-library.pant`, normalizing with `--normalize "Borrow"` produces:

**Chapter 0**: The `Borrow` action and the domains it directly references (`Book`, `User`). The body contains propositions about borrowing — which reference `available`, `borrower`, etc.

**Chapter 1**: Declares `available`, `borrower`, and their supporting types (glossing the terms previewed in chapter 0's body). The body references further terms.

**Chapter 2+**: Further supporting terms, glossed progressively until all dependencies are resolved.

Any declarations not reachable from `Borrow` appear in a final appendix chapter.

## Complete Grammar

```
document    ::= 'module' UPPER '.' import* context_decl* chapter ('where' chapter)*

import      ::= 'import' UPPER '.'

context_decl ::= 'context' UPPER '.'

chapter     ::= declaration+ '---' proposition*    // Head must be non-empty

declaration ::= UPPER '.'                                              // Domain
              | UPPER '=' type '.'                                     // Type alias
              | '{' UPPER (',' UPPER)* '}' LOWER param* guard* '=>' type '.'  // Rule with context footprint
              | LOWER param* guard* '=>' type '.'                      // Rule with return type
              | LOWER param '=>' type '=' 'closure' LOWER '.'        // Closure (transitive closure of target)
              | UPPER '~>' LABEL '|' param ((',' param) | (',' guard))* '.'  // Action with context + params
              | UPPER '~>' LABEL '.'                                   // Action with context, no params
              | '~>' LABEL '|' param ((',' param) | (',' guard))* '.'  // Action + params
              | '~>' LABEL '.'                                         // Action, no params

param       ::= LOWER ':' type

type        ::= type '+' type                          // Sum
              | type '*' type                          // Product
              | '[' type ']'                           // List
              | UPPER                                  // Named type
              | '(' type ')'                           // Grouped

proposition ::= expr '.'
              | 'initially' expr '.'                       // Initial-state constraint

expr        ::= 'all' bindings '|' expr               // Universal
              | 'some' bindings '|' expr              // Existential
              | 'each' bindings '|' expr              // Comprehension
              | expr '<->' expr                        // Biconditional
              | expr '->' expr                         // Implication
              | expr 'or' expr                         // Disjunction
              | expr 'and' expr                        // Conjunction
              | '~' expr                               // Negation
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
