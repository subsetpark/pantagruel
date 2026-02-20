# Pantagruel

A specification language checker for formal system descriptions.

Pantagruel lets you write precise specifications of systems using domains, rules, and logical propositions. The checker validates that your specifications are well-typed and internally consistent.

## Installation

Requires OCaml 4.14+ and opam.

```bash
# Install dependencies
opam install menhir sedlex ppx_deriving yojson sexplib0 parsexp alcotest

# Build
dune build

# Run tests
dune test

# Install globally (optional)
dune install
```

## Usage

```bash
# Type-check a specification file
pant myspec.pant

# Read from stdin
echo 'module TEST. Foo. ---' | pant

# Run bounded model checking (requires z3 or cvc5)
pant --check myspec.pant
pant --check --bound 5 myspec.pant        # Set domain element bound (default: 3)
pant --check --solver cvc5 myspec.pant     # Use alternative solver

# Output formats
pant --json myspec.pant                    # JSON with resolved types and full AST
pant --markdown myspec.pant                # Rich Markdown with Unicode symbols
pant --format myspec.pant                  # Reformat with standard style
pant --normalize myspec.pant               # Output N-normal form
pant --ast myspec.pant                     # Print AST (OCaml format, for debugging)

# Specify module search path for imports
pant --module-path ./specs myspec.pant
```

## Samples

The `samples/` directory contains reference specifications:

- `01-basics.pant` - Fundamental syntax: domains, rules, quantifiers
- `02-library.pant` - Library system with actions and state transitions
- `03-types.pant` - All type features: products, sums, lists, aliases
- `04-operators.pant` - All operators: logical, comparison, arithmetic
- `05-state.pant` - State transitions with primed expressions and frame conditions
- `06-advanced.pant` - Function overrides and qualified names
- `07-pantagruel.pant` - Self-specification of the Pantagruel language
- `08-contexts.pant` - Context declarations and write-permission boundaries

The `samples/smt-examples/` directory demonstrates bounded model checking:

- `contradiction.pant` - Conflicting postconditions detected by `--check`
- `invariant-violation.pant` - Missing precondition allows invariant violation
- `dead-operation.pant` - Unreachable action due to unsatisfiable precondition
- `underspecified.pant` - Well-specified banking example with contexts and frame conditions

## Language Overview

A Pantagruel document consists of a module declaration, optional imports, and one or more chapters. Each chapter has a **head** (declarations) and a **body** (propositions), separated by `---`.

### Basic Structure

```
module LIBRARY.

import USERS.

> Domains
Book.
Loan.

> Rules
available b: Book => Bool.
~> Borrow | u: User, b: Book.
---
> Propositions about the library
all b: Book | available b -> b in Book.
```

### Declarations

**Domains** define the basic types in your system:
```
User.
Document.
```

**Type aliases** create composite types:
```
Point = Nat * Nat.           // Product type (tuple)
Result = Value + Error.      // Sum type
UserList = [User].           // List type
```

**Rules** define operations with typed parameters:
```
// With return type
owner d: Document => User.
distance p: Point, q: Point => Real.

// Nullary (no parameters)
nobody => User.
```

**Actions** define state transitions (no return type). Action labels are free-form text, separated from parameters by `|`:
```
// Action (with ~>) - for state transitions
~> Check out | u: User, d: Document.
~> Do something.
```

### Propositions

The body contains logical propositions that must be boolean:

```
// Literals
true.
false.

// Quantifiers
all u: User | u in User.
some d: Document | owner d = nobody.

// Comparisons
all x: Nat, y: Nat | x + y >= x.

// Membership and cardinality
all u: User | u in User.
#User >= 0.
```

### Actions and Primed Expressions

Actions model state transitions. Within a chapter that has an action, you can use **primed expressions** to refer to the post-state of rules:

```
User.
Document.
owner d: Document => User.
~> Check out | u: User, d: Document.
---
// owner' refers to owner after check-out
owner' d = u.
```

### Contexts

Contexts define write-permission boundaries for actions. Context names are declared at the module level, and rules declare which contexts they belong to with a `{Ctx}` prefix. Actions specify which context they operate within using `Ctx ~>`.

```
module BANKING.
context Accounts.

Account.
{Accounts} balance a: Account => Nat.

Accounts ~> Withdraw | a: Account, amount: Nat.
---
balance' a = balance a - amount.
```

### Operators

| Category | Operators |
|----------|-----------|
| Logical | `and`, `or`, `not`, `->` (implication) |
| Comparison | `=`, `!=`, `<`, `>`, `<=`, `>=` |
| Membership | `in`, `subset` |
| Arithmetic | `+`, `-`, `*`, `/` |
| Other | `#` (cardinality), `.N` (projection) |

### Comments

```
// Line comment
> Doc comment (at start of line)
```

### Built-in Types

- `Bool` - boolean values
- `Nat` - positive integers (1, 2, 3, ...)
- `Nat0` - non-negative integers (0, 1, 2, ...)
- `Int` - all integers
- `Real` - real numbers
- `String` - text strings
- `Nothing` - empty/null type

Numeric types form a hierarchy: `Nat < Nat0 < Int < Real`

## Module System

Specifications can import other modules:

```
module INVENTORY.

import PRODUCTS.
import WAREHOUSE.

Item.
stock i: Item => Nat0.
---
all i: Item | stock i >= 0.
```

Use `--module-path` to specify where to find imported `.pant` files.

## Examples

### Simple Domain Model

```
module ACCOUNTS.

Account.
balance a: Account => Int.
~> Deposit | a: Account, amount: Nat.
---
// Balance increases after deposit
balance' a = balance a + amount.

// Other accounts unchanged
all other: Account | other != a -> balance' other = balance other.
```

### Data Structures

```
module GEOMETRY.

Point = Real * Real.
origin => Point.
distance p: Point, q: Point => Real.
---
origin = (0.0, 0.0).
all p: Point | distance p p = 0.0.
all p: Point, q: Point | distance p q = distance q p.
```

## Bounded Model Checking

The `--check` flag translates your specification into SMT-LIB2 and verifies it using an SMT solver (z3 by default). This performs three checks for each action:

1. **Contradiction detection** - Are the action's postconditions satisfiable? If not, no state transition can satisfy all constraints simultaneously.

2. **Invariant preservation** - Do invariants (propositions in non-action chapters) still hold after the action fires? Reports a counterexample when a violation is found.

3. **Precondition satisfiability** - Can the action's preconditions ever be met, given the invariants? Flags unreachable "dead" operations.

Checking is bounded: domain types are modeled with a finite number of elements (default 3, configurable with `--bound`). This means checks are sound within the bound but not complete for all possible domain sizes.

### Example: detecting an invariant violation

```
module BANKING.
context Accounts.

Account.
{Accounts} balance a: Account => Nat0.
---
all a: Account | balance a >= 0.

where

Accounts ~> Withdraw | a: Account, amount: Nat.
---
balance' a = balance a - amount.
all b: Account | b != a -> balance' b = balance b.
```

```bash
$ pant --check banking.pant
OK: Action 'Withdraw' postconditions are satisfiable
FAIL: Invariant 'all a: Account | balance a >= 0' may be violated by action 'Withdraw'
  Action:
    a = Account_0
    amount = 2
  Before:
    balance Account_0 = 1
  After:
    balance' Account_0 = -1
OK: Action 'Withdraw' preconditions are satisfiable
```

The counterexample shows that withdrawing 2 from a balance of 1 violates the non-negative invariant. The fix is to add a precondition: `balance a >= amount`.

### Contexts and frame conditions

When an action declares a context (`Accounts ~> Withdraw`), the checker automatically generates **frame conditions**: functions not in the context are asserted unchanged. This means invariants that only reference extracontextual functions are trivially preserved and skipped, reducing verification work.

## Editor Integration

Pantagruel can be used with any editor that supports LSP through [efm-langserver](https://github.com/mattn/efm-langserver).

### efm-langserver Configuration

Add to your efm-langserver config (typically `~/.config/efm-langserver/config.yaml`):

```yaml
version: 2
tools:
  pantagruel-lint: &pantagruel-lint
    lint-command: 'pant ${INPUT}'
    lint-stdin: false
    lint-formats:
      - '%f:%l:%c: error: %m'

languages:
  pantagruel:
    - <<: *pantagruel-lint
```

### Neovim (with nvim-lspconfig)

```lua
require('lspconfig').efm.setup({
  filetypes = { 'pantagruel' },
  init_options = { documentFormatting = true },
})

-- Associate .pant files with pantagruel filetype
vim.filetype.add({
  extension = { pant = 'pantagruel' }
})
```

### VS Code

Install the [efm-langserver extension](https://marketplace.visualstudio.com/items?itemName=ponsuke.vscode-efm-langserver) and configure similarly.

## License

BSD-3
