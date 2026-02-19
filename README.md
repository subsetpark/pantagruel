# Pantagruel

A specification language checker for formal system descriptions.

Pantagruel lets you write precise specifications of systems using domains, procedures, and logical propositions. The checker validates that your specifications are well-typed and internally consistent.

## Installation

Requires OCaml 4.14+ and opam.

```bash
# Install dependencies
opam install menhir sedlex ppx_deriving alcotest

# Build
dune build

# Run tests
dune test

# Install globally (optional)
dune install
```

## Usage

```bash
# Check a specification file
pantagruel myspec.pant

# Read from stdin
echo 'module TEST. Foo. ---' | pantagruel

# Output JSON (includes resolved types and full AST)
pantagruel --json myspec.pant

# Print the AST (OCaml format, for debugging)
pantagruel --ast myspec.pant

# Specify module search path for imports
pantagruel --module-path ./specs myspec.pant
```

## Samples

The `samples/` directory contains reference specifications demonstrating all language features:

- `01-basics.pant` - Fundamental syntax: domains, procedures, quantifiers
- `02-library.pant` - Library system with actions and state transitions
- `03-types.pant` - All type features: products, sums, lists, aliases
- `04-operators.pant` - All operators: logical, comparison, arithmetic
- `05-state.pant` - State transitions with primed expressions and frame conditions
- `06-advanced.pant` - Function overrides and qualified names
- `07-pantagruel.pant` - Self-specification of the Pantagruel language
- `08-contexts.pant` - Context declarations and write-permission boundaries

## Language Overview

A Pantagruel document consists of a module declaration, optional imports, and one or more chapters. Each chapter has a **head** (declarations) and a **body** (propositions), separated by `---`.

### Basic Structure

```
module LIBRARY.

import USERS.

> Domains
Book.
Loan.

> Procedures
available b: Book => Bool.
~> borrow u: User, b: Book.
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

**Procedures** define operations with typed parameters:
```
// With return type
owner d: Document => User.
distance p: Point, q: Point => Real.

// Nullary (no parameters)
nobody => User.
```

**Actions** define state transitions (no return type):
```
// Action (with ~>) - for state transitions
~> check-out u: User, d: Document.
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

Actions model state transitions. Within a chapter that declares an action, you can use **primed expressions** to refer to the post-state:

```
User.
Document.
owner d: Document => User.
~> check-out u: User, d: Document.
---
// owner' refers to owner after check-out
owner' d = u.
```

### Contexts

Contexts declare write-permission boundaries for actions. Context names are declared at the module level, and procedures declare which contexts they belong to with a `{Ctx}` prefix. Actions declare which context they operate within using `Ctx ~>`.

```
module BANKING.
context Accounts.

Account.
{Accounts} balance a: Account => Nat.

Accounts ~> withdraw a: Account, amount: Nat.
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
~> deposit a: Account, amount: Nat.
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
