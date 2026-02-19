# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the project
dune build

# Run all tests
dune test

# Run a specific test executable
dune exec test/test_lexer.exe
dune exec test/test_parser.exe
dune exec test/test_check.exe

# Run the CLI tool
dune exec pantagruel -- <file.pant>
dune exec pantagruel -- --json <file.pant>  # Output JSON with resolved types
dune exec pantagruel -- --ast <file.pant>   # Print AST (OCaml format)
```

## Architecture

Pantagruel is a specification language checker written in OCaml. It processes `.pant` specification files through a multi-pass pipeline.

### Processing Pipeline

1. **Lexing** (`lib/lexer.ml`): Sedlex-based lexer
   - Tracks beginning-of-line state for doc comment handling (`>` at line start)
   - Produces `PROJ` tokens for `.N` (tuple projection) to avoid grammar conflicts with DOT

2. **Parsing** (`lib/parser.mly`): Menhir LR parser
   - Document structure: `module NAME. imports chapters`
   - Chapters: head (declarations) separated by `---` from body (propositions)
   - Qualified names use `::` syntax (e.g., `Module::name`) to avoid DOT conflicts

3. **Collection** (`lib/collect.ml`): Pass 1 - Declaration gathering
   - Collects domain declarations, type aliases, and rule signatures
   - Resolves type expressions to internal types
   - Detects duplicate declarations and recursive aliases

4. **Type Checking** (`lib/check.ml`): Pass 2 - Expression validation
   - Infers types for all expressions
   - Validates propositions are boolean
   - Handles action contexts (primed expressions like `f'`)

### Key Type Representations

- `Ast.type_expr`: Syntactic types from parser (TName, TList, TProduct, TSum)
- `Types.ty`: Internal type representation with numeric hierarchy (Nat < Nat0 < Int < Real)
- `Env.t`: Type environment mapping names to entries (domains, aliases, rules, variables)

### Module System (`lib/module.ml`)

- Scans directories for `.pant` files and builds a registry
- Handles imports with cycle detection
- Merges environments from imported modules

### Language Features

- Domains: `User.` declares a domain type
- Type aliases: `Point = Nat * Nat.`
- Rules: `owner d: Document => User.` (with return type via `=>`)
- Actions: `~> Check out | u: User.` (state transitions via `~>`, free-form label, `|` before params)
- Actions enable primed expressions (`owner' d`) for state transitions
- Action labels are not in the term namespace — they're purely human-readable annotations
- Quantifiers: `all u: User | ...` and `some x: T | ...`
- Tuple projection: `point.1`, `point.2`
- List cardinality: `#users`
- Membership: `x in Domain`
- Contexts: `context Accounts.` at module level, `{Accounts} balance ...` for footprint, `Accounts ~> Withdraw | ...` for actions

## Dependencies

- **sedlex**: Unicode lexer generator
- **menhir**: LR parser generator
- **ppx_deriving**: Derives `show` and `eq` for types
- **alcotest**: Test framework
