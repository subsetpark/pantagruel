# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Setup

Requires opam (OCaml package manager) and `just` (cross-language task runner: `brew install just` on macOS, `cargo install just` otherwise). All other OCaml tooling is installed via opam.

**`just` is the workspace-level entry point.** Run `just --list` from the repo root to see every task. The recipes encode cross-language dependency ordering — `just ts2pant-test-integration` builds the pant binary first, then runs the integration tests, exactly as CI does. Never run `dune build` from inside test code; that pattern caused a 15-hour pre-commit hang because Node's per-file test isolation made every worker race for `_build/.lock`. See `tools/ts2pant/scripts/prebuild-pant.mjs` and `tools/ts2pant/CLAUDE.md` § "Test layout".

### Ensure an opam switch is active

Every opam command requires an active switch. Switches are heavyweight (~2GB each) — this repo uses a single **local switch** at the repo root. Worktrees share this switch via `opam env --switch=<repo-root> --set-switch`.

```bash
# Check if a switch is active
opam switch show

# If "No switch is currently set" and the local switch exists:
eval $(opam env --switch=/path/to/pantagruel --set-switch)

# If no local switch exists (fresh clone), create one from the repo root:
opam init -y              # only if opam has never been initialized
opam switch create . 5.4.1
eval $(opam env)
```

All subsequent `opam install` and `dune` commands require the switch environment. If commands fail with opam/ocamlfind errors, run `eval $(opam env)` first.

### Core only (OCaml CLI tool)

```bash
opam install ./pantagruel.opam --deps-only --with-test
dune build
dune test
```

### Core + WASM (needed for ts2pant)

The WASM build compiles the Pantagruel parser to WebAssembly via `wasm_of_ocaml`. ts2pant embeds this binary for capture-avoiding substitution and AST construction.

```bash
# 1. Install core OCaml dependencies
opam install . --deps-only --with-test

# 2. Install WASM toolchain (js_of_ocaml + wasm_of_ocaml)
#    These are listed in pantagruel-wasm.opam but must be installed explicitly
#    because `opam install . --deps-only` does not always pull build-only deps
#    for secondary packages.
opam install js_of_ocaml js_of_ocaml-ppx wasm_of_ocaml-compiler

# 3. Install binaryen (WASM optimizer, required by wasm_of_ocaml at link time)
npm install -g binaryen

# 4. Build the core library (WASM build depends on pantagruel_parser)
dune build

# 5. Build the WASM binary (gated by BUILD_WASM env var)
BUILD_WASM=true dune build wasm/
#    Produces: _build/default/wasm/pant_wasm.bc.wasm.js
#              _build/default/wasm/pant_wasm.bc.wasm.assets/

# 6. Build ts2pant (copies + patches WASM into src/wasm/, then runs tsc)
cd tools/ts2pant
npm install
npm run build        # runs build:wasm then tsc
npm test             # verify everything works
```

The `wasm/dune` file uses `(enabled_if (= %{env:BUILD_WASM=false} "true"))`, so `dune build` without `BUILD_WASM=true` skips the WASM target entirely. This lets `dune build` succeed without WASM dependencies installed.

## Build Commands

Prefer `just <recipe>` from the repo root for everything that crosses language boundaries (anything involving ts2pant). Direct `dune` commands below still work for OCaml-only flows.

```bash
# Workspace-level (preferred when touching ts2pant or wasm)
just                   # list every recipe
just build             # OCaml core
just build-pant        # just the pant binary
just test              # all tests, OCaml + ts2pant (unit + integration)
just ts2pant-precommit # mirror what the pre-commit hook runs

# OCaml-only direct commands
dune build

# Run all tests
dune test

# Run a specific test executable
dune exec test/test_lexer.exe
dune exec test/test_parser.exe
dune exec test/test_check.exe

# Run the CLI tool
dune exec pant -- <file.pant>
dune exec pant -- --json <file.pant>    # Output JSON with resolved types
dune exec pant -- --ast <file.pant>     # Print AST (OCaml format)
dune exec pant -- --normalize "Borrow" <file.pant>  # Top-down normal form from root term

# SMT verification (bounded model checking)
dune exec pant -- --check <file.pant>           # Run all checks with z3
dune exec pant -- --check --bound 5 <file.pant> # Set domain element bound (default: 3)
dune exec pant -- --check --solver cvc5 <file.pant>  # Use alternative solver

# Build WASM target (see Setup above for dependency installation)
BUILD_WASM=true dune build wasm/
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
   - Stores declaration guards in `Env.rule_guards` for SMT guard injection
   - Detects duplicate declarations and recursive aliases

4. **Type Checking** (`lib/check.ml`): Pass 2 - Expression validation
   - Infers types for all expressions
   - Validates propositions are boolean
   - Handles action contexts (primed expressions like `f'`)

### Key Type Representations

- `Ast.type_expr`: Syntactic types from parser (TName, TList, TProduct, TSum)
- `Types.ty`: Internal type representation with numeric hierarchy (Nat < Nat0 < Int < Real)
- `Env.t`: Type environment mapping names to entries (domains, aliases, rules, variables). Also stores `rule_guards` — declaration guards keyed by rule name (formal params + guard expressions) for SMT guard injection.

### Module System (`lib/module.ml`)

- Scans directories for `.pant` files and builds a registry
- Handles imports with cycle detection
- Merges environments from imported modules

### Language Features

- Domains: `User.` declares a domain type
- Type aliases: `Point = Nat * Nat.`
- Rules: `owner d: Document => User.` (with return type via `=>`)
- Actions: `~> Check out @ u: User.` (state transitions via `~>`, free-form label, `@` before params)
- Actions enable primed expressions (`owner' d`) for state transitions
- Action labels are not in the term namespace — they're purely human-readable annotations
- Quantifiers: `all u: User | ...` and `some x: T | ...` (body must be Bool)
- Comprehensions: `each u: User | f u` produces `[U]`; with a combiner prefix (`+ over each u: User | f u`) produces a scalar aggregate (e.g., `Nat0`)
- Tuple projection: `point.1`, `point.2`
- List cardinality: `#users`
- Membership: `x in Domain`
- Closures: `ancestor b: Block => [Block] = closure parent.` derives transitive closure of an endorelation
- Contexts: `context Accounts.` at module level, `{Accounts} balance ...` for footprint, `Accounts ~> Withdraw @ ...` for actions
- Conditionals: `cond guard1 => val1, guard2 => val2, true => default` (multi-armed, arms must be Bool, consequences must have same type, checked for exhaustiveness during `--check`)
- Declaration guards: `score u: User, active u => Nat.` — guards are stored in the environment and automatically injected as antecedents in SMT queries when the guarded function is applied

## Packages

The project produces two opam packages, both declared in `dune-project`:

- **pantagruel**: Core CLI tool and libraries. Dependencies: sedlex, menhir, ppx_deriving, yojson, sexplib0, parsexp, alcotest (test).
- **pantagruel-wasm**: WebAssembly build of the parser. Dependencies: pantagruel, js_of_ocaml, js_of_ocaml-ppx, js_of_ocaml-compiler (build), wasm_of_ocaml-compiler (build).

The WASM executable in `wasm/dune` is gated by `(enabled_if (= %{env:BUILD_WASM=false} "true"))` so that `dune build` succeeds even without the WASM dependencies installed.
