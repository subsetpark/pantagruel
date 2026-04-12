# ts2pant Development Guide

## Architecture

ts2pant translates TypeScript function bodies into Pantagruel propositions. The main
translation pipeline lives in `src/translate-body.ts`.

Two translation modes:
- **Pure functions**: `f(x) { ... return expr }` -> `f x = <expr>.`
- **Mutating functions**: `f(obj) { obj.prop = val }` -> `prop' obj = <val>.` + frame conditions

## Key Patterns for Body Translation

### Let-Elimination (Const Binding Inlining)

Translating `const a = e1; const b = e2; return e3` into a single expression is
**let-elimination** — the inverse of A-normal form conversion. This is a well-studied
program transformation. Our implementation follows the standard approach:

1. **Separate TDZ validation from substitution.** Forward-reference detection runs on
   the TS AST *before* any translation to opaque expressions. This is a well-formedness
   check on the input program, not part of the substitution algorithm.

2. **Use a monotonic unique supply for hygienic names.** Local const bindings are mapped
   to internal names (`$0`, `$1`, ...) via a `UniqueSupply` closure. These names cannot
   collide with property-accessor heads in the target language (e.g., `balance` in
   `app(var("balance"), [obj])`). Never derive counter values from array lengths or
   mutable state — use the supply's `next()` method.

3. **Apply substitutions via right-fold (inside-out).** For bindings `[a=e1, b=e2, c=e3]`
   and body `E`, substitute in reverse order: first `$c := e3`, then `$b := e2`, then
   `$a := e1`. Each substitution automatically flows through prior replacements. This
   eliminates the need to "apply all prior substitutions to each initializer."

4. **One shared function for both paths.** `inlineConstBindings()` is used by both
   `translatePureBody` and `collectAssignments`. Do not duplicate substitution logic.

### References

- [Secrets of the GHC Inliner](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf) — Peyton Jones & Marlow, JFP 2002. Definitive treatment of let-inlining.
- [Locally Nameless Representation](https://boarders.github.io/posts/locally-nameless/) — alternative approach using de Bruijn indices for bound variables.
- [A-Normal Form](https://en.wikipedia.org/wiki/A-normal_form) — the representation our TS input is approximately in.

### Opaque AST Constraint

Pantagruel expressions are opaque wasm handles (`OpaqueExpr`). We cannot inspect or
traverse them from TypeScript. All substitution must go through `ast.substituteBinder()`,
which is a proper capture-avoiding substitution implemented in OCaml. This is why we
use hygienic `$N` names (Barendregt convention) rather than locally nameless / de Bruijn.

## Common Pitfalls (from PR #84 post-mortem)

These bugs are **structurally prevented** by the patterns above. If you find yourself
working around any of these, you are likely not using the shared abstractions:

| Bug | Root Cause | Prevention |
|-----|-----------|------------|
| freshBinder picks a name used by a const | Const names in same namespace as binder names | Hygienic `$N` names never collide with letter-based binders |
| Forward const reference silently inlined | TDZ check interleaved with substitution | Phase 1: validate on TS AST before any translation |
| `balance` const collides with `.balance` accessor | String-name substitution hits property accessor heads | `$N` names cannot appear as property names |
| Property names flagged as variable references | `expressionReferencesNames` traversed all identifiers | Function skips syntactic name positions (PropertyAccess.name) |
| Counter reuse in nested scopes | Counter derived from array.length after splice | UniqueSupply closure is monotonic by construction |

## Testing

```bash
cd tools/ts2pant && npx vitest run          # all tests
cd tools/ts2pant && npx vitest run -t "const" # const-related tests
```

Fixture files for const bindings:
- `tests/fixtures/constructs/expressions-const-bindings.ts` (pure path)
- `tests/fixtures/constructs/functions-mutating-const.ts` (mutating path)
