# Guard Narrowing Survey (Workstream M2)

**Workstream:** ts2pant Flow-Sensitive Guard & Effect Analysis — Milestone 2
(`guard-narrowing-survey`). **Date:** 2026-05-27. **Method:** read-only probe over
ts2pant `src/` (653 functions) + `tests/fixtures` after M1 (`guard-purity-user-calls`)
merged. No translator behavior changed.

## Purpose

M1 gave ts2pant a single checker-aware purity oracle and admitted calls to pure
user functions in condition/predicate positions. M3 (`guard-flow-narrowing`) is the
greenfield layer that propagates a test's truth into its branch — and it is the
cross-workstream dependency for the discriminated-union workstream's M3. This
survey measures (1) M1's actual dogfood impact, (2) which narrowing patterns the
corpus actually uses, to scope and design M3.

## (1) M1 dogfood impact — smaller than the raw count suggested, and that's correct

The body-lowering diagnostic counted ~58 functions blocked on "early-return
predicate has side effects" / "impure if-condition in mutating body". After M1 the
counts are 52 + 10. The headline is **not** the absolute delta (the src corpus also
grew ~23 functions, and a measurement artifact — ~280 functions throw "Maximum call
stack size exceeded" during whole-file re-translation — adds noise). The headline is
**what remains and why**:

> The residual predicate-rejections are dominated by predicates that call the
> **TypeScript compiler API** (`checker.getX(...)`, `ts.isX(...)`), which lives in
> `node_modules` and the M1 classifier **correctly bails on** (→ effectful).
> Examples: `isStringLikeType`, `getBuiltinNamespace`, `isNamedCallableDeclaration`,
> `declarationName`.

So M1 delivered exactly its addressable set — predicates over **local pure helpers**
(demonstrated by the `expressions-pure-call-predicate` fixtures it cleared) — and the
remainder are checker-API calls that are inherently unmodellable and *should* stay
rejected. **Implication for the workstream:** ts2pant's own source is a weaker
dogfood target for the purity lever than the raw ~66 figure implied, because it is
TS-compiler-API-heavy. This does not diminish M1 (the lever is real for user code);
it recalibrates expectations for self-translation specifically.

## (2) Narrowing-pattern inventory (src + fixtures)

| Pattern | Count | Notes |
|---|---|---|
| **Discriminant `switch`** (`switch (x.kind)`) | **58** | `switch prop.kind` / `decl.kind` / `expr.kind` / `e.kind` — ts2pant's own IR walkers over `IR1Expr`/`IRExpr` discriminated unions. **This is the DU-workstream M3 dependency, and it is heavily used.** |
| **`=== literal` on a property** | 169 | Mix of true discriminant narrowing (`.kind === "..."`) and ordinary value compares (`.length === 0`, `.status === 2`). The `.kind === lit` subset is DU-relevant. |
| **Nullish** (`x !== null`, `=== undefined`) | 165 | Guards a subsequent non-null access. Large, self-contained lever. |
| **Boolean / call guards** (`if (f(x))`) | 368 | Includes user predicates **and** TS type-guards (`ts.isX(n)`). |
| **Type predicates** (`x is T`) | (under-measured) | The probe's per-file checker could not resolve signatures, so type-predicate calls fell into the boolean-guard bucket. Qualitatively `ts.isX(...)` type guards are *pervasive* in ts2pant's own code — the primary way it discriminates AST node kinds. |

### Reading the inventory

- **Discriminant narrowing is the clear M3 priority.** It is both the DU M3
  hard-dependency *and* the densest real pattern (58 switches + the `.kind === lit`
  ifs). For the DU workstream it narrows *user* discriminated unions; for
  self-translation it narrows ts2pant's own IR unions.
- **Nullish narrowing (165)** is the next lever — guarding non-null field/access
  inside `if (x != null)`.
- **Type-predicate narrowing (`ts.isX`)** is pervasive but refines *TypeScript
  compiler types* (complex `ts.Node` unions), so its self-translation payoff is
  entangled with how ts2pant models those types — higher effort, lower near-term
  certainty than discriminant/nullish.

## M3 design proposal

A **function-scoped assumption environment**: when lowering enters a branch whose
test is a recognized narrowing predicate, push the predicate's fact into an
environment that lowering consults to discharge guards; pop on exit. Facts:

- `discriminant`: `<expr> = <literal>` (from `x.kind === "c"` / matched `switch` arm)
  → discharges a discriminant-guarded variant-field rule. **The DU workstream's
  `du-discriminant-narrowing` consumes exactly this fact.**
- `non-null`: `<expr>` present (from `x != null` / `x !== undefined`) → discharges a
  nullability obligation.
- `predicate`: `<predicate-application>` true (from a boolean/type-guard test) → the
  base case.

Intra-function only (workstream decision); no field-name special-casing — the
discriminant fact keys on the structural `<property-access> === <literal>` shape, not
on `.kind`. Facts render as z3 path-condition assumptions (occurrence typing modulo
theories).

## Verdict & recommended M3 scope

Proceed to M3. Applying the workstream's gate (a pattern is in M3 iff it spans
multiple corpus functions **or** a downstream workstream hard-depends on it):

- **Discriminant narrowing** — IN (both criteria).
- **Nullish narrowing** — IN (165 sites).
- **Type-predicate narrowing** — IN by count, but flagged higher-effort (refines
  TS-compiler types); candidate to sequence last or defer.

**Recommended split (workstream structural change — for the operator to weigh):**
split M3 into **M3a `du-discriminant-narrowing-layer`** (discriminant + the boolean
base case — delivers the DU dependency first, smallest surface) and **M3b
`nullish-and-predicate-narrowing`** (nullish, then type-predicate). This unblocks the
DU workstream as early as possible rather than gating it behind the larger
nullish/predicate work.

## Caveats

- The ~280 "Maximum call stack size exceeded" throws during whole-file re-translation
  are a probe/translator-robustness artifact on large or mutually-recursive functions;
  they are not narrowing data and warrant a separate robustness look.
- The type-predicate vs boolean-guard split is unreliable (per-file checker could not
  resolve call signatures); treat the 368 boolean-guard bucket as containing a large,
  unquantified share of `ts.isX` type guards.
