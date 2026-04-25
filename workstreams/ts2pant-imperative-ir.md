# Workstream: ts2pant Imperative IR (IRSC-Faithful Normalization Layer)

## Vision

Build a TypeScript-faithful imperative intermediate representation layer
(Layer 1) between the TS AST and ts2pant's existing functional IR (Layer 2,
today's `IRExpr`/`IRStmt`). Normalization passes against Layer 1 collapse
operationally-equivalent TS surface forms — increment spellings, conditional
families, iteration families — into a small canonical vocabulary. Lowering
passes run against ONE canonical input shape per construct, so adding a new
TS spelling never again requires extending a recognizer.

Reference: Vekris/Cosman/Jhala, *Refinement Types for TypeScript* (PLDI
2016, [arxiv:1604.02480](https://arxiv.org/pdf/1604.02480)). Their FRSC→IRSC
SSA transformation is the precedent. ts2pant cited it but stopped short of
building it. This workstream commits to it.

## Current State

After PR #128 lands (Stages 1–7 of the existing IR Migration Status table in
`tools/ts2pant/CLAUDE.md`), ts2pant has a Pant-shaped expression IR
(`IRExpr`: `Var`, `Lit`, `App`, `Cond`, `Let`, `Each`, `Comb`, `Forall`,
`Exists`, `IRWrap`) plus a thin statement layer (`IRStmt`: `Write`, `LetIf`,
`Seq`, `Assert`). TS-side recognizers in `tools/ts2pant/src/translate-body.ts`
and `tools/ts2pant/src/ir-build.ts` build IR directly from TS-AST. Pure-path
only.

The proximate motivator for this workstream is PR #131
(`recognizeMuSearch` accepting five surface spellings of the same `n ↦ n+1`
step). Five recognizer cases for one semantic operation is the structural
problem; this workstream retires it by moving normalization into a layer
where syntactic variation collapses *before* recognition runs.

This workstream **supersedes Stages 9–11** of the existing IR Migration
Status table (mutating-path SSA, frame conditions, mutating-path cutover).
**Stage 8** (pure-path cutover, deleting legacy code subsumed by IR) is
unchanged and lands as currently described.

## Key Challenges

- **Equivalences that aren't equivalences.** Several surface forms look the
  same but aren't: `==` vs `===` with non-Bool operands; `switch` with
  fall-through vs if-chain; `.forEach(cb)` vs `for-of` when `cb` captures
  `this` or returns from an enclosing scope; `&&`/`||` as expressions on
  non-Bool values. The normalizer must refuse to canonicalize when it
  can't prove equivalence. Conservative-refusal logic is itself nontrivial.
- **Conservative-refusal cost.** Policy 3(b) — reject the function with
  UNSUPPORTED at the normalizer when equivalence can't be proven. Real
  fixtures will reveal whether this pessimism is acceptable or whether
  some classes need pass-through-un-normalized as a fallback. Measured at
  M1; policy may be revisited per equivalence class.
- **Vocabulary lock-in at M1.** Layer 1 forms (`Foreach`, `Cond`, `Assign`,
  etc.) are decided up front. Forms can be *added* later (e.g., `IsNullish`
  primitive at M4) but not changed. Mid-stream re-litigation is the failure
  mode the workstream is meant to prevent.
- **Iteration shapes mutation.** `Foreach(binder, source, body)` with a
  *statement* body (admitting `Assign`) is the canonical iteration form.
  Shape A (uniform iterator write), Shape B (accumulator fold), and
  `.reduce` desugaring all require statement-body iteration. This forces
  the former Stages 9–11 mutating-path work to land *with* iteration
  normalization in M3, not as a separate milestone.
- **Hard rule per equivalence class.** During migration, an entire
  equivalence class moves to the new path in one milestone — no
  half-migrated classes co-existing with legacy recognizers. Faster to
  ship, slower per milestone, prevents the cross-talk hazard the IR was
  meant to retire.
- **Opaque AST constraint unchanged.** All normalization happens on Layer 1
  before any `OpaqueExpr` exists. No syntactic peeking on `OpaqueExpr` from
  any normalization or lowering pass.

## Milestones

### Milestone 1: imperative-ir-conditionals — ✅ landed

**Status**: landed across four stacked commits on
`zax--ts2pant-imperative-ir-workstream`:

- Patch 1 (`9e3b185`) — L1 vocabulary (`ir1.ts`) + lowering (`ir1-lower.ts`)
  + unit tests. ~880 lines of new code, no plumbing changes.
- Patch 2 (`7600ec3`) — L1 builder (`ir1-build.ts`) +
  `isStaticallyBoolTyped` (`purity.ts`) + plumbing into `translate-body.ts`
  gated on `TS2PANT_USE_L1=1`. New L1-plumbing integration tests.
  Validation: byte-identical output for all 431 existing tests under both
  flag states.
- Patch 3 (`9bbb25c`) — hard-rule cutover. Flag deleted, legacy
  `translateIfStatement` and inline ternary handler removed. Net −257
  lines. 469 tests pass under always-on L1.
- Patch 4 — docs (this commit).

**Pessimism rate**: 0 — no existing fixture or dogfood case rejects under
the conservative-refusal policy. The `&&`/`||` Bool-type predicate
accepts every site reached by current fixtures because non-Bool
short-circuit isn't an L1-conditional form (it falls through to the
legacy `translateOperator` path, which Patch 3 left intact for non-
conditional uses including `.reduce` callbacks). Real-fixture pessimism
will surface only as new TS code drops in; revisit policy if that happens.

**Definition of Done**:
- New module `tools/ts2pant/src/ir1.ts` (Layer 1 IR types) declares the full
  Layer 1 vocabulary. Statements: `Block`, `Let`, `Assign`, `Cond` (multi-arm
  with optional else), `Foreach`, `For`, `While`, `Return`, `Throw`,
  `ExprStmt`. Expressions: `Var`, `Lit`, `BinOp`, `UnOp`, `App`, `Member`,
  `Cond`. (Forms unused in M1 are *declared* but unimplemented; vocabulary
  is locked here.)
- New module `tools/ts2pant/src/ir1-build.ts` translates TS AST → Layer 1
  for the conditional surface forms only. Other forms still go through the
  existing TS-AST → Layer 2 path.
- Conditional surface-form normalization handles: if-chains (single-armed,
  if/else, if/else-if/else), ternary chains (right-associative flatten),
  `switch` without fall-through (every case ends in `break`/`return`/
  `throw`; `default` last; non-fall-through cases collapse to one
  `Cond` arm), `&&` and `||` as expressions when both operands are
  statically Bool-typed. All collapse to one canonical `Cond` form.
- Strict reject (3(b)): `switch` with any fall-through case, `default` not
  last when case-set non-exhaustive, `&&`/`||` with non-Bool operands. Each
  rejection produces an UNSUPPORTED with a specific reason.
- New module `tools/ts2pant/src/ir1-lower.ts` lowers Layer 1 `Cond`
  (statement and expression) to Layer 2 `Cond`. Other Layer 1 forms not
  yet exercised.
- `tools/ts2pant/CLAUDE.md` IR Migration Status table updated: Stages 9–11
  marked superseded, pointer to this workstream. New "Imperative IR" section
  documents the Layer 1 vocabulary and the conditional canonical form.
- Hard rule honored: every conditional-shaped TS construct now goes through
  Layer 1. Legacy conditional handlers in `translate-body.ts` deleted.
- All existing `pant --check`-clean fixtures still pass; new fixtures cover
  the conditional surface forms and the rejection cases.

**Why this is a safe pause point**: Conditionals are a self-contained
equivalence class. Every other recognizer remains on the TS-AST → Layer 2
path. The Layer 1 vocabulary is declared but only one normalization pass
exercises it. Codebase translates the same set of TS programs as before
M1, plus newly-translatable ones (conditional families that weren't
previously recognized uniformly).

**Unlocks**: M2 (assign + μ-search) builds on the Layer 1 vocabulary that
M1 declares. The architecture is proven on a richer equivalence class than
PR #131's increment-only motivator, so M2 is incremental rather than
exploratory.

**Open Questions**:
- Switch exhaustiveness: M1 uses syntactic check (default-last). Type-based
  exhaustiveness (literal-union switch with no default) is deferred — leave
  as UNSUPPORTED with a reason.
- Real-fixture pessimism rate from conservative refusal: measured during M1.
  If a high fraction of `&&`/`||` uses involve non-Bool operands, M4 may
  need to land before M3.

---

### Milestone 2: imperative-ir-assign-mu-search

**Definition of Done**:
- `ir1-build.ts` extends to translate increment surface forms: `i++`, `++i`,
  `i--`, `--i`, `i += k`, `-=`, `*=`, `/=`, `%=`, `i = i ⊕ k` (commutative
  ops only), `i = k ⊕ i` (same). All collapse to canonical
  `Assign(target, value)` with the value reconstructed as the appropriate
  `BinOp`.
- `recognizeMuSearch` rewritten to match against Layer 1: pattern
  `Block([Let(i, init), While(p, Assign(i, BinOp(Add, Var(i), Lit(1))))])`.
  One pattern, no `isPlusOneStep` helper. PR #131's recognizer extension
  is structurally subsumed.
- All five `+1` step spellings produce byte-identical Pant output (snapshot
  equality with M1's pre-rewrite output).
- `ir1-lower.ts` extends to handle Layer 1 `Assign` in pure-path contexts
  (read-modify-write of a const-bound accumulator, μ-search counter). Full
  mutation semantics deferred to M3.
- Legacy increment recognizer code deleted; hard rule honored for the
  increment equivalence class.
- New fixtures cover non-increment uses of `Assign` reachable in pure-path
  (e.g., `let acc = 0; for (...) acc += ...; return acc` — but only the
  μ-search-shaped cases land here; the iteration body remains on the
  legacy path until M3).

**Why this is a safe pause point**: Assign and μ-search form a coherent
slice. The Layer 1 vocabulary is now exercised by two normalization classes,
proving the architecture composes. Iteration with `Assign` in body still
goes through legacy recognizers (Shape A/B are unchanged).

**Unlocks**: M3 — iteration normalization can now use `Assign` in body
position without inventing a new form. Mutation work (former Stages 9–11)
re-forms on top of `Assign`.

**Open Questions**:
- `i = f(i)` (arbitrary self-update, e.g. iterate-to-fixpoint): build as
  Layer 1 `Assign(i, App(f, [Var(i)]))` and let the lowering pass decide
  rejection. Recognizer for iterate-to-fixpoint is its own future work,
  out of scope.
- Decrement μ-search (`i--` with `while (P(i))`): canonical form admits
  it, but the SMT lowering for `min over each j: Int, j <= INIT, ~P(j) | j`
  needs verification on a fixture before declaring supported. M2 lands the
  syntax; the encoding is M2 if straightforward, deferred otherwise.

---

### Milestone 3: imperative-ir-iteration-mutation

**Definition of Done**:
- `ir1-build.ts` extends to translate iteration surface forms:
  `for (const x of arr) {body}`, `arr.forEach(x => {body})`,
  `for (let i = 0; i < arr.length; i++) {body using arr[i]}` (when `arr[i]`
  is read-only), `arr.reduce((a, x) => f(a, x), init)`. All collapse to
  canonical `Foreach(binder, source, body)` with statement-body. `.reduce`
  desugars into `Block([Let(acc, init), Foreach(x, arr,
  Assign(acc, App(f, [Var(acc), Var(x)]))), Return(Var(acc))])`.
- Strict reject (3(b)): `forEach` callback that captures `this` or returns
  early through enclosing scope; for-loop with non-`.length` bound or
  non-`++` step that can't normalize to Foreach (falls through to `For` or
  rejection).
- New `ir1-lower.ts` pass classifies `Foreach` bodies: pattern
  `Foreach(x, src, Assign(Member(x, p), e))` → Shape A
  (`all x in src | p' x = e`); pattern
  `Foreach(x, src, Assign(Member(a, p), BinOp(op, Member(a, p), f(x))))` → Shape B
  (`p' a = p a op (combOp over each x in src | f x)`); pattern
  `Let(acc, init); Foreach(x, src, Assign(acc, body))` followed by
  `Return(acc)` → fold (`init op (combOp over each x in src | step x)`).
  Frame-condition synthesis runs on the Layer 1 → Layer 2 lowering output.
- Layer 2 `Write` and `LetIf` (the mutating-path forms PR #128 introduced)
  are now driven by the Layer 1 `Assign` lowering, not by direct TS-AST →
  Layer 2 translation. The "write-key" SSA discipline lands here as a
  Layer 1 → Layer 2 pass, not as a Layer 2 internal form.
- Legacy iteration recognizers (`extractStructuredIteration`, the Shape A/B
  branches in `translate-body.ts`, the chain-fusion handling for `.reduce`)
  deleted. Hard rule honored.
- Mutating-path cutover: every TS construct that produces a primed equation
  now flows through Layer 1. The former Stages 9–11 are complete.

**Why this is a safe pause point**: Iteration and mutation are a coherent
unit (one shapes the other). Both pure-path and mutating-path now flow
through Layer 1 for the three normalization classes (conditionals,
assign/μ-search, iteration). Property access, equality, and nullish-check
normalization remain on the legacy expression path — but those are
expression-level orthogonal classes; the architectural backbone is
complete.

**Unlocks**: M4 (equality/nullish) and M5 (property access) become
optional, low-risk follow-ups. M6 (cleanup) deletes whatever legacy code
remains in `translate-body.ts`.

**Open Questions**:
- Index-for over `.length` with mutating writes through `arr[i]`: today's
  legacy code rejects this. Layer 1 normalization could handle it via
  `Foreach(x, arr, Assign(Member(x, ...), ...))` if `arr[i] = …` is read
  back during the same iteration. Risk of subtle aliasing semantics; deferred
  unless a fixture demands it.
- Frame-condition emission ordering: the Layer 1 → Layer 2 lowering pass
  must produce frames in a stable order (deterministic across runs). Today's
  `state.modifiedProps` is a Set with insertion-order iteration; the pass
  needs the same guarantee.

---

### Milestone 4 (deferred): equality-nullish-normalization

**Definition of Done**:
- New Layer 1 primitive `IsNullish(expr)`. Surface forms `x == null`,
  `x === null || x === undefined`, `x === undefined`, `typeof x === 'undefined'`
  all collapse to `IsNullish(Var(x))`.
- `BinOp(===, …)` is the canonical equality form; `BinOp(==, …)` rejected
  unless we can prove operand types Bool/numeric/string-literal-equivalent.
- Existing `??` and `?.` lowerings (currently on Layer 2 since Stages 2–3)
  are revisited if their interaction with `IsNullish` simplifies — but no
  redesign required; the goal is M4 absorbs the equality/nullish equivalence
  class without disturbing existing forms.
- Hard rule honored for the equality/nullish class.

**Why this is a safe pause point**: Equality and nullish are
expression-only; no statement-level forms touched. All other classes
already on Layer 1 from M1–M3.

**Unlocks**: M6 cleanup can delete more legacy expression-handling code.

**Open Questions**:
- Whether to land at all. Decided post-M3 based on real-fixture pressure
  from M1's conservative-refusal measurements. May absorb into M3 if
  pressure is high.

---

### Milestone 5 (deferred): property-access-normalization

**Definition of Done**:
- Layer 1 `Member(receiver, name)` is canonical for both `obj.name` and
  `obj["name-literal"]`. Computed access `obj[expr]` stays as
  `App(index, [obj, expr])` and is rejected unless type system resolves
  to a known field set.
- Hard rule honored.

**Why this is a safe pause point**: Property access is a small,
self-contained equivalence class.

**Unlocks**: M6 cleanup of the last legacy expression handling.

**Open Questions**:
- Likely absorbed into whichever earlier milestone first needs uniform
  property-access shape (probably M3, when Foreach bodies do
  `Assign(Member(x, p), …)` and need a single Member form). Standalone
  milestone only if not absorbed.

---

### Milestone 6: legacy-recognizer-cleanup

**Definition of Done**:
- `tools/ts2pant/src/translate-body.ts` slimmed to the Layer 1 → Layer 2
  lowering plumbing only. All TS-AST → Layer 2 direct paths deleted.
- `IRWrap` form deleted from `IRExpr` (the migration escape hatch is no
  longer reachable).
- IR Migration Status table in `tools/ts2pant/CLAUDE.md` removed (or
  archived); replaced by a "Layer 1 → Layer 2 → Pant" architecture
  description anchored on this workstream.
- Single-pipeline default: `--use-ir` flag (env `TS2PANT_USE_IR=1`) deleted
  or made always-on.
- All snapshots stable; no behavioral change relative to M3 + (M4|M5 if
  landed).

**Why this is a safe pause point**: End state of the workstream. The
codebase has one translation pipeline, one IR vocabulary, one normalization
discipline. The recognizer-extension treadmill is retired.

**Unlocks**: Future TS surface support is a normalization-pass extension
(small, localized) instead of a recognizer addition.

## Dependency Graph

```text
1 (imperative-ir-conditionals)        → []
2 (imperative-ir-assign-mu-search)    → [1]
3 (imperative-ir-iteration-mutation)  → [2]
4 (equality-nullish-normalization)    → [3]   (deferred; may absorb into 3)
5 (property-access-normalization)     → [3]   (deferred; may absorb into 3)
6 (legacy-recognizer-cleanup)         → [3, 4, 5]
```

PR #128 must merge before M1 begins. M1–M3 are strictly sequential because
each adds a class to the same Layer 1 vocabulary and the hard-rule
constraint forbids partial migration of a class. M4 and M5 can run in
parallel if both are needed.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Conservative-refusal pessimism rate | Measure during M1 on real fixtures. If high, M4 may need to land before M3 or policy may relax to pass-through-un-normalized for specific forms. | M1 |
| `Reduce` as own L1 form vs desugared | Decided: desugared into `Let + Foreach + Assign`. IRSC-faithful. May revisit if downstream pattern recognition proves brittle. | Decided in design (this doc) |
| M4 standalone vs absorbed into M3 | Depends on M1 pessimism measurement and how much equality/nullish lives inside iteration bodies. | Post-M2 |
| M5 standalone vs absorbed | Property access likely absorbed into M3 (Foreach bodies need `Member`). | Post-M2 |
| Decrement μ-search (`i--`) SMT encoding | Layer 1 admits it; SMT lowering needs verification on a fixture. | M2 |
| Index-for with mutating `arr[i] = …` writes | Aliasing semantics non-trivial. Deferred unless a fixture demands it. | Post-M3 |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Layer 1 vocabulary locked at M1 | Mid-stream re-litigation of canonical forms is the failure mode the workstream is meant to prevent. Forms can be *added* later (e.g., `IsNullish` at M4) but not changed. |
| Conservative refusal = reject the function (3(b)) | Cleaner architecturally; matches the "find equivalences that are actually equivalences" discipline. Pessimism measured per-class; policy revisitable per-class if real fixtures hurt. |
| Hard rule per equivalence class (4) | Faster per milestone, prevents the cross-talk hazard the IR was meant to retire. No half-migrated classes coexisting with legacy recognizers. |
| Mutation lands with iteration in M3 (5(b)) | `Foreach` body must be a *statement* (admit `Assign`) to support Shape A, Shape B, and `.reduce` desugaring uniformly. Iteration normalization without `Assign` would force splitting the form. The former Stages 9–11 re-form inside M3. |
| First slice = conditionals, not increment-only (1(b)) | Richer demonstration of the architecture earning its keep. Touches more recognizer surface area than μ-search alone. M1 conditionals + M2 assign + M3 iteration is a steeper but more honest validation curve than three small increments. |
| No `If(cond, then, else)` separate from `Cond` | Single-armed if = `Cond([(g, body)], None)`. Multi-armed normalizes to the same form. One downstream pattern. |
| No `Inc(target)` separate from `Assign` | `i++` builds as `Assign(i, BinOp(Add, Var(i), Lit(1)))`. The "+1 step" is a single arithmetic predicate at μ-search lowering, not a separate form. |
| `&&`/`\|\|` normalized only when Bool-typed | Truthy/falsy semantics on non-Bool values diverge from `Cond`'s Boolean guards. Conservative refusal on non-Bool. |
| `switch` fall-through rejected at L1 build | Each case must end in `break`/`return`/`throw`. Fall-through is a different control structure with no clean canonical form. |
| `Reduce` desugared into `Let + Foreach + Assign` | IRSC-faithful (every iteration is `Foreach`; fold is a downstream pattern). Avoids a second iteration form. |
| Workstream supersedes Stages 9–11 of `tools/ts2pant/CLAUDE.md` IR Migration Status; Stage 8 unchanged | Stage 8 (pure-path cutover, deleting legacy code subsumed by IR) depends only on PR #128 landing, not on this architecture. Stages 9–11 (mutating-path SSA, frame conditions, mutating-path cutover) re-form on the Layer 1 substrate inside M3. |
