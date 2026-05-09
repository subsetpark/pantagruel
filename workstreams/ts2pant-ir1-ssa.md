# Workstream: ts2pant IR1 SSA

## Vision

Replace IR1's statement-position symbolic-state lowering with a comprehensive
SSA form directly in IR1 for every TypeScript construct ts2pant currently
supports. The end state is a cleaner architecture where mutation semantics,
branch joins, read-after-write behavior, frame conditions, Map/Set overrides,
and loop summaries are explicit IR concepts rather than behavior hidden inside
`SymbolicState` folds. This workstream does not add new TypeScript syntax
support; it makes the currently supported subset more regular and easier to
extend later.

## Current State

IR1 is currently a TypeScript-shaped canonical layer. Value-position code
lowers through `IR1Expr -> IRExpr -> OpaqueExpr`; mutating bodies lower
directly through `lowerL1Body` in `tools/ts2pant/src/ir1-lower-body.ts`, which
threads `SymbolicState` from `tools/ts2pant/src/translate-body.ts` and emits
`PropResult[]`.

That design is SSA-like but not SSA. IR1 still contains `assign`, `while`,
`for`, `foreach`, `cond-stmt`, `map-effect`, and `set-effect` statement forms.
Branch joins are encoded by mutating cloned `SymbolicState` values and merging
write keys with `cond` expressions. Map/Set semantics, `Set.clear()`, staged
read-through-write behavior, Shape A foreach equations, and Shape B accumulator
folds all live as special paths in the lowering code.

## Key Challenges

- **Changing IR1 directly**: This workstream intentionally changes the IR1
  contract rather than adding a new post-IR1 layer. That means the migration
  must keep the codebase runnable at every milestone while the central IR type
  changes.
- **Location SSA, not local-variable SSA**: The important mutable names are
  Pantagruel state locations: property rules at receivers, Map value and
  membership rules, Set membership at receivers, and frameable rule symbols.
- **Joins without explicit Pant phi nodes**: IR1 needs phi-like nodes for branch
  joins, but final Pantagruel output still erases them to `cond` expressions,
  overrides, quantified equations, or frame equations.
- **Map/Set semantics are not scalar assignments**: Map writes coalesce into
  tuple-keyed overrides, Set writes use element overrides plus a symbolic
  `cleared` predicate, and staged reads must observe writes in the same path.
- **Loop support is summary-based today**: General loop SSA is out of scope for
  this workstream because general loop syntax is not currently supported.
  Existing support consists of μ-search, Shape A foreach, and Shape B
  accumulator folds. SSA must represent those summaries explicitly without
  promising arbitrary loop fixed points.
- **Rip-and-replace risk**: Byte-for-byte compatibility is not required, but
  semantic parity for currently supported fixtures is required. Test coverage
  must compare behavior at the Pantagruel/wasm-check level, not only snapshots.

## Milestones

### Milestone 1: ir1-ssa-contract

**Definition of Done**:
`tools/ts2pant/src/ir1.ts` defines IR1 as the SSA-bearing semantic IR for both
value and effect positions. The type vocabulary includes explicit state
locations, SSA versions, reads, writes, branch joins, Map effects, Set effects,
frameable modified-rule tracking, and loop-summary forms for existing foreach
Shape A / Shape B and μ-search outputs. The document comments in `ir1.ts`,
`tools/ts2pant/CLAUDE.md`, and this workstream agree on the new IR1 contract.
No production builder is required to emit the new forms yet, but the new types
compile and unit tests can construct representative IR1 SSA values.

**Why this is a safe pause point**:
The milestone is type-level and documentation-level. Existing translation
behavior can continue to run through the old lowering path while the new IR1
contract is reviewed in isolation.

**Unlocks**:
Implementation of the new builder and lowerer against a stable vocabulary.

**Open Questions**:
None. Version identity is settled: IR1 SSA uses opaque version symbols, with
`version-location` / equivalent metadata linking each version back to its
location. Degenerate joins are structurally invalid in final IR1 SSA; shared
builder helpers should simplify them away before constructing join nodes.

---

### Milestone 2: ir1-ssa-builder-scalars

**Definition of Done**:
The mutating-body build path emits IR1 SSA for scalar property mutation and
simple branch mutation:

- direct property assignment
- compound property assignment after desugaring
- read-after-write on property access
- single-arm `if` with optional `else`
- nested supported `if` bodies
- early-exit guard patterns that are currently supported

The old `SymbolicState` path remains available only as a fallback for Map/Set
and foreach constructs not migrated in this milestone. Property mutation
fixtures are validated through snapshots and wasm typechecking.

**Why this is a safe pause point**:
The codebase supports all previous scalar property-mutation behavior. Unsupported
or not-yet-migrated state classes still use the old path, so no existing
supported syntax is lost.

**Unlocks**:
Removal of scalar property logic from `SymbolicState` and a concrete proof that
IR1 can own SSA generation directly.

---

### Milestone 3: ir1-ssa-map-set

**Definition of Done**:
Map and Set mutation semantics move into IR1 SSA:

- `Map.set` and `Map.delete` create explicit SSA writes over value and
  membership locations.
- `Map.get` and `Map.has` reads resolve through staged SSA writes.
- `Set.add`, `Set.delete`, and `Set.clear` create explicit membership and
  cleared-state SSA updates.
- `Set.has` reads resolve through staged writes and cleared predicates.
- Branch joins for Map/Set use IR1 join nodes rather than `mergeOverrides`
  over cloned `SymbolicState`.
- Existing Map/Set mutating fixtures pass snapshots and wasm typechecking.

The old Map/Set-specific mutation primitives in `translate-body.ts` are either
deleted or reduced to pure helper functions used by IR1 lowering.

**Why this is a safe pause point**:
All currently supported non-looping mutation classes are represented in the new
IR1 SSA architecture. Remaining old-path usage is limited to loop summaries and
special forms.

**Unlocks**:
A single SSA read/write/join model for scalar properties, Maps, and Sets.

**Open Questions**:
None. Map state is modeled as two coordinated SSA locations: one for the value
rule and one for the membership predicate. `Set.clear()` remains inside the
Set membership location's SSA value rather than becoming a separate location.

---

### Milestone 4: ir1-ssa-loop-summaries

**Definition of Done**:
Existing loop-like support is represented in IR1 SSA without claiming general
loop SSA:

- μ-search keeps a first-class SSA summary that lowers to the existing
  `min over each` form.
- foreach Shape A emits explicit quantified write summaries in IR1, producing
  distinct summary versions rather than ordinary scalar write versions.
- foreach Shape B emits explicit accumulator-fold summaries in IR1, producing
  distinct summary versions rather than ordinary scalar write versions.
- Build-time in-iteration read-after-write behavior is represented without
  ad hoc `subState` mutation.
- Loops that are currently unsupported remain unsupported with at least as
  clear a diagnostic as today.

Existing foreach, forEach, accumulator-fold, and μ-search fixtures pass
snapshots and wasm typechecking.

**Why this is a safe pause point**:
All currently supported mutation and loop-summary behavior now flows through
IR1 SSA. General `for` / `while` support remains explicitly out of scope, so
the codebase does not imply support it cannot provide.

**Unlocks**:
Full removal of `lowerL1Body`'s old `SymbolicState` execution model.

**Open Questions**:
None. Loop summaries produce distinct summary versions. They can satisfy reads
and mark rules modified, but they lower through quantified/fold-specific paths
rather than ordinary point-write paths.

---

### Milestone 5: ir1-ssa-propresult-lowering

**Definition of Done**:
IR1 SSA has one lowering path to `PropResult[]`:

- property SSA final versions lower to primed equations
- Map SSA final versions lower to value and membership override equations
- Set SSA final versions lower to quantified membership assertions
- branch joins lower to `cond` expressions or override values
- loop summaries lower to quantified equations / accumulator equations
- frame conditions derive from IR1's modified-rule set, not from
  `SymbolicState.modifiedProps`

`tools/ts2pant/src/ir1-lower-body.ts` is either rewritten as the IR1 SSA lowerer
or replaced by a clearly named module. `translate-body.ts` no longer owns
mutation semantics beyond orchestration and helper utilities.

**Why this is a safe pause point**:
The new IR1 SSA architecture is end-to-end for all currently supported syntax.
The old lowering machinery may still exist as dead code, but the production path
does not depend on it.

**Unlocks**:
Code deletion and simplification.

---

### Milestone 6: ir1-ssa-ripout

**Definition of Done**:
The obsolete symbolic-state execution machinery is removed or narrowed to
small pure utilities:

- `SymbolicState` no longer serves as the semantic model for mutating bodies.
- `putWrite`, `addWrittenKey`, `mergeOverrides`, `installMapWrite`,
  `installSetWrite`, `readMapThroughWrites`, and `readSetThroughWrites` are
  deleted, renamed, or moved behind IR1 SSA lowering if still genuinely useful.
- `translate-body.ts` no longer contains parallel mutation paths.
- Comments in `tools/ts2pant/CLAUDE.md` and the old imperative-IR workstream
  are updated to mark `SymbolicState` as superseded.
- Full ts2pant unit and integration suites pass.

**Why this is a safe pause point**:
There is only one mutation architecture left. The codebase is smaller, the
supported syntax is unchanged, and unsupported syntax still rejects
conservatively.

**Unlocks**:
Future syntax work can target IR1 SSA directly instead of extending special
symbolic-execution paths.

---

### Milestone 7: ir1-ssa-invariants

**Definition of Done**:
The new architecture has direct invariant tests and developer-facing
documentation:

- unit tests assert each write produces a fresh SSA version
- branch joins require compatible location kinds
- every read resolves to a dominating version, initial version, or explicit
  loop summary
- every modified Pantagruel rule suppresses frame generation exactly once
- unsupported loops and unsupported branch shapes produce targeted diagnostics
- property-based or table-driven tests cover representative scalar, Map, Set,
  branch, foreach, and μ-search programs

**Why this is a safe pause point**:
The migration is complete and guarded by tests that target the new abstraction,
not just emitted snapshots. Future refactors can rely on explicit invariants.

**Unlocks**:
Follow-on work to add new syntactic forms, generalize loops, or reduce remaining
legacy expression-path complexity.

## Dependency Graph

```text
1 (ir1-ssa-contract) -> []
2 (ir1-ssa-builder-scalars) -> [1]
3 (ir1-ssa-map-set) -> [2]
4 (ir1-ssa-loop-summaries) -> [3]
5 (ir1-ssa-propresult-lowering) -> [4]
6 (ir1-ssa-ripout) -> [5]
7 (ir1-ssa-invariants) -> [6]
```

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Frame granularity | Frames are emitted per Pantagruel rule. Internally, SSA could track only modified rules or track modified locations and derive rules afterward. | Milestone 5 |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Change IR1 directly | The goal is clean architecture, not a compatibility layer. Adding another post-IR1 layer would preserve the current split rather than fixing it. |
| No new TypeScript syntax in this workstream | The purpose is to regularize semantics for the currently supported subset. New syntax can come after the SSA model is stable. |
| Rip-and-replace is acceptable | Byte-for-byte compatibility with the existing `SymbolicState` implementation is less important than ending with one coherent architecture. |
| Prioritize clean architecture | Correctness and future extensibility matter, but the leading constraint is removing special code paths and making mutation semantics explicit in IR1. |
| Use location SSA | Pantagruel state is rule/location-oriented, so SSA over local variable names would miss the real mutation model. |
| Use opaque version identities | Versions are opaque symbols allocated by the IR1 SSA builder. Each version records its location; callers do not rely on location-scoped numeric counters. |
| Forbid degenerate joins structurally | A final IR1 SSA join must represent a real merge. If both branches resolve to the same version, or the result would be identical to an input, the builder should simplify instead of constructing a join. |
| Model Map state as two coordinated locations | This mirrors Pantagruel's emitted value rule and membership predicate, and it lets frames remain rule-oriented. |
| Keep `Set.clear()` inside the Set membership value | `clear` changes how membership falls through; treating it as part of the Set location's SSA value avoids inventing a separate artificial location. |
| Represent early exits as ordinary branch joins where possible | The existing if-conversion semantics should map to SSA branch joins unless implementation evidence shows a dedicated continuation-summary node is cleaner. |
| Use distinct loop-summary versions | Shape A/B foreach effects are quantified/folded summaries, not point writes. Distinct summary versions keep that lowering distinction explicit while still allowing reads and modified-rule tracking. |
| Require semantic parity, not byte-for-byte output parity | Output must pass `pant`, preserve supported behavior, and support compiling TypeScript together with handwritten Pantagruel annotations in comments. Snapshot churn is acceptable when the emitted spec remains valid. |
| Prefer smaller obvious modules | Use focused modules such as `ir1-ssa-build.ts`, `ir1-ssa-lower.ts`, and related helpers rather than concentrating the new architecture in one large file. |
| Keep general loops out of scope | Current support is summary-based. A comprehensive general-loop SSA/fixed-point design should be a future workstream, not smuggled into this migration. |
