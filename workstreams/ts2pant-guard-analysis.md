# Workstream: ts2pant Flow-Sensitive Guard & Effect Analysis

## Vision

Make ts2pant reason about *which facts hold where*. Two capabilities: (1) treat
calls to pure user functions as first-class values/predicates so a condition like
`if (isValid(x))` lowers to the synthesized EUF rule instead of being rejected as
effectful; and (2) a flow-narrowing layer that propagates a test's truth into its
branch (`if (P) { … assume P … }`), so guards, nullish checks, discriminant
equalities, and type predicates discharge the obligations that depend on them.
The end state unblocks the largest remaining dogfood class (~66 functions whose
predicates call helpers) and provides the shared narrowing layer the
discriminated-union workstream's M3 was promised to consume.

## Current State

Guard *extraction* is largely complete and conservative (the 46-day-old roadmap's
first two items are done): `isAssertionCall` recognizes `asserts cond` functions
via `ts.TypePredicateNode.assertsModifier` (`translate-signature.ts:298–371`);
if-throw / if-else-throw guards are classified by `classifyGuardIf` (~712); and
**conservative interprocedural call-following** is implemented in `followGuards` /
`resolveCallTarget` (~380–527) — it substitutes actuals for formals and bails on
recursion (visited set), dynamic dispatch, and `node_modules`. Extracted guards
attach to the emitted rule/action as preconditions (`types.ts` `PantRule.guard` /
`PantAction.guard`; emitted at `emit.ts:154–165`).

Two things are **not** done:

- **User calls in condition/predicate positions are rejected.**
  `expressionHasSideEffects` (`translate-body.ts:3072`) returns `true` for *any*
  non-builtin call, so a predicate `if (isValid(x)) …` is refused at
  `translate-body.ts:2397` ("early-return predicate has side effects") and
  `ir1-build-body.ts:958` ("impure if-condition in mutating body"). Free-call-decl
  now synthesizes an EUF rule head for such a call in *value* position, but the
  purity gate rejects it before lowering in *condition* position. There is no
  user-function purity classifier — calls default to effectful.

- **Flow narrowing does not exist** — it is explicitly greenfield. Branches are
  lowered context-free; nothing propagates a test's truth into its branch
  (`ir1-build.ts:1173` defers literal-union narrowing; `nullish-recognizer.ts`
  deliberately avoids relying on TS flow-narrowing). Reading a discriminant or a
  nullish guard produces no assumption usable inside the branch.

Effect-TS *purity* classification exists (`purity.ts` `resolveEffectLibraryExport`
+ a large allowlist), but **error-channel extraction** (`Effect<A,E,R>` error
union, `yield* new ErrorClass()`) is unimplemented.

## Key Challenges

- **Soundness of user-function purity inference.** Misclassifying an effectful
  function as pure would let an unsound fact into a guard/condition. The analysis
  must be conservative-by-bail (unknown ⇒ effectful), reusing the existing
  call-following conservatism (recursion/dynamic-dispatch/node_modules/IO/mutation
  ⇒ bail). Inferred, not annotated (see Decisions).

- **Flow narrowing is greenfield and the deepest piece.** Propagating a predicate
  truth into a branch as an assumption — and threading it through ts2pant's IR/SSA
  — is new. The any/unknown-opaque workstream already flagged that TS flow
  narrowing "does not always survive ts2pant's intermediate representations." The
  layer must be generic enough to serve four consumers (discriminant equality,
  nullish, user type-predicates, boolean guards) without special-casing any field
  name. Its exact shape is the reason for a survey milestone before building it.

- **Intra-function only.** Cross-call and assertion-function *narrowing* (as
  opposed to guard extraction, which already follows calls) stays out of scope,
  matching the any/unknown-opaque workstream's "cross-function-call narrowing not
  trusted by default" decision.

- **Two downstream consumers depend on this.** The DU workstream's M3 consumes the
  flow-narrowing layer; the nullish recognizer and any/unknown handling can consume
  it too. The layer's interface is a cross-workstream contract.

## Established Precedents

- **paper — Tobin-Hochstadt & Felleisen, "Logical Types for Untyped Languages" (occurrence typing, ICFP 2010)** — https://www2.ccs.neu.edu/racket/pubs/icfp10-thf.pdf
  The canonical model for the flow-narrowing layer (M3): a predicate in test
  position refines the facts that hold in each branch. TypeScript's own narrowing
  is informally occurrence typing; ts2pant's variant refines an *assumption set*
  whose facts are discharged by z3 rather than a type (cf. "occurrence typing
  modulo theories", Kent et al. PLDI 2016). Shapes how M3 represents a branch
  assumption and what predicate forms it recognizes.

- **existing-implementation — in-repo conservative guard call-following** —
  `tools/ts2pant/src/translate-signature.ts` (`followGuards`, `resolveCallTarget`, `buildSubstitutionMap`, ~380–527)
  The conservatism template every milestone reuses: resolve a direct call to a
  block-bodied local function, substitute actuals for formals, recurse with a
  visited set, and bail (no fact extracted, never a wrong fact) on recursion /
  dynamic dispatch / `node_modules`. M1's user-function purity inference and M4's
  Effect extraction follow the same bail discipline.

- **paper — Kroening & Strichman, Decision Procedures Ch. 1–4 (path conditions / EUF)** — https://www.decision-procedures.org/
  The narrowed predicate becomes an *assumption* (path condition) discharged by the
  solver when checking the branch's obligations; pure user calls lower to
  uninterpreted functions whose only built-in axiom is congruence. The basis for
  why an inferred-pure call and a narrowed guard are sound to assume.

- **documentation — TypeScript Compiler API** — https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API
  `getTypePredicateOfSignature` / `assertsModifier` (already used for assertions;
  reused in M3 for user `x is T` type-predicate narrowing), `getSymbolAtLocation`
  and declaration resolution (M1 callee resolution), and control-flow inspection.

## Milestones

### Milestone 1: guard-purity-user-calls

**Definition of Done**:
- A conservative user-function **purity classifier**: a called function is *pure*
  iff its body has no mutation of non-locals, no IO/throw-as-effect, no calls to
  effectful functions (recursive, visited-set guarded), and is resolvable
  (block-bodied, not `node_modules`, not dynamic dispatch) — reusing the
  `resolveCallTarget` discipline. Unknown ⇒ effectful (bail).
- A call to a pure user function in a **condition / early-return predicate /
  while-predicate / mutating-if-condition** position is no longer rejected by
  `expressionHasSideEffects`; it lowers to the EUF rule head free-call-decl
  synthesizes for that call (`is-valid x` etc.), so the predicate becomes a
  Bool-valued rule application.
- Effectful calls in those positions still reject with their existing diagnostics
  (conservative refusal preserved).
- The dogfood/constructs fixtures blocked solely on "early-return predicate has
  side effects" / "impure if-condition in mutating body" (where the called
  predicate is pure) now type-check; their allowlist entries are removed.

**Why this is a safe pause point**: Purely additive to the purity oracle and the
condition-lowering path; effectful calls still reject, so nothing unsound is
admitted. Predicates over pure user calls now translate, a meaningful standalone
dogfood win, with or without any narrowing.

**Unlocks**: M2 can measure the post-purity dogfood residual and inventory the
narrowing patterns that remain.

**Operator Actions Before Next Milestone**:
- Run `npm run -s test:unit` / `test:integration`; confirm the targeted
  free-call-decl-adjacent fixtures now pass and no effectful predicate slipped
  through (spot-check a few inferred-pure classifications against the callee body).

**Open Questions**:
- Exact purity predicate boundary (are local `const` bindings / pure loops inside
  the callee allowed? are recursive-but-pure functions classified pure or bailed?).
  Resolve during M1's gameplan.

---

### Milestone 2: guard-narrowing-survey

**Definition of Done**:
- Read-only survey over the dogfood + constructs corpus, run after M1: how many
  functions M1 unblocked; what remains; and an **inventory of the narrowing
  patterns** that would discharge remaining obligations — discriminant equality
  (`x.kind === lit`), nullish (`x != null`), user type-predicates (`x is T`),
  boolean guards (`if (P) …`) — with counts and intra- vs cross-function split.
- A committed report (under `tools/ts2pant/docs/`) proposing the M3 flow-narrowing
  layer's interface and representation (how a branch assumption is carried and
  discharged), and whether M3 should be one milestone or split by pattern.
- Names the cross-workstream consumers (DU M3, nullish, any/unknown) and the
  interface each needs.
- No translator behavior change — measurement only.

**Why this is a safe pause point**: Nothing changed in emitted output; the
codebase is exactly as after M1 plus a report. The measure→decide boundary that
justifies designing the greenfield narrowing layer against evidence rather than
guessing.

**Unlocks**: An evidence-based M3 design + scope, and a concrete narrowing
interface the DU workstream can commit to.

**Operator Actions Before Next Milestone** (the measure→decide boundary):
- **Review the survey and decide M3's scope** — which narrowing patterns to
  implement and whether M3 splits into per-pattern milestones. Proposed gating: a
  pattern is in M3 iff it appears across multiple corpus functions OR a downstream
  workstream (DU M3) hard-depends on it (discriminant equality qualifies on the
  latter regardless of corpus count).
- **Confirm the interface** the DU workstream and nullish/any-unknown will consume,
  so M3 builds the shared layer, not a bespoke one.

**Open Questions**:
- One milestone vs. split-by-pattern for M3. Resolved by this survey.

---

### Milestone 3: guard-flow-narrowing

**Definition of Done**:
- A **flow-narrowing layer**: within a function, a test's truth is propagated into
  its branch as an assumption usable by lowering — covering (at least) boolean
  guards and the patterns the M2 survey selected (discriminant equality, nullish,
  user `x is T` type-predicates). The narrowed assumption discharges obligations in
  the branch (e.g. a discriminant-guarded variant-field rule, a non-null access)
  and is rendered so z3 can use it as a path condition.
- **Intra-function only**; cross-call / assertion-function narrowing remains
  refused (matches the any/unknown-opaque decision).
- No field-name special-casing — discriminant narrowing keys on the structural
  predicate (`<field> === <literal>`), not on `.kind`.
- The layer exposes a stable interface the DU workstream's M3 consumes.

**Why this is a safe pause point**: Narrowing is additive — un-narrowed code keeps
its current (sound-by-guard) lowering; narrowed branches gain provable facts.
ts2pant now reasons flow-sensitively within a function.

**Unlocks**: The discriminated-union workstream's M3 (`du-discriminant-narrowing`)
— a **cross-workstream unlock**; richer nullish and any/unknown handling.

**Operator Actions Before Next Milestone**:
- Confirm before/after entailment set shows the targeted narrowing obligations now
  discharge and nothing regressed.

**Established Precedents** (milestone-scoped): the workstream-level occurrence-typing
and path-condition precedents are consumed here directly.

**Open Questions**:
- The propagation surface through ts2pant's IR/SSA (the any/unknown workstream
  flagged narrowing "does not always survive" the IR). Resolve during M3's gameplan
  using the M2 interface proposal.

---

### Milestone 4: guard-effect-error-channel

**Definition of Done**:
- Extract the error channel `E` from `Effect<A, E, R>` signatures to enumerate
  failure modes, and AST-match `yield* new ErrorClass(...)` inside `if` to recover
  the actual guard condition, feeding both into Pantagruel action preconditions —
  reusing the existing Effect purity/symbol-resolution infrastructure
  (`purity.ts:resolveEffectLibraryExport`) and the M1 purity classifier.
- Conservative: bail (no precondition) on Effect shapes that don't match, never a
  wrong one.

**Why this is a safe pause point**: Additive precondition source for Effect code;
non-Effect code unaffected. Terminal milestone — the old roadmap's third item.

**Unlocks**: Richer static failure-mode info for Effect-TS codebases. Closes the
workstream.

**Operator Actions Before Next Milestone**: none (terminal).

**Established Precedents** (milestone-scoped):
- **library — Effect-TS** — https://effect.website
  Source of the `Effect<A, E, R>` type shape and the `yield* new ErrorClass()`
  error-yield idiom this milestone pattern-matches.

**Open Questions**:
- Whether branded types / `Schema.positive()` filters are in scope or deferred
  (name-based pattern matching, lower confidence). Resolve during M4's gameplan.

## Dependency Graph

```text
1 (guard-purity-user-calls)    → []
2 (guard-narrowing-survey)     → [1]
3 (guard-flow-narrowing)       → [2]   ── unlocks DU workstream M3 (cross-workstream)
4 (guard-effect-error-channel) → [1]   (parallelizable with 2/3; sequenced last)
```

**Note**: M1 → M2 → M3 is the critical path (the flow-narrowing layer). M4 depends
only on M1 (Effect precondition extraction is independent of narrowing) and may run
in parallel, but is positioned last by priority. **M3 is the cross-workstream
unlock** for the discriminated-union workstream's `du-discriminant-narrowing`
milestone, which is gated on this layer existing.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Purity predicate boundary for user functions | const bindings / pure loops / recursive-pure inside the callee | Milestone 1 |
| Which narrowing patterns does M3 implement, and does M3 split? | Driven by the survey + the DU hard-dependency on discriminant equality | Milestone 2 |
| Flow-narrowing interface the DU / nullish / any-unknown workstreams consume | Cross-workstream contract | Milestone 2 |
| Propagation surface through IR/SSA | any/unknown flagged narrowing may not survive the IR | Milestone 3 |
| Effect branded types / Schema filters in scope? | Name-based, lower confidence | Milestone 4 |

## Decisions Made

These are decisions that did NOT produce a citable precedent — framing,
constraints, and rejected approaches. Accepted precedents (occurrence typing,
in-repo call-following, Kroening & Strichman, TS Compiler API, Effect-TS) live in
`Established Precedents`.

| Decision | Rationale |
|----------|-----------|
| User-function purity is **inferred** from the callee body, not annotated | Consistent with the existing conservative call-following; no new `@pure` annotation burden, and sound-by-bail (unknown ⇒ effectful) rather than trusting a possibly-mismarked human annotation. |
| Both condition-purity and flow-narrowing live in **one workstream** | They are the two facets of "reason about which facts hold where"; flow-narrowing (M3) is the layer the DU workstream was promised to consume, so it belongs on the same roadmap rather than orphaned. |
| Flow narrowing is **intra-function only** | Matches the any/unknown-opaque workstream's "cross-function-call narrowing not trusted by default" decision. Guard *extraction* still follows calls (already implemented); narrowing does not. |
| No field-name / discriminant special-casing in narrowing | Inherits the project-wide constraint: discriminant-equality narrowing keys on the structural predicate `<field> === <literal>`, never on `.kind` by name. |
| Effect E-channel extraction is the **final** milestone, not dropped | The user opted to keep it; it is not on either critical path (dogfood lever M1, DU dependency M3), so it is sequenced last and depends only on M1. |
| Guard *extraction* (asserts, if-throw, call-following) is treated as **already done** | Verified in current code (`translate-signature.ts`); the old roadmap's items 1–2 are implemented, so this workstream does not re-plan them. |
