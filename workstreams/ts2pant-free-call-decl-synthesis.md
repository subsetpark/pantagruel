# Workstream: ts2pant Free-Call-Decl Synthesis

## Vision

Close the `free-call-decl` gap: every TypeScript call that ts2pant accepts
structurally but cannot model — unknown user functions, stdlib methods not in
the dispatch table — gets a Pantagruel rule declaration so the emitted document
type-checks, and the high-value builtins get sound EUF axioms so dogfood `@pant`
assertions actually *entail* under z3 rather than merely parsing. The end state
turns "structurally accepted, emits `> UNSUPPORTED: free-call-decl`" into
"declared and (where it matters) provable," unblocking the next wave of dogfood
corpus expansion that the Local-Binding SSA workstream's `let` support set up.

## Current State

The Local-Binding SSA workstream is complete (M2 `let-mutation-ssa` landed via
PRs #264–#268). `let` is no longer a structural blocker, which pushed the
dominant failure class downstream: `tools/ts2pant/tests/known-typecheck-failures.mts`
now carries **20 `free-call-decl` entries** — well above the ≥10 threshold the
Local-Binding workstream set as the criterion for justifying this work.

ts2pant already has most of the machinery this workstream extends:

- **Dispatch table** — `BUILTINS` in `tools/ts2pant/src/builtins.ts` maps TS
  surface forms (`Math.max`, `String.prototype.toUpperCase`, …) to qualified
  Pant rule references. `lookupBuiltinByCall` resolves a `ts.CallExpression`
  symbol-based (not textual), gating on arity.
- **Shared EUF modules** — `samples/js-stdlib/{JS_MATH,JS_STRING,TS_PRELUDE}.pant`
  declare EUF rule heads with signatures and **no body axioms**, deliberately:
  the `JS_STRING.pant` header states that JS string semantics (locale-sensitive
  case folding, surrogate pairs) are too brittle to axiomatize soundly, so
  downstream consumers rely on EUF congruence alone (Kroening & Strichman Ch. 4).
- **Signature inference** — `extract.ts:301` and `translate-signature.ts:304/384`
  already call `checker.getSignatureFromDeclaration` / `getResolvedSignature` to
  recover param/return types; `translate-signature.ts` maps TS types to Pant
  sorts.
- **Declaration + import emission** — `PantRule` / `PropResult` (`types.ts`) are
  the declaration vocabulary; `emit.ts:90` emits `import <MOD>.` for dep modules;
  `emit.ts:69/168` emit the `> UNSUPPORTED: <reason>` doc-comment lines.

What is **missing**: any path that emits a rule *declaration* for a call that
does not dispatch to a known builtin. `extract.ts:911` notes the bug directly —
such calls fall "through to a raw TS identifier, emitting an undeclared Pant"
reference, so the wasm typecheck fails on an undeclared identifier. The 20
`free-call-decl` entries split into two categories: **unknown user functions**
(`freeCall`, `zeroArityCall`, `nestedCalls`, `methodCall`, `callWithPropArg`,
`constImpureCall`, `effectfulConstRejected`, `bubbleNegation`, …) and **known
stdlib not yet in the dispatch table** (`String.prototype.replace`,
`Array.from`, `Array.prototype.length`-on-derived-arrays, `Map.prototype.
{entries,values,keys}`, `Set.prototype.has`, the `constArrayFromMap` /
`constMapEntries` / `chainedArrayCount` cluster).

## Key Challenges

- **Two distinct synthesis paths.** Unknown user functions need a rule
  declaration synthesized *into the emitted document* (local, per-document).
  Known stdlib methods belong in the *shared* `js-stdlib` modules (cross-document
  rule identity, the natural home for axioms). Keeping these two mechanisms
  coherent — and deciding dispatch precedence between them — is the central
  design tension.

- **Typed signature synthesis vs the any/unknown frontier.** Synthesizing a
  *typed* Pant rule head from a TS signature (`unknown-fn x: Int => Int`) gives
  far more entailment leverage than a uniformly-opaque head, but a TS signature
  that is `any`/`unknown` in a parameter or return position has no concrete Pant
  sort. This intersects the (drafted, not-started) `ts2pant-any-unknown-opaque`
  workstream. This workstream must not block on that one — it needs a
  self-contained conservative fallback for any/unknown-typed positions.

- **EUF soundness is the floor's whole safety story.** The floor declares rules
  with congruence-only semantics (no axioms). That is sound by construction
  (Kroening & Strichman Ch. 4) but proves nothing beyond `f(x) = f(x)`. The
  danger is in the *axiom* milestone: any unsound axiom silently validates false
  assertions. The existing `JS_STRING.pant` refusal to axiomatize string methods
  is the precedent — M4 must be conservative and justify every axiom's soundness.

- **Entailment ≠ type-checking.** The headline lesson from the Local-Binding
  workstream's dogfood expansion: assertions can type-check while proving nothing
  substantive (the trivial `f x = f_impl x` + reflexivity pattern). This
  workstream's value is measured by *entailment*, which is exactly why it needs a
  measurement milestone (M3) between the floor and the axioms — the measure→decide
  boundary that makes this a workstream, not a single gameplan.

- **Distinguishing "needs an axiom" from "needs a different encoding."** Some
  builtins (`Map.entries`, `Map.values`) already have a structured Map encoding
  elsewhere in ts2pant; closing their entailment gap may mean *connecting to the
  existing encoding* rather than adding a fresh axiom. The survey (M3) must
  classify which is which.

## Established Precedents

- **paper — Kroening & Strichman, *Decision Procedures* Ch. 4 (Equality with
  Uninterpreted Functions)** — https://www.decision-procedures.org/
  The EUF/congruence foundation the entire floor rests on. Already cited in
  `builtins.ts` and the `js-stdlib` module headers. Every synthesized rule head
  (local or shared) is sound under congruence alone; congruence is the *only*
  built-in axiom for opaque calls. Every milestone touches this — the floor
  relies on it for safety, the survey measures against it, the axiom milestone
  carefully extends beyond it.

- **existing-implementation — js-stdlib dispatch architecture** —
  `tools/ts2pant/src/builtins.ts` + `samples/js-stdlib/*.pant`
  The in-repo precedent for modeling stdlib calls: a data-only dispatch table
  pointing at hand-written `.pant` modules that sit on the same CI typecheck path
  as the samples corpus. M2 extends this verbatim (new JS_ARRAY/JS_MAP/JS_SET
  modules + table entries); M4 adds axioms to those modules. The architecture's
  "modules are the source of truth for signatures and axioms; the table is a thin
  loader" discipline is load-bearing.

- **documentation — TypeScript Compiler API (`getResolvedSignature`,
  `getSignatureFromDeclaration`, `getTypeAtLocation`)** —
  https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API
  The source of truth for synthesizing typed rule heads. M1 reads the resolved
  signature of each free call to recover param/return TS types, then maps them to
  Pant sorts via the existing `translate-signature.ts` machinery. The fidelity of
  the typed floor depends on correctly handling overloads, generics, and the
  any/unknown frontier.

- **algorithm — McCarthy theory of arrays (select/store) and finite-set
  cardinality** — https://doi.org/10.1145/321033.321034
  The candidate axiom shapes for M4's structural builtins: `Array.prototype.length`
  ≥ 0, the relationship between `Map` size and `Array.from(map.values()).length`,
  `Set.prototype.has` membership reflexivity. These are sound structural axioms
  (unlike string semantics) expressible in the SMT-LIB array/set theories Pant
  already lowers to. Only consumed by M4.

## Milestones

### Milestone 1: free-call-opaque-floor

**Definition of Done**:
- Every TS call that does not dispatch to a known builtin and is not otherwise
  modeled gets a synthesized Pant rule declaration emitted into the document,
  instead of falling through to an undeclared identifier (`extract.ts:911`).
- The rule head is **typed from the TS resolved signature**: param and return
  sorts come from `checker.getResolvedSignature` mapped through the existing
  `translate-signature.ts` type→sort machinery. A parameter or return position
  typed `any`/`unknown` falls back to a conservative local Opaque sort (a minimal
  self-contained encoding — NOT a dependency on the `ts2pant-any-unknown-opaque`
  workstream; if that workstream later lands, this fallback is superseded).
- Declarations are de-duplicated within a document (one head per distinct
  function identity + arity), matching the dedup discipline at `extract.ts:310`.
- All 20 `free-call-decl` fixtures that involve unknown user functions now
  type-check; their allowlist entries are removed. Stdlib-method fixtures still
  on the floor (declared locally as opaque) also type-check and lose their
  entries.
- Pure EUF — no body axioms emitted.

**Why this is a safe pause point**: Every accepted document type-checks; the
`free-call-decl` failure class is gone from the allowlist. Assertions over these
calls may not entail (congruence only), but nothing regresses — the floor is
sound by construction. If the team stops here permanently, ts2pant emits
well-formed Pant for every structurally-accepted function, which is a meaningful
standalone improvement.

**Unlocks**: M2 (upgrade high-frequency stdlib calls to shared modules) and M3
(measure which assertions still fail to entail).

**Operator Actions Before Next Milestone**:
- Run `npm run -s test:unit` and `npm run -s test:integration`; confirm zero
  `free-call-decl` entries remain for user-function fixtures and no snapshot
  regressions on the existing corpus.
- Spot-check 2–3 synthesized rule heads against their TS signatures to confirm
  the type→sort mapping is faithful (no silent Opaque fallback where a concrete
  sort was available).

**Established Precedents** (milestone-scoped):
- **documentation — TypeScript Compiler API** —
  https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API —
  M1's typed synthesis reads `getResolvedSignature`; correct overload/generic
  handling is the fidelity gate.

**Open Questions**:
- Dispatch precedence: when a call *could* be a known builtin (M2's territory)
  but M2 hasn't landed yet, M1 synthesizes a local opaque head. After M2, the
  dispatch table takes precedence. Confirm the precedence order (table first,
  local synthesis as fallback) is encoded so M2 is a pure upgrade with no
  double-declaration.

---

### Milestone 2: stdlib-dispatch-coverage

**Definition of Done**:
- `BUILTINS` (`builtins.ts`) gains entries for the high-frequency stdlib methods
  in the `free-call-decl` corpus: `String.prototype.replace` (string-arg form),
  `Array.from`, `Array.prototype.length` (on derived arrays), `Map.prototype.
  {entries,values,keys}`, `Set.prototype.has`. Arity gating follows the existing
  `deriveBuiltinSpec` discipline.
- New shared modules `samples/js-stdlib/{JS_ARRAY,JS_MAP,JS_SET}.pant` (and
  extensions to `JS_STRING.pant`) declare the corresponding EUF rule heads —
  signatures only, **no body axioms** (the established `js-stdlib` design).
- The stdlib-method fixtures (`constArrayFromMap`, `constMapEntries`,
  `chainedArrayCount`, `constReplaceLiteral`, …) dispatch to the shared modules
  rather than M1's local synthesis; emitted documents carry `import <MOD>.`.
- These modules sit on the samples CI typecheck path (no bespoke test infra).

**Why this is a safe pause point**: Still pure EUF; documents type-check; no
entailment regression. The only behavioral change is that stdlib calls now route
through shared modules with curated signatures instead of per-document opaque
synthesis — higher fidelity and a stable home for M4's axioms. If the team stops
here, the floor is simply cleaner and more shareable.

**Unlocks**: M3 can attribute entailment gaps to specific shared-module rules
(e.g. "blocked on a missing `JS_MAP::entries` axiom").

**Operator Actions Before Next Milestone**:
- Confirm the new `.pant` modules typecheck on the samples CI path.
- Confirm no fixture that dispatched under M1's local synthesis regressed to
  `free-call-decl` (the table must cover everything it claims).

**Established Precedents** (milestone-scoped):
- **existing-implementation — js-stdlib dispatch architecture** —
  `tools/ts2pant/src/builtins.ts` + `samples/js-stdlib/*.pant` — M2 replicates
  the JS_MATH/JS_STRING pattern for the array/map/set namespaces verbatim.

---

### Milestone 3: entailment-survey

**Definition of Done**:
- A read-only survey runs across the dogfood + constructs fixture corpus: for
  each `@pant` assertion that involves a free/builtin call, classify the
  assertion as (a) type-checks, (b) entails under z3 (`pant --check`), (c) if it
  does not entail, *why* — and whether the gap is closable by a **soundly
  modelable** axiom on a specific shared-module rule (vs needing a different
  encoding, e.g. connecting `Map.entries` to ts2pant's existing Map encoding, vs
  being fundamentally unprovable).
- Output is a committed survey report (a doc under `tools/ts2pant/docs/` or a
  generated test artifact) with a per-builtin breakdown: rule, blocked-assertion
  count, proposed axiom (if any), soundness justification sketch, encoding-vs-axiom
  verdict.
- No source-translator behavior change — measurement only.

**Why this is a safe pause point**: Nothing changed in the emitted output; the
survey is pure observation. The codebase is exactly as it was after M2, plus a
report.

**Unlocks**: An evidence-based decision on whether M4 is justified and which
builtins it should target.

**Operator Actions Before Next Milestone** (the measure→decide boundary):
- **Review the survey report.** Decide whether to proceed to M4. **Gating
  criterion (proposed, confirm during M3's gameplan):** proceed iff ≥5 distinct
  dogfood/fixture assertions are blocked *solely* on a soundly-modelable axiom
  for a shared-module rule. Below that, the entailment gap is better closed
  per-assertion in ad-hoc gameplans, or the floor is simply accepted as terminal.
- **Decide the per-builtin scope for M4.** From the survey, select the specific
  rules whose axioms are (a) sound, (b) blocking real assertions. Explicitly
  exclude brittle-semantics rules (string case folding, locale, surrogate
  handling) per the `JS_STRING.pant` precedent.
- **Abort condition:** if the survey shows most gaps are encoding gaps (not
  missing axioms), do NOT start M4 — open a separate encoding-integration
  workstream instead.

---

### Milestone 4: targeted-sound-axioms

**Definition of Done**:
- For each shared-module rule the M3 survey selected, add EUF body axioms to its
  `js-stdlib` module — restricted to **structurally sound** axioms (array length
  non-negativity, Map-size / values-array-length cardinality, Set membership
  reflexivity, and similar), each with a soundness justification in the module
  comment. No string-semantics axioms.
- The dogfood/fixture assertions the survey flagged as "blocked solely on this
  axiom" now entail under `pant --check` (z3).
- The samples CI typecheck path still passes; no existing assertion's entailment
  regresses (an axiom that over-constrains would make some currently-sound
  assertion unprovable or, worse, validate a false one — guard with the survey's
  before/after entailment set).

**Why this is a safe pause point**: Terminal milestone. Every selected builtin's
assertions entail; every axiom is justified sound. The `free-call-decl` story is
complete: structurally accepted → declared → (where it matters) provable.

**Unlocks**: Closes the workstream. The remaining entailment gaps are, by M3's
verdict, either encoding-integration work (separate workstream) or fundamentally
out of scope (brittle semantics).

**Established Precedents** (milestone-scoped):
- **algorithm — McCarthy theory of arrays + finite-set cardinality** —
  https://doi.org/10.1145/321033.321034 — the sound axiom shapes for the
  structural builtins; expressible in the SMT-LIB theories Pant already lowers to.

**Open Questions**:
- Whether any selected axiom needs a Pant grammar/encoding feature that does not
  yet exist (e.g. relating an `Array.from(map.values())` result to the Map's
  cardinality may need the Map encoding to expose a size rule). If so, that
  dependency is surfaced here and may push the specific builtin to a follow-on.

## Dependency Graph

```text
1 (free-call-opaque-floor)    → []
2 (stdlib-dispatch-coverage)  → [1]
3 (entailment-survey)         → [2]
4 (targeted-sound-axioms)     → [3]
```

Strictly sequential. M2 upgrades the floor M1 establishes (so M1's local
synthesis is the backstop for anything M2's table doesn't cover). M3 must measure
the post-M2 state (shared-module rules are where axiom gaps get attributed). M4 is
gated on M3's decision and per-builtin scope — the measure→decide boundary that
makes this a workstream rather than one gameplan.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Dispatch precedence (table vs local synthesis) | M1 synthesizes locally; M2 adds table entries that must take precedence so M2 is a pure upgrade with no double-declaration. Encode table-first ordering in M1. | M1's gameplan |
| any/unknown fallback coupling | M1's typed synthesis falls back to a conservative local Opaque sort for any/unknown-typed positions. This must be self-contained (no dependency on the drafted `ts2pant-any-unknown-opaque` workstream). If that workstream later lands, the fallback is superseded — coordinate then, not now. | M1's gameplan |
| M3→M4 gating threshold | Proposed: ≥5 distinct assertions blocked solely on a soundly-modelable axiom. Confirm the exact number during M3's gameplan once the survey shape is concrete. | M3's gameplan |
| Encoding-integration vs axiom | Some Map/Array gaps may be closable by connecting to ts2pant's existing Map encoding rather than adding an axiom. The survey (M3) classifies; genuine encoding work is explicitly a separate workstream, not M4. | M3 review |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Typed-from-TS-signature synthesis for user free functions (not opaque-only) | User-resolved (write-workstream dialogue 2026-05-26). A typed rule head (`unknown-fn x: Int => Int`) gives real entailment leverage; uniformly-opaque heads type-check but prove nothing. The TS signature is already available via `getResolvedSignature`, and `translate-signature.ts` already maps types to sorts — the fidelity is cheap to capture. Opaque fallback is reserved for genuine any/unknown positions. |
| Shared js-stdlib modules are a separate milestone (M2), not folded into the floor | User-resolved (write-workstream dialogue 2026-05-26). The shared modules are the natural, established home for M4's axioms (cross-document rule identity); folding stdlib into per-document local synthesis would force M4 to synthesize axioms per-document and diverge from the JS_MATH/JS_STRING architecture. |
| Floor is pure EUF; axioms are gated behind a measurement milestone | The entailment-vs-type-checking distinction is the whole reason this is a workstream. Landing axioms speculatively risks both wasted effort (axioms no assertion needs) and unsoundness (axioms that validate false assertions). M3 forces an evidence-based decision. |
| M4 axioms restricted to structural soundness; no string-semantics axioms | Inherits the `JS_STRING.pant` design decision: JS string semantics (locale, case folding, surrogate pairs) are too brittle to axiomatize soundly. M4 targets array/map/set structural facts only. |
| `var` and closure-captured reassignment remain out of scope | These are the Local-Binding workstream's explicit carve-outs, unrelated to free-call-decl. Their allowlist entries (`var-rejected`, `closure-captured-reassignment`) stay. |
