# Workstream: ts2pant Local-Binding SSA

## Vision

Route every TypeScript local binding (`const` and pure `let`, then mutable
`let`) through ts2pant's existing scalar-SSA pipeline instead of the
inline-and-substitute path that the prelude scanner uses today. The current
path forces every prelude statement to be a pure expression that can be
duplicated at every use site, which compels a conservative "is this call
pure?" allowlist (`isKnownPureCall` in `tools/ts2pant/src/purity.ts`) that
rejects most idiomatic TypeScript — regex-argument `String.prototype.replace`,
`Array.from(...)`, `Map.prototype.{entries,keys,values}`, never-reassigned
`let` bindings, and anything else not pre-listed. With SSA-routed bindings,
each binding becomes one versioned location referenced (not re-evaluated) at
every use site, so the question "is this RHS pure?" is irrelevant for safety;
the only remaining question is whether the RHS has a Pant translation, which
fails downstream cleanly as `free-call-decl` rather than upstream as a
prelude rejection. The end state expands the dogfood corpus from ~11 hand-
picked one-liner functions to substantially more, and unblocks idiomatic
TypeScript helper code (regex-replace chains, intermediate const-bound
computations, never-reassigned `let`) for any caller of the translator.

## Current State

The IR1 SSA workstream (`workstreams/ts2pant-ir1-ssa.md`, M1–M7) and the
General-Loop SSA workstream (`workstreams/ts2pant-general-loop-ssa.md`,
M1–M7) are both complete. IR1 SSA is the production mutating-body boundary
for scalar property mutation, Map/Set semantics, foreach Shape A/B, counter
/ bounded-while / fixed-point loops, and break/continue/return/throw early
exits. The split-form rule-definition recognition (PR #254 on branch
`split-form-rule-definitions`) just routed recursive Pant rule bodies
through SSA-aware `define-fun-rec` emission without growing the grammar.

Local bindings, however, do *not* go through any of this. The function-body
entry point `extractReturnExpression`
(`tools/ts2pant/src/translate-body.ts:1716`) collects `ConstBinding[]`
entries from a function's prelude, gates each by
`expressionHasSideEffects(decl.initializer, checker)`
(`translate-body.ts:2420`) which itself delegates to `isKnownPureCall`
(`tools/ts2pant/src/purity.ts:415`), and then `inlineConstBindings`
(`translate-body.ts:1237`) inline-substitutes each binding into the return
expression via `substituteIR1ExprSubtree`
(`tools/ts2pant/src/ir1-substitute.ts`). This is duplication, so it must be
pure — hence the allowlist. The IR1 AST has an `IR1Let` node
(`tools/ts2pant/src/ir1.ts`) for representing locally-bound names, but the
IR1 builder *never constructs one* — local bindings are inlined out of
existence before they reach the IR1 layer. `let` and `var` bindings are
rejected outright at `translate-body.ts:1834-1835` with
`"let/var bindings not supported"`, regardless of whether they are ever
reassigned.

The dogfood corpus (`tools/ts2pant/tests/integration/dogfood.test.mts`)
carries 11 annotated functions — every one is a single-statement
`return <expr>;` shape. A probe in this conversation against five candidate
helpers (`manglePantTypeToFragment`, `depModuleNameForFile`,
`cellTupleShapes`, `cellRegisterName`, `parseAnnotations`) found that four
of the five hit one of the prelude rejections (`"const binding with
side-effectful initializer"` / `"local bindings or multiple statements
before return"` / `"local variable declaration (let/var or effectful
const)"`). None of those calls is in fact impure; they are simply outside
the conservative allowlist that the inlining path requires.

## Key Challenges

- **IR1Let is a ghost node.** The type exists in `ir1.ts` but no builder
  constructs it; no SSA pass versions it; no emitter renders it. M1 must
  bring it to life across translate-body → ir1-build-body → ir1-ssa-scalars
  → emit in one cohesive change. Each layer has decisions to make:
  - Does the IR1 builder emit one `IR1Let` per `const` declaration, or a
    block-level `IR1Let` per `VariableStatement`?
  - Does scalar SSA treat a local binding as a new SSA location class, or
    extend the existing property-location machinery?
  - Does emission produce a body-chapter equation (`x = <expr>.`) per
    binding, a let-style preamble inside the chapter, or inline the RHS
    only when no aliasing is possible? The recently-landed split-form
    recursive-rule pass already taught the Collect/SMT layers to recognise
    chapter-body equations as definitions of declared rules — local
    bindings are structurally the same shape.

- **Inlining is still load-bearing for snapshot continuity.** Existing
  fixtures emit inlined output today (`fieldRuleName interface-name
  field-name = to-pant-term-name interface-name + "--" + to-pant-term-name
  field-name.`); under SSA-routed handling, every existing const-binding
  emission would gain an intermediate let-derived equation. This is
  semantically equivalent but breaks every snapshot. M1 has to decide:
  preserve inlining for single-use bindings (an optimisation pass over the
  SSA output) or accept widespread snapshot churn.

- **Local-variable mutation is real new SSA work.** The existing scalar SSA
  in `tools/ts2pant/src/ir1-ssa-scalars.ts` versions property locations
  (`Account_balance@a` → v1, v2, v3). Local variables aren't property
  locations — there's no receiver, no primed counterpart, no framing
  obligation. M2 must extend the scalar SSA scheme to a new "local
  variable" location class, or unify the two under a shared
  scalar-location vocabulary.

- **Downstream emittability remains the long pole.** Even after SSA-routed
  bindings unblock the prelude scanner, calls like
  `pantType.replace(/\s+/gu, "")` have no Pant equivalent — `replace`,
  `Array.from`, `Map.values()` etc. emit as `> UNSUPPORTED: free-call-decl`
  at the emitter. SSA fixes the failure *surface* (correct error, no false
  positives) but the user-visible coverage gain requires the separate
  Pant-stdlib / opaque-uninterpreted-function work. This workstream
  explicitly leaves that as a follow-on; the operator action between this
  workstream and any free-call-decl follow-up is "audit the resulting
  `known-typecheck-failures.mts` allowlist to decide which builtins
  warrant Pant-side declarations".

- **Conservative classification of "immutable let".** A `let` binding is
  immutable iff it is never the LHS of an assignment expression, never the
  operand of `++`/`--`, never destructured into a reassigning pattern, and
  never captured by a closure that itself reassigns it. The recogniser must
  walk the function body and *under-accept* when ambiguity is possible —
  the worst outcome of over-conservatism is the existing `let/var
  bindings not supported` rejection; the worst outcome of incorrectly
  classifying a mutable let as immutable is silently emitting incorrect
  Pant.

## Established Precedents

- **algorithm — Cytron et al. 1991 SSA construction** —
  https://doi.org/10.1145/115372.115320
  The canonical SSA algorithm. Every milestone of the prior IR1 SSA and
  general-loop SSA workstreams inherits its phi-placement and back-patching
  protocol; this workstream extends scalar SSA to a new location class
  (local bindings) using the same two-pass dominance-frontier construction.
  The scalar SSA builder in `tools/ts2pant/src/ir1-ssa-scalars.ts` already
  follows the Cytron shape for property locations.

- **paper — Knobe & Sarkar 1998 — Array SSA Form** —
  https://doi.org/10.1145/268946.268956
  Established precedent for routing *non-syntactic-variable* locations
  through SSA — Knobe & Sarkar treat each array element as its own location
  with per-write versioning. The IR1 SSA scalar pipeline already cites this
  precedent for foreach-element writes (per-element distinct SSA versions).
  Local variables under this workstream are a simpler case of the same
  technique: each binding is a degenerate "location" with one preheader
  version and one or more reassignment versions.

- **paper — Chow et al. 1996 — Effective Representation of Aliases and
  Indirect Memory Operations in SSA Form** —
  https://doi.org/10.1007/3-540-61053-7_72
  The "memory SSA" precedent already cited by `tools/ts2pant/AGENTS.md` for
  scalar property versioning. M1 and M2 extend the same scheme to local
  variables; the boundary between "this binding is a property write" and
  "this binding is a local-variable write" disappears under the unified
  Memory SSA framing — both produce indexed version chains that emit as
  body-chapter equations.

- **pattern — Capture-avoiding substitution via Bindlib (already cited by
  the IR1 substitution workstream)** —
  https://lepigre.fr/ocaml-bindlib/
  The existing `substituteIR1ExprSubtree` primitive in
  `tools/ts2pant/src/ir1-substitute.ts` (which today drives the inlining
  path) is built on this discipline. M1 keeps the substitution primitive
  in place for the SSA-output optimisation pass that inlines single-use
  bindings to preserve snapshot continuity.

## Milestones

### Milestone 1: const-binding-ssa

**Definition of Done**:

- `extractReturnExpression` (`tools/ts2pant/src/translate-body.ts:1716`)
  collects `const` bindings without consulting `expressionHasSideEffects`
  for safety reasons — the purity gate remains *only* as a hint for which
  bindings can be safely inlined for snapshot continuity (see below), not
  as a precondition for acceptance.
- IR1 build (`tools/ts2pant/src/ir1-build-body.ts`) emits `IR1Let` nodes
  for each `const` binding instead of pre-substituting them into the
  return expression.
- The scalar SSA pass (`tools/ts2pant/src/ir1-ssa-scalars.ts`) versions
  `IR1Let` bindings as a new scalar location class — one preheader
  version per binding, no loop-back, no primed counterpart.
- Pant emission renders each versioned binding as a chapter-body equation
  `<binding-name> = <RHS>.` and references to the binding resolve to its
  current SSA version. The recently-landed split-form recursive-rule
  recognition (`Collect.recognize_split_form_bodies` in `lib/collect.ml`)
  already accepts this shape; this milestone reuses it.
- A snapshot-continuity optimisation pass over the SSA output detects
  *single-use* bindings whose RHS passes `expressionHasSideEffects` and
  inlines them via `substituteIR1ExprSubtree` before emit. This preserves
  the existing snapshot output for the dogfood corpus and minimises
  reviewer churn on this milestone's PR.
- The existing `expressions-const-bindings.ts`,
  `expressions-const-pure-calls.ts`, and `functions-mutating-const.ts`
  fixture snapshots are byte-identical (gated by snapshot tests).
- A new fixture corpus exercises shapes that previously rejected:
  multi-use pure bindings, regex-argument String methods, `Array.from`,
  `Map.{entries,keys,values}`, multi-statement bodies with intermediate
  computations. Each new fixture function either emits cleanly or
  surfaces `> UNSUPPORTED: free-call-decl` at the emitter (gated by
  `tests/known-typecheck-failures.mts` allowlist entries for the
  free-call-decl cases).
- Three to five new functions in `tools/ts2pant/src/translate-types.ts`
  carry `@pant` annotations and dogfood test entries — at minimum
  `manglePantTypeToFragment` and `depModuleNameForFile`, both of which
  rejected on the probe in this conversation.
- The `let/var bindings not supported` rejection remains in place for
  this milestone; only `const` flows through the new path.

**Why this is a safe pause point**:

After M1 lands, the translator has two production paths for local
bindings — the legacy inline-substitute path (still active for single-use
pure bindings as an optimisation) and the new SSA-routed path (for
multi-use bindings and bindings whose initializer is now accepted under
SSA). Both produce checkable Pant; the snapshot corpus is unchanged for
existing fixtures; the dogfood corpus has grown. `let` and `var`
bindings still reject, but no fixture's behavior has degraded. If the
team chooses to stop here permanently, the codebase carries one extra
SSA emission path (low maintenance burden) and a meaningfully larger
dogfood corpus.

**Unlocks**:

M2 (`let-mutation-ssa`) — once `const` flows through SSA, extending the
scheme to versioned `let` reassignment is incremental rather than
foundational. M3 (`free-call-decl-synthesis`, in a separate workstream)
becomes the obvious next user-visible coverage gain.

**Operator Actions Before Next Milestone**:

- Run the full `npm run test:unit` and `npm run test:integration` suites
  on the M1 PR. Confirm zero regressions in the constructs snapshot
  corpus.
- Audit `tests/known-typecheck-failures.mts` for the newly-allowlisted
  `free-call-decl` entries M1 added. Each is a Pant-stdlib gap the
  separate free-call-decl workstream will eventually close; decide
  whether any are urgent enough to fold into an interim milestone of
  *that* workstream, or whether to wait until it lands holistically.
- Walk the dogfood corpus expansion: confirm each newly-annotated
  function's `@pant` annotations entail under `pant --check` (z3).
- Decide whether M2's scope should extend the immutable-let recognition
  (never-reassigned `let` → treated as `const`) before the full mutable-
  let SSA, or whether the two should land together.

**Open Questions** (resolved during `write-gameplan` for this milestone):

- Single-use inlining as an *optimisation* pass on SSA output, or as a
  *fallback* path that the SSA pipeline skips entirely for single-use
  bindings? The optimisation pass is more uniform (one SSA path with a
  post-pass) but requires snapshot-continuity infrastructure. The
  fallback path is more disruptive (two production paths) but
  zero-snapshot-churn.
- IR1Let emission granularity: one `IR1Let` per declaration, one per
  `VariableStatement` (which may contain multiple declarations), or
  collapse adjacent `IR1Let` nodes into a block-level `IR1LetBlock`?
- Scalar SSA location class: extend the existing
  `IR1SsaPropertyLocation` to admit a "local binding" kind, or introduce
  a parallel `IR1SsaLocalBindingLocation`? The first is simpler; the
  second keeps property writes and local bindings linguistically
  distinct.

---

### Milestone 2: let-mutation-ssa

**Definition of Done**:

- The `let/var bindings not supported` rejection at
  `translate-body.ts:1834-1835` is replaced by per-binding classification:
  - `let` with no reassignment in the function body → treated identically
    to `const` (M1 path).
  - `let` with reassignment → routes through scalar SSA with one version
    per assignment.
  - `var` — still rejected (`var` hoisting and function-scope semantics
    make safe handling materially harder; punt to a follow-up workstream
    if real-world demand surfaces).
- The scalar SSA pipeline versions each reassignment of a `let` binding
  as a new SSA version, parallel to how it versions property writes
  today. The emitted Pant carries one equation per version
  (`x = <init>.`, then `x' = <reassigned-value>.` if the reassignment is
  guarded, or chained equations).
- The recogniser for "is this `let` ever reassigned?" walks the function
  body looking for assignment expressions, `++`/`--` operators on the
  binding, and destructuring patterns that reassign. It under-accepts
  conservatively: any ambiguous case falls into the "treat as mutable
  let" branch, which is still supported (just heavier in emission).
- New fixtures cover: `let` with one reassignment, `let` with several
  reassignments in straight-line code, `let` with reassignment inside a
  branch, `let` with reassignment inside a loop (this last case routes
  the binding through the existing general-loop SSA machinery as a
  newly-recognised mutated location).
- Existing fixtures' snapshots remain byte-identical (no change to
  `const`-only or property-mutation behavior).

**Why this is a safe pause point**:

After M2, every TypeScript local-binding shape that the translator can
sensibly produce Pant for is accepted. The translator no longer
gratuitously rejects `let` — rejection now traces to a real semantic
limitation (downstream emittability, addressed by the follow-up
free-call-decl workstream). The dogfood corpus has grown further.

**Unlocks**:

A meaningful expansion of the application-code TypeScript that ts2pant
can translate end-to-end, without further translator-side work. The next
coverage gain is downstream: synthesising Pant-side declarations for
opaque builtins, addressed by a separate workstream.

**Operator Actions Before Next Milestone**:

This is the terminal milestone of this workstream. After M2, the team
must decide:

- Whether to start the **free-call-decl synthesis workstream** (opaque
  uninterpreted-function declarations for builtins like `string-replace`,
  `array-from`, `map-entries`, etc.). The criterion is concrete: how
  many `known-typecheck-failures.mts` entries carry the `free-call-decl`
  tag after M2 lands? If ≥10, the workstream is justified by direct
  coverage gain; below that, the gap may be addressable per-call in
  ad-hoc gameplans.
- Whether to start a **`var` handling workstream** if real fixtures
  surface that need it. This is unlikely — `var` is rarely used in
  modern TypeScript — but the carve-out is explicit so the team
  knows where the boundary is.

**Established Precedents** (milestone-scoped):

- **algorithm — Cytron 1991 SSA construction (reassignment case)** —
  https://doi.org/10.1145/115372.115320
  The reassignment-as-new-version mechanic is the canonical Cytron pattern
  the existing property-location scalar SSA already follows. This
  milestone applies the same mechanic to local-variable assignments —
  no novel algorithm, just a new location class consuming the same
  protocol.

**Open Questions** (resolved during `write-gameplan` for this milestone):

- Should the immutable-let detection (never-reassigned `let` → treated
  as `const`) land in M2's first patch as a stepping stone before the
  full mutable-let SSA, or should both be folded into a single set of
  patches? The former gives a smaller incremental scope; the latter
  avoids two passes over the recogniser.
- Per-loop `let` mutation: when a `let` binding is reassigned *inside*
  a loop body, does it route through the general-loop SSA as a newly-
  recognised mutated location (parallel to how property writes are
  handled), or does the loop-summary machinery reject and surface a
  diagnostic? The first option is the more general handling; the
  second is the safer initial scope.

## Dependency Graph

```text
1 (const-binding-ssa)    → []
2 (let-mutation-ssa)     → [1]
```

Strictly sequential. M2 builds on M1's scalar-SSA local-binding
infrastructure.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Is the snapshot-continuity optimisation pass (single-use inlining of pure bindings) worth the complexity, or should we accept widespread snapshot churn in M1 and let reviewers re-baseline? | The optimisation pass keeps existing snapshots stable, which makes the M1 PR substantially easier to review (every diff is intentional new behavior). The downside is ~200 lines of post-SSA optimisation logic that could be deferred. | Resolved during M1's gameplan |
| Is the free-call-decl synthesis (Pant-stdlib for builtins) the right name and scope for the follow-on workstream, or should it be subdivided further (per-builtin module, per-call-shape)? | The synthesis problem touches every accepted-but-unmodeled call uniformly; subdividing per-builtin would fragment the design. But the scope grows large if it commits to a full Pant-stdlib module. | Resolved by the follow-on workstream's own discovery |
| Should the dogfood expansion in M1 also cover functions in modules other than `translate-types.ts` — e.g. the `purity.ts` and `annotations.ts` helpers, even if they require partial-translation accommodations? | Expanding outside `translate-types.ts` raises the question of whether translate-time partial-translation is a feature ts2pant should support. | Resolved during M1's gameplan |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| SSA-routed local bindings instead of expanding the purity allowlist | The allowlist is the wrong abstraction: it conflates "is this RHS pure?" with "can this RHS be safely inlined?". With SSA, the inlining safety question disappears — each binding is referenced (not re-evaluated) at use sites, so any RHS that the TS type-checker accepts is admissible regardless of purity. The allowlist's narrow scope (a handful of methods per type) is exactly the wrong place to expand; the right place is the IR layer that already knows how to handle versioned writes. |
| Two milestones (`const`, then mutable-`let`), not one | M1's scope is materially larger than M2's even though M2 sounds "harder" — M1 lights up an entire ghost AST node (`IR1Let`) across four code layers; M2 extends an already-functional layer to a new assignment shape. Bundling would over-commit the M1 PR; separating gives a clean safe pause point after M1. |
| `var` is out of scope, indefinitely | `var` is rarely used in modern TypeScript and its hoisting + function-scope semantics complicate SSA construction non-trivially. The carve-out is explicit so future maintainers don't expect `var` to fall out for free; it doesn't, and a follow-up workstream would be needed if real-world demand surfaces. |
| Free-call-decl synthesis is OUT OF SCOPE | Closing the free-call-decl gap (synthesising opaque Pant rule declarations for accepted-but-unmodeled builtins like `string-replace`) is a distinct concern that touches Pant-side stdlib design, not TypeScript translation. Bundling would dilute focus; separating lets each workstream succeed on its own terms. M2's operator instructions explicitly call out the criterion for starting that follow-on. |
| Keep the existing inline-substitute path active as a snapshot-continuity optimisation | Removing inlining entirely would change emission for every existing fixture (every const binding gains an intermediate equation). The functional change is null but the snapshot churn would be enormous. Routing single-use pure bindings through inlining as a post-SSA optimisation preserves the existing corpus byte-for-byte while admitting the new shapes through the SSA path. |
| Cite Memory SSA (Chow 1996) and Array SSA (Knobe & Sarkar 1998) as the precedent backbone | Both papers are already cited in the prior IR1 SSA workstream and `tools/ts2pant/AGENTS.md`. The unified Memory-SSA framing — "every location-with-versioned-writes is the same protocol" — is exactly the right abstraction for treating local bindings as just another location class, and the IR1 SSA scalar machinery already follows it for property writes. |
