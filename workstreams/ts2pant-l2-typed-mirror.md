# Workstream: ts2pant L2 Typed Mirror

## Vision

Reframe L2 (`IRExpr` in `tools/ts2pant/src/ir.ts`) as a **complete typed
mirror of OpaqueExpr** — every wasm `ast.*` constructor has a corresponding
L2 form — while ensuring **L1→L2 carries zero recognition logic**. Every L1
expression form maps to exactly one L2 form mechanically. All recognition,
normalisation, and target-shape decisions live upstream at the TS-AST → L1
boundary. The end state: L2 stays as a typed staging area for type-safe
construction and snapshot-testable inspection, but stops being mistaken for
a transformation IR.

## Current State

L2 has ten forms. Most are 1-to-1 with L1 expression forms (Var, Lit, App,
Cond, Each), but four are L2-only and carry the Pant-target shapes:
`CombTyped`, `Forall`, `Exists`, `Comb` (and arguably `Let` as an expression-
position form). Today the μ-search recognizer fires at the L1→L2 boundary:
`isCanonicalMuSearchForm` validates at L1, `lowerL1MuSearch` rewrites to L2
`CombTyped` (`tools/ts2pant/src/ir1-lower.ts`). This is the one substantive
transformation at L1→L2; everything else is structural.

The "layering principle" in `tools/ts2pant/AGENTS.md` (and the IR doc) claims
**"L1 — canonicalized TypeScript syntax. No Pantagruel-target awareness."**
This is already weakened in practice: L1 has `IsNullish`, which is a
Pant-target normalisation of TS nullish forms (M4 P5). The principle is
aspirational rather than enforced. Recognizers live in both layers — some at
TS-AST → L1 (functor-lift, nullish, increment), one at L1→L2 (μ-search).

L2's actual usage outside `ir-emit.ts` is minimal: a small amount of debug
printing (`ir1-printer.ts`) and the L1→L2 lowerer. No analyser walks L2.
No optimiser rewrites L2. No verifier reads L2. Nothing knocks on the door
the layering principle leaves open.

The accumulated effect: L2 reads to a future contributor as "a real IR with
transformation passes" when in practice it's "a typed builder for
OpaqueExpr." That confusion is the cost we're paying for the aspirational
framing.

## Key Challenges

- **L1 vocabulary expansion**. The four L2-only forms (`CombTyped`,
  `Forall`, `Exists`, `Comb`) must gain L1 counterparts so the upstream
  recognizers can produce them directly. Each new L1 form needs a
  constructor, structural equality (already covered by `ir1SsaExprEquals`'s
  recursive walker but the new forms need arms), the IR1 substitution
  primitive's binder-site table extended, and a 1:1 mechanical lowering
  arm in `ir1-lower.ts`.

- **Recognizer migration discipline**. Today `lowerL1MuSearch` is at L1→L2;
  the migration moves the recognition to TS-AST → L1 (extending
  `recognizeLetWhilePair` or its equivalent to produce L1 `CombTyped`
  directly). The change is small but the discipline is load-bearing:
  future recognizers must not regress to L1→L2 placement, because the
  whole point of the refactor is to make L1→L2 a pure mechanical pass.

- **Snapshot equivalence on every fixture**. The migration must produce
  byte-identical Pant output on every existing fixture that exercises
  μ-search, functor-lift, nullish, etc. (the recognizers that touch the
  affected forms). Any drift is a stop-the-line condition.

- **Layering principle's honest restatement**. The AGENTS.md / IR doc
  framing currently asserts L1 has "no Pantagruel-target awareness." After
  this workstream, the framing must instead state: "L1 is the canonical
  form after all source-level recognition; L2 is L1's expression subset
  packaged as a typed OpaqueExpr mirror." The honest statement is more
  useful than the aspirational one but requires careful prose.

- **Dependency on the IR1 substitution workstream**. The L1 vocabulary
  expansion adds three or four new binder sites (`comb-typed` binder,
  `forall` binder, `exists` binder, `comb`'s inner `each` binder). The
  IR1 substitution primitive's binder-site enumeration must absorb these.
  See "Decisions Made" for the sequencing rationale.

## Established Precedents

- **paper — IRSC (Vekris, Cosman, Jhala — PLDI 2016)** —
  https://doi.org/10.1145/2908080.2908110
  Already cited as ts2pant's foundational IR reference. The original IRSC
  separation between FRSC (source-shape, named, with assignment) and IRSC
  (target-shape, SSA, pure) serves a verification-soundness purpose
  because IRSC has a downstream consumer (the refinement-type checker).
  ts2pant has no L2-reading consumer beyond `ir-emit.ts`, so the IRSC
  argument does not transfer at full strength. This workstream
  acknowledges the divergence rather than maintaining it as fiction:
  L2 stays a typed mirror, not a "real" pure IR; the verification
  benefit IRSC achieves through the separation is achieved here by
  L1's canonicalisation discipline.

- **pattern — Recognizer-at-source / canonical-IR pattern**
  The principle: every IR layer below the source-AST layer is structural,
  with all recognition firing at the source-AST→canonical-IR boundary.
  Widely used in compiler design (e.g., Clang's AST → ClangIR lowering
  carries no recognizers; all decisions are in the Sema phase). Closest
  contemporary analogue in spec-language tooling: F\*'s extraction
  pipeline. ts2pant's M3 architectural decision to bypass L2 for
  mutating-body lowering is the prior precedent within ts2pant —
  recognizers and SSA construction concentrated at L1 build, with the
  output flowing directly to PropResult[]. This workstream extends that
  discipline to the pure / expression path.

- **pattern — Typed shadow / mirror IR (smart constructor layer)**
  Maintaining a typed IR that nominally mirrors an opaque or external
  target representation, used purely for type safety and test
  affordance, not for transformation. Common in language frontends that
  emit to LLVM IR (the frontend's typed IR before lowering) and in
  TypeScript / Flow code-mod tooling (typed wrappers over Babel AST).
  ts2pant's post-refactor L2 is exactly this pattern: a typed mirror
  of OpaqueExpr with no transformation responsibility.

- **architectural decision — M3 mutating-body bypass of L2**
  `tools/ts2pant/docs/intermediate-representation.md` § "Locked
  decisions": "Mutating-body lowering bypasses L2 (post-M3 architectural
  commitment). The L2 statement vocabulary that the original Stages
  9–11 plan called for turned out to be a compatibility shim mirroring
  symbolicExecute line-for-line." The prior intra-project demonstration
  that L2 didn't earn its keep for statement-position lowering. This
  workstream applies the same honest reading to the expression-position
  L1→L2 path: most of L2 is structural shadow, not transformation.

## Milestones

### Milestone 1: l2-typed-mirror

**Definition of Done**:

Three coordinated changes ship together as one atomic gameplan:

*(1) L1 expression vocabulary expansion.*
`tools/ts2pant/src/ir1.ts` exports three new IR1 expression forms that
today exist only in L2:

- `IR1Expr.kind: "comb-typed"` — `{ combiner, binder, binderType, guards, proj }`. Mirrors L2 `CombTyped`.
- `IR1Expr.kind: "forall"` — `{ binder, binderType, guard?, body }`. Mirrors L2 `Forall`.
- `IR1Expr.kind: "exists"` — `{ binder, binderType, guard?, body }`. Mirrors L2 `Exists`.

Each new form has:
- A constructor helper (`ir1CombTyped`, `ir1Forall`, `ir1Exists`) in
  `ir1.ts`.
- Structural-equality arm in `ir1SsaExprEquals` and every other
  IR1Expr-exhaustive walker (`ir1-printer.ts`, `ir1-ssa-counter-loop.ts`,
  `ir1-ssa-fixed-point.ts`).
- Free-vars / substitution scope handling in the IR1 substitution
  primitive (`tools/ts2pant/src/ir1-substitute.ts`, from
  `workstreams/ts2pant-ir1-substitution.md`). The binder is registered
  as introducing scope; capture-risk and shadow-needle checks apply
  uniformly.
- A mechanical 1:1 lowering arm in `tools/ts2pant/src/ir1-lower.ts`
  (`lowerL1Expr`): L1 `comb-typed` → L2 `CombTyped` via `irCombTyped`,
  etc.

`Comb` (L2's chain-fusion output) is *not* mirrored at L1: no
recognizer in this workstream's scope produces it upstream; it stays
L2-only as a chain-fusion artefact.

*(2) μ-search recognizer migration.*
The μ-search recognizer produces L1 `comb-typed` directly instead of
being applied at the L1→L2 boundary:

- The TS-AST → L1 build pass (`recognizeLetWhilePair` and/or its
  callers in `translate-body.ts` / `ir1-build.ts`) detects the
  let-while pair and builds L1 `comb-typed` for valid μ-search shapes.
- `isCanonicalMuSearchForm` and `lowerL1MuSearch` in
  `tools/ts2pant/src/ir1-lower.ts` are simplified or removed — the
  L1→L2 lowering for `kind: "comb-typed"` is now a pure 1:1 mechanical
  mapping via the lowering arm from (1).
- All existing μ-search fixtures (`expressions-while-mu-search.ts` and
  any test exercising `isCanonicalMuSearchForm`) produce byte-identical
  Pant output. The `tools/ts2pant/tests/constructs.test.mts.snapshot`
  diff is empty; any drift aborts the gameplan.

Other Pant-target normalisations that fire at L1→L2 (e.g., `is-nullish`
→ cardinality) are *not* migrated in this workstream. The principle is
established by migrating μ-search; the rest follow as separate
workstreams if they earn the work.

*(3) Layering-principle restatement.*
`tools/ts2pant/AGENTS.md` and
`tools/ts2pant/docs/intermediate-representation.md` are reframed:

- The IR doc's "Layering principle" subsection is rewritten. The
  previous "L1 — canonicalized TypeScript syntax. No Pantagruel-target
  awareness" framing is replaced with the honest statement: "L1 is the
  canonical form after all source-level recognition; L2 is L1's
  expression subset packaged as a typed OpaqueExpr mirror. All
  recognition and target-shape decisions happen at TS-AST → L1; L1→L2
  is a mechanical 1:1 lowering."
- The IR doc's `## IRExpr — 10 forms` table gains an explanatory
  paragraph clarifying L2's purpose: type-safe construction of
  OpaqueExpr, snapshot-testable inspection, no transformation
  responsibility.
- AGENTS.md's Foundational Pattern / IRSC references are updated to
  note ts2pant's divergence from the original IRSC purpose (we have no
  L2-reading verifier; the separation is for type safety and
  inspectability only).
- A discipline statement lands in the IR doc: "L1→L2 carries no
  recognition decisions. New recognizers must fire at TS-AST → L1
  and produce canonical L1 forms. Review will reject any L1→L2
  transformation that is not a 1:1 mechanical mapping or a structural
  rewrite of an already-canonical L1 form."
- A corollary statement lands alongside: "L2 is not a substitution
  target. Any transformation that wants to rewrite an L2 expression
  is asking the wrong question — either build the canonical L1 form
  upstream and substitute there (using the IR1 substitution primitive
  from `workstreams/ts2pant-ir1-substitution.md`), or lower to
  OpaqueExpr and use `ast.substituteBinder`."
- `workstreams/ts2pant-imperative-ir.md` is updated where it asserts L1
  has "no Pant-target awareness" — corrected to align with the
  post-refactor framing.

**Why this is a safe pause point**:
This is the workstream's only milestone. Snapshot equivalence is the
load-bearing gate: every fixture exercising μ-search produces
byte-identical Pant output before and after the gameplan. `git revert`
of the gameplan restores the pre-refactor state. The discipline is
established by the absence of L1→L2 recognition machinery for
μ-search and by the documentation; future contributors find the
principle in the code state and the prose simultaneously.

**Operator Actions After Milestone Lands**:
- Run the full ts2pant integration suite including `pant --check`. The
  migration should produce identical SMT results to pre-refactor.
- Confirm the AGENTS.md / IR doc additions read clearly to a reviewer
  unfamiliar with the workstream's history. The discipline is only
  useful if future contributors discover it.
- Confirm any future workstream that adds an L1→L2 transformation
  attempt is rejected in review (the discipline doc is the appeal
  authority). The next likely test case is general-loop SSA L5
  (break/continue): its build pass must produce canonical L1 forms,
  not L1→L2 rewrites.

**Unlocks**:
The IR layering doesn't surface again as an ambient question in code
review. Future workstreams (general-loop SSA L5 / L6, any new
recognizer family) inherit the discipline without re-discovering it.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- The exact representation of type expressions in L1 forms. L2's
  `CombTyped.binderType` is a plain `string` today; L1 mirrors this
  directly without a structured `IR1TypeExpr` ADT.
- Whether `Comb` (L2) needs an L1 counterpart. Resolved: no — `Comb` is
  L2-only chain-fusion output and no recognizer in this workstream
  migrates chain fusion.
- Whether existing `recognizeLetWhilePair` extends to build L1
  `comb-typed` directly or a new helper is needed.
- How to handle μ-search recognition failure at the TS-AST → L1
  boundary (today's rejection from `isCanonicalMuSearchForm` must
  surface earlier with an actionable diagnostic).
- Whether the discipline statement is enforced by lint or docs + review.
  Recommendation: docs + review until a violation appears.

## Dependency Graph

- Milestone 1 -> []

Single milestone. The vocabulary expansion, recognizer migration, and
documentation form one atomic gameplan: no human pause, no observation
window, no outcome-conditional decision between them. The orchestrator
executes every patch back-to-back; the dependency graph lives inside
the single gameplan via `dependsOn` between patches.

This workstream **depends on `ts2pant-ir1-substitution`**: M1 of this
workstream adds binder sites (the `comb-typed` binder, `forall` binder,
`exists` binder, `comb`'s inner `each` binder) to the IR1 binder-site
enumeration. The IR1 substitution primitive must be the place that
enumeration lives. If `ts2pant-ir1-substitution` has not landed when
M1 starts, this workstream either (a) waits, or (b) absorbs the
enumeration-extension work scope-creep. Default: wait. Order matters.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| `Comb` (L2) → L1 counterpart needed? | `Comb` is produced by `.reduce` chain fusion at L1→L2. If chain fusion stays structural (not a target-aware recognition), `Comb` could remain L2-only. Decision affects M1's scope. | M1 write-gameplan |
| Other L1→L2 normalisations | `IsNullish` → cardinality `#x = 0` at L1→L2 is described as "mechanical lower" already. Audit whether any other L1→L2 transformations are recognitions in disguise (decisions hidden inside structural-looking code) and decide whether to migrate them. | M2 write-gameplan |
| `IR1TypeExpr` representation | The L2-only forms carry typed binders; L1 needs to carry the same type info. Whether to introduce an `IR1TypeExpr` shadow of `IRTypeExpr` or reuse opaque type handles is a design choice with downstream tooling implications. | M1 write-gameplan |
| Discipline enforcement | Lint rule vs documentation-only. Recommendation: docs + review until a violation appears. | M3 write-gameplan |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Keep L2 as a complete typed mirror; do not shrink it (Path A) and do not delete it (Path B) | The hybrid path keeps L2's main remaining value — typed construction of OpaqueExpr, TS-side snapshot/inspection. The honest framing is that L2 is a typed mirror, not a transformation IR; the refactor changes how L2 is used, not what L2 contains. Shrinking L2 would lose the type-safe builders for structural forms; deleting L2 would lose snapshot affordance and create wasm-API coupling in `ir1-lower.ts`. The hybrid pays a modest L1-vocabulary expansion cost for a cleaner principle. |
| L1 absorbs Pant-target forms (`CombTyped`, `Forall`, `Exists`, possibly `Comb`) | The "no Pant-target awareness at L1" framing is already broken (L1 has `IsNullish`). Acknowledging that L1 absorbs canonical Pant-target shapes is more honest than maintaining the fiction. The vocabulary expansion is bounded (3-4 forms) and the binder discipline is well-understood through the IR1 substitution workstream. |
| Single milestone (one gameplan), not three | An earlier draft of this workstream split the work into M1 (vocabulary), M2 (recognizer migration), M3 (docs restatement). Re-examined against the gameplan atomicity criteria, the splits could not be justified: no human intervention happens between vocabulary landing and recognizer migration; no observation window is needed (snapshot equivalence is the immediate gate within the migration patch); no decision is conditioned on prior milestone outcomes (the docs reframe is mechanically derivable from the code state). The "M1 stands alone if M2 reveals problems" framing was self-fulfilling — snapshot tests catch problems in the migration patch regardless of milestone boundary. Same pattern as the substitution workstream's collapse. One atomic gameplan with the migration patch dependent on the vocabulary patch is the correct structure. |
| Depend on the IR1 substitution workstream landing first | M1 adds binder sites; the substitution primitive's binder-site enumeration must absorb them. If the substitution workstream hasn't landed, M1 either waits or scope-creeps. Sequencing matters: substitution workstream → this workstream's M1 → M2 → M3. |
| Snapshot-equivalence gates M2 | Snapshot tests are byte-level. M2's behavior-preserving claim is gated by byte-identical Pant output on every affected fixture. This is the highest-precision regression signal available. |
| Defer additional L1→L2 normalisation migrations beyond μ-search | The principle is established by migrating one recognizer. Other normalisations (is-nullish → cardinality, if those are decisions in disguise) can follow as separate workstreams if a real need surfaces. Bundling them risks scope explosion and obscures the principle's articulation. |
| Documentation-only discipline (no lint) | The discipline statement in M3 is enforced by review, not lint. Lint rules for "no L1→L2 recognition" would require an AST-based check on `ir1-lower.ts`'s arms, which is over-investment until a violation has been attempted. If violations recur, lint can land in a follow-on. |
| L2 keeps all 10 forms (no shrinkage in scope) | This is the explicit divergence from Path A. The hybrid's value is that L2 stays a *complete* typed mirror — not a partial one that splits responsibility between L2 forms and direct OpaqueExpr construction. Either L2 mirrors OpaqueExpr fully, or it doesn't justify the layer. The hybrid commits to "fully." |
| L2 is not a substitution target (corollary of typed-mirror) | The typed-mirror principle says L2 carries no transformation. Substitution is a transformation. Therefore: no transformation that wants to rewrite an L2 expression should target L2. The two valid routes are (1) build the canonical L1 form upstream and substitute at L1 using the IR1 substitution primitive (`workstreams/ts2pant-ir1-substitution.md`), or (2) lower to OpaqueExpr and use `ast.substituteBinder`. This corollary sharpens the IR1 substitution workstream's "Scope: IR1 only" decision from "deferred until a real site demands it" to "structurally forbidden by the typed-mirror principle." Stated explicitly so substitution-disguised-as-construction doesn't sneak in. |
| No standalone design-doc milestone | The workstream itself is the design document. The doc-restatement work lives in M3 alongside the code state matching the principle. |
