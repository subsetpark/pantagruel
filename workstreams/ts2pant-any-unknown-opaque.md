# Workstream: ts2pant `any` / `unknown` handling via Opaque sort

## Vision

Give ts2pant a principled, two-layer mechanism for handling TypeScript `any` and `unknown` while Pantagruel stays monomorphic. The floor is a synthesized `Opaque` domain that carries unanalyzable values soundly; the ceiling is call-site monomorphization that recovers precision when the call graph is closed enough for whole-program specialization. The end state lets ts2pant accept real-world TS containing `any` / `unknown` (third-party APIs, JSON parsing, gradual code) and emit Pant that *verifies what it can* and *honestly opaques what it can't* — replacing today's all-or-nothing rejection.

## Current State

`tools/ts2pant/src/translate-types.ts:1102` rejects `unknown` via the `UNSUPPORTED_UNKNOWN` sentinel, which propagates through every composite type constructor (tuple, array, set, Map K/V, union, anonymous record) and poisons the entire signature. `any` is not detected explicitly — it falls through to `checker.typeToString()` and emits the literal string `"any"`, which is unparseable in Pant. The L2 typed-mirror workstream (landed 2026-05-21) re-established the layering discipline that makes `OpaqueExpr` the principled home for "value present, structure erased" — exactly the floor's representation.

## Key Challenges

- **Soundness of operation propagation.** Operations consuming an `Opaque`-sorted value must propagate opacity, or the floor leaks structure it doesn't have. Snapshot equivalence catches output drift but does not catch newly-unsound proofs.
- **TS narrowing semantics.** `checker.getTypeAtLocation` returns the narrowed type at a use site, but flow narrowing across function call boundaries is unreliable. Trusting it too aggressively introduces false precision; ignoring it loses the most common precision-recovery opportunity (post-`typeof` and post-discriminator-check uses).
- **Whole-program scope for monomorphization.** ts2pant translates one source file at a time (with dep modules for cross-file types). The monomorphization layer needs a richer program view — at minimum, all call sites of a function within the same module; ideally, across dep modules.
- **Dogfood as ground truth.** ts2pant translates parts of itself; the dogfood corpus contains real `any` / `unknown` uses. Any mistake here surfaces immediately as broken or wrong Pant output, which is both an asset (fast feedback) and a hazard (a regression blocks unrelated work).
- **Layer interaction.** Narrowing-based precision recovery and monomorphization-based precision recovery overlap: a function whose `unknown` parameter is narrowed at every use may not need specialization, and vice versa. Survey-before-implement (M5 → M6) is the mechanism for avoiding double-counting.

## Established Precedents

- **paper — Bobot, Filliâtre, Marché, Paskevich: "Encoding polymorphism with SMT logic"** (Why3, 2011)
  Why3's three-way taxonomy of polymorphism encodings into monomorphic SMT (monomorphization, type erasure with explicit tags, uninterpreted-sort encoding) is the closest existing prior art for the floor+ceiling design. Read it before committing the M6 specialization strategy — the soundness proofs constrain how the uniform-fallback (our `Opaque` floor) must interact with the specialized cases.

- **pattern — Monomorphization with uniform-encoding fallback** (Boogie, Dafny, Why3, Stainless)
  All four verifiers specialize polymorphic functions per call type where tractable and fall back to a uniform encoding otherwise. Constrains M5 / M6: specialization must be opt-out (callers can request the uniform encoding) and must not change the meaning of code that the uniform fallback already handles. The Dafny monomorphization pass is the closest engineering analog — bounded by recursion and size, falls back to uninterpreted sort.

- **pattern — Uninterpreted sort encoding for dynamic value spaces** (Rosette, Z3 community JS/Python encodings)
  Standard SMT pattern: model the universe of dynamic values as one uninterpreted sort with `is-int` / `intify` projection-injection function pairs. Our `Opaque` is the same shape *restricted to no projections* — we do not aim to support reflection or runtime type queries, so the floor declares the domain and stops. Cited for M1's domain shape.

- **paper — Siek & Taha: "Gradual Typing for Functional Languages"** (Scheme Workshop, 2006)
  The dynamic type `?` and its consistency relation give the conceptual framing for M4: TS's own narrowing acts as the runtime contract that crosses from the untyped (`any` / `unknown`) into the typed (concrete sort) region. We are not implementing gradual typing — we are reading the narrowing the TS compiler already does — but the soundness intuition (information flows *into* typed regions only at explicit boundaries) shapes which uses of `getTypeAtLocation` are trustworthy.

- **library — TypeScript compiler API** — https://github.com/microsoft/TypeScript
  Already a workstream-wide dependency. Specific surfaces this workstream relies on: `ts.TypeFlags.Any` (M1, currently undetected), `ts.TypeFlags.Unknown` (M1, today's only handled case), `checker.getTypeAtLocation(node)` for flow-narrowed types at use sites (M4), `checker.getSignaturesOfType` and call-graph walking (M5).

## Milestones

### Milestone 1: ts2pant-opaque-vocabulary

**Definition of Done**:
- `mapTsType` detects `ts.TypeFlags.Any` explicitly (currently silent) and routes it through the same path as `unknown`.
- A new synthesized domain `Opaque` is registered via the existing module-level synthesizer pattern (peer of `MapSynth`, `anonRecord`, `tupleShape`).
- L1 vocabulary gains one new form `OpaqueValue` carrying `{sort: "Opaque", origin: SourceRef}`; every IR1Expr-exhaustive walker (`ir1-substitute.ts`, `ir1-printer.ts`, `ir1-ssa-*`, `ir1-lower.ts`) gains a mechanical arm.
- L1→L2 mechanical lowering arm for `OpaqueValue` → L2 `OpaqueExpr`.
- No caller emits `OpaqueValue` yet. Snapshot-equivalent on every fixture in `tools/ts2pant/tests/`.

**Why this is a safe pause point**: Vocabulary is dormant. `any` detection routes to the existing `UNSUPPORTED_UNKNOWN` sentinel, so the only behavioral change is that files containing `any` now produce the same honest rejection that `unknown`-containing files already do — strictly better than today's silent broken emission. No fixture changes.

**Unlocks**: M2 (opt-in opaque policy can flow through to the new form), M4 (narrowing recovery has somewhere to land the recovered type alongside the opaque fallback).

**Open Questions** (resolve during write-gameplan):
- `OpaqueValue.origin: SourceRef` field shape — full `ts.Node` reference (heavy, but rich for diagnostics) vs. `{file: string, line: number}` (lightweight, sufficient for the user-facing skip reason)?
- `Opaque` domain emission policy — declare unconditionally per module (simple) vs. only when a use exists (matches `MapSynth` pattern)?
- Equality semantics on `Opaque` — what does Pant's default decidable equality give us, and is that what we want for two opaque values from different sources?

---

### Milestone 2: ts2pant-opaque-opt-in

**Definition of Done**:
- `mapTsType` grows a `policy: "reject" | "opaque"` parameter; default remains `"reject"` to preserve existing behavior.
- When `policy === "opaque"`, `any` and `unknown` lower to the `Opaque` sort and the containing expression produces an `OpaqueValue` (via M1's vocabulary) instead of poisoning the signature.
- Composite types (tuple, array, set, Map K/V, union, record) propagate opacity correctly: any sub-position that is `Opaque` makes the containing type either `Opaque` or `[Opaque]` / `Opaque * T` / etc. as appropriate.
- Operations consuming an `OpaqueValue` propagate opacity (any L1 expression with an opaque operand becomes opaque) unless the consumer's TS-side type at the use site is concrete (deferred to M4; for now, opacity is contagious).
- A test fixture set explicitly opts into `policy: "opaque"` and produces verifiable Pant. The existing fixture set, still on `"reject"`, is snapshot-equivalent.

**Why this is a safe pause point**: Opt-in. Nothing changes for code that doesn't request opaque mode. The new policy is exercised by dedicated fixtures; the rest of the corpus is untouched. Reverting the workstream here leaves only the dormant M1 vocabulary in place.

**Unlocks**: M3 (default flip is now mechanical), M4 (narrowing has a sound starting point — opacity is contagious until proven otherwise), dogfood evaluation of whether the opaque encoding meaningfully reduces UNSUPPORTED-skips.

**Operator Actions Before Next Milestone**:
- Enable `policy: "opaque"` on the full dogfood corpus and rerun the ts2pant test suite. Observe for 3–5 days.
- Watch for: (a) Pant rules that now verify but should not (false-positive verifications — the opaque encoding accidentally satisfies a property it shouldn't); (b) Pant rules that now fail to parse (composite-type opacity propagation bug); (c) regressions in UNSUPPORTED-skip diagnostics.
- Abort condition: any false-positive verification, or any new parse failure on a previously-passing fixture. If aborted, fix in a follow-up patch under M2 before proceeding to M3.
- Decision criterion to proceed: dogfood stable for the observation window AND the count of dogfood files that move from "rejected" to "verified-with-opaque-parts" is non-zero.

**Open Questions** (resolve during write-gameplan):
- Threading of the `policy` parameter through `mapTsType` callers — config flag (one switch for the whole pipeline run), per-fixture pragma (`// @ts2pant policy: opaque`), or per-call (most flexible, most plumbing)?
- For composite types with mixed opacity (`[number, unknown]`), is the encoding `Int * Opaque` (precision-preserving) or `Opaque` (uniform)? `Int * Opaque` is harder downstream but more useful.

---

### Milestone 3: ts2pant-opaque-default

*(✅ COMPLETE — PRs #330–#332. The M2→M3 observation-window blocker — an opaque-contagion crash on synthetic SSA loop-body nodes — was fixed first in PR #329; the corpus runs 111/111 under opaque.)*

**Definition of Done**:
- Default value of `mapTsType`'s `policy` parameter flips from `"reject"` to `"opaque"`.
- Every fixture in the corpus that previously rejected on `any` / `unknown` now produces Pant with the `Opaque` encoding; snapshots regenerate exactly once and the regenerated output is the new baseline.
- The `"reject"` policy remains available for fixtures that explicitly want the steering behavior (UNSUPPORTED-skip with "declare a real type").
- Documentation in `tools/ts2pant/AGENTS.md` (or local equivalent) describes the policy and when each value is appropriate.

**Why this is a safe pause point**: The opaque encoding is now the default and has been validated on the dogfood corpus during the M2→M3 observation window. The codebase is in a strictly more permissive state — code that was previously rejected now translates with explicit opacity. The reject policy is preserved as an opt-in. Reverting the workstream here means flipping one default back.

**Unlocks**: M4 has a stable target (the opaque baseline is now what narrowing recovers from), M5 has a meaningful sample (the corpus now contains `OpaqueValue` instances to survey).

**Open Questions** (resolved during write-gameplan):
- ~~Snapshot regeneration discipline — single commit vs. per-fixture cleanup commits?~~ **Resolved**: single regeneration co-located with the flip patch (Patch 2), forced by gameplan atomicity — a flip that left snapshots stale would leave the suite red between patches. Matches the repo's single-regeneration-commit precedent.
- ~~Default-resolution point — flip the pipeline default only, or resolve inside `mapTsType`?~~ **Resolved** (Design B): resolve the default inside `mapTsType` via `opts?.policy ?? DEFAULT_OPAQUE_POLICY` (single source of truth), not only at the pipeline boundary, matching the DoD wording.

---

### Milestone 4: ts2pant-opaque-narrowing

*(✅ COMPLETE — PRs #333–#335; built atop the landed opaque-default flip.)*

**Definition of Done**:
- At use sites of `any` / `unknown` values, `checker.getTypeAtLocation(useNode)` is consulted. If the narrowed type is concrete (not `any` / `unknown`), the use lowers to that concrete sort instead of `OpaqueValue`.
- The narrowing is *use-site local*: it applies at the specific occurrence, not retroactively to the declaration. Two uses of the same `unknown`-typed variable can lower to different sorts if they are narrowed differently.
- Common patterns covered: post-`typeof x === "..."` checks, post-`Array.isArray(x)`, post-discriminator-property checks on union types, post-`instanceof` (where the right-hand side is a declared class).
- Snapshots regenerate for fixtures exercising these patterns; new fixtures cover each narrowing form.
- Cross-function-call narrowing is *not* trusted by default — a fresh `OpaqueValue` flows in at the parameter binding even if the caller had narrowed it. This is documented as a conservative choice with rationale.

**Why this is a safe pause point**: Pure precision improvement. The encoding remains sound (we narrow only when the TS compiler asserts the narrowed type, which is a property the TS language already guarantees). Reverting M4 is mechanical (drop the `getTypeAtLocation` call site).

**Unlocks**: M5's survey now reflects post-narrowing opacity — sites that are still opaque after narrowing are the real monomorphization candidates. Also unblocks the guard-analysis workstream's M3b-deferred type-predicate cases — the "future M3c" that `tools/ts2pant/docs/guard-narrowing.md` notes needs a refined-type representation now has the opaque sort-recovery layer to build on.

**Open Questions** (resolved during write-gameplan):
- ~~Which narrowing forms to handle in the first cut and which to defer?~~ **Resolved**: recover every narrowed type that `mapTsType` supports under reject policy (scalars + supported composites + instanceof-class), with a conservative bail to Opaque on `UNSUPPORTED_UNKNOWN`. This covers typeof / `Array.isArray` / `instanceof` / discriminator automatically (TS does the narrowing; we map the result), needs no bespoke per-form allowlist, and ships ungated.
- ~~How to express the "cross-function narrowing not trusted" boundary in the printer / diagnostics?~~ **Resolved**: no per-occurrence diagnostic — the Opaque sort is already visible in output, and the boundary is documented in `opaque-narrowing.md`. Consistent with the M3 decision to add no opaque diagnostic.

---

### Milestone 5: ts2pant-opaque-mono-survey

*(Executed 2026-06-22 — not gameplanned: a read-only survey has no behavior change, feature flag, patch-sequencing, or atomicity surface, so it was implemented directly. Survey tool: `tools/ts2pant/src/mono-survey.ts` + `scripts/mono-survey.mts` (`npm run survey`); report at `tools/ts2pant/docs/opaque-mono-survey.{md,json}`.)*

**Outcome**: Over the dogfood corpus (42 files, 770 top-level functions), **1** function has a top-level `any`/`unknown` parameter (`emit.ts:isExecError`, verdict `disagree-unbounded`); **0%** all-agree, far below the 25% threshold. The dogfood corpus is a strict, self-hosted codebase that avoids top-level `any`/`unknown` by construction, so it is **not representative** of the `any`/`unknown`-heavy user code M6 targets — a near-zero count reflects the corpus, not the value of monomorphization. **Decision: do not proceed to M6 on this evidence; the workstream pauses at M5.** A go decision requires re-surveying representative `any`/`unknown`-heavy user TypeScript; absent that, M5 is the workstream's terminal milestone (the M5 DoD permits closing here). The classifier was validated against `tests/fixtures/mono-survey/cases.ts` (all six verdict branches) so a future re-survey over real inputs is a config change, not new work.

**Definition of Done**:
- A read-only whole-program pass walks every function whose signature contains `any` / `unknown` (declaration site) and records every call site within the same module (and dep modules if reachable).
- For each call site, the pass records the TS type observed at the argument position via `checker.getTypeAtLocation`.
- A report is emitted (machine-readable JSON + human-readable summary) per source file, listing: function name, declaration site, count of call sites, distribution of observed argument types, monomorphization-friendliness verdict (all-agree / disagree-but-bounded / disagree-unbounded / recursive / oversized).
- The report does *not* modify translation output. Snapshot-equivalent with M4 baseline.

**Why this is a safe pause point**: Pure measurement. The report informs the M6 decision but commits to nothing. If the report shows that monomorphization would not meaningfully recover precision (e.g., most `any` / `unknown` sites are already narrowed by M4, or disagreement at call sites dominates), the workstream legitimately ends here.

**Unlocks**: An evidence-based decision on whether M6 is worth the work.

**Operator Actions Before Next Milestone**:
- Run the survey across the dogfood corpus and any other representative ts2pant inputs.
- Read the human-readable summary. Compute: (a) fraction of `any` / `unknown` sites where all call sites agree on a concrete type, (b) fraction where disagreement is bounded (≤3 distinct types), (c) fraction that are recursive or oversized.
- Decision criterion to proceed to M6: at least 25% of sites are in category (a). Below that threshold, the precision recovery is not worth the implementation cost; close the workstream after M5.
- If proceeding, record the LOC threshold and the maximum-distinct-types threshold for M6 as part of the gameplan handoff.

**Open Questions** (resolved during implementation):
- ~~Report format?~~ **Resolved**: `opaque-mono-survey.json` (machine, regenerable) + `opaque-mono-survey.md` (human summary with a derived threshold verdict), both written by `npm run survey`.
- ~~Cross-module reach?~~ **Resolved**: candidates come from the requested files; call sites are scanned across the whole non-foreign program (`resolveSourceFileDependencies` + filter out default-lib/node_modules), so cross-file calls within the corpus are visible without chasing foreign imports.
- ~~Exported functions with external callers?~~ **Resolved**: flagged `externalCallersUnknown` (a function can be soundly specialized only per-visible-site, never replaced wholesale, when its full caller set is outside the surveyed program).

**Known limitation**: the survey scans top-level `FunctionDeclaration`s only (not class methods or arrow-const bindings). A grep confirmed this does not undercount the dogfood corpus (no arrow-const any/unknown-param helpers exist), but a re-survey over user code should lift this.

---

### Milestone 6: ts2pant-opaque-mono

**Definition of Done**:
- A new pipeline stage runs after M1–M5 are in effect but before L1 lowering. For each function whose signature contains `any` / `unknown` AND whose M5-survey verdict is "all-agree" AND whose body is under the LOC threshold AND which is non-recursive: the pass specializes the function under a mangled name with the agreed-upon concrete type substituted.
- All call sites are rewritten (in the in-memory TS AST or a side table consulted by `translate-signature.ts` / `translate-body.ts`) to invoke the specialized name.
- Functions that fail any criterion fall back to M2/M3's `Opaque` encoding unchanged.
- Specialization is opt-out per source file (a pragma or annotation can suppress it).
- New fixtures cover the all-agree case, the recursive-skip case, the oversized-skip case, and the disagree-fallback case.

**Why this is a safe pause point**: The full two-layer design is in place. Specialization recovers precision for the common case; the floor catches everything else. The workstream's vision is achieved. Reverting M6 leaves the floor + narrowing in place, which is itself a meaningful improvement over the starting state.

**Unlocks**: Genuinely precise verification of programs containing utility functions over `any` / `unknown` whose call graphs are closed. Closes the workstream.

**Open Questions** (resolve during write-gameplan):
- LOC threshold — small enough that specialization doesn't bloat output, large enough to catch real utilities. The M5 report will inform this.
- Recursion detection — direct only, or full SCC analysis? Full SCC is principled; direct-only is simpler and likely sufficient for the utility-function pattern.
- Interaction with dep modules — if the function being specialized is in a dep module, where does the specialized clone live?
- Name mangling scheme — `originalName__String`, `originalName_at_TypeArgString`, or something else? Must round-trip through Pant identifier syntax (no `$`, `<`, etc.).

## Dependency Graph

```text
1 (ts2pant-opaque-vocabulary)         → []
2 (ts2pant-opaque-opt-in)             → [1]
3 (ts2pant-opaque-default)            → [2]    (operator obs window between 2 and 3)
4 (ts2pant-opaque-narrowing)          → [2]
5 (ts2pant-opaque-mono-survey)        → [3, 4]
6 (ts2pant-opaque-mono)               → [5]    (operator decision between 5 and 6)
```

M3 and M4 can proceed in parallel once M2 has landed and the observation window is complete — M4 doesn't depend on the default flip, only on the opt-in policy existing. M5 wants both in place to get an accurate survey.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| `Opaque` granularity — single global sort vs. per-module-boundary | Decided: single global sort (simpler, can revisit if proofs need finer-grained distinctness). Listed here for traceability. | Resolved at workstream draft |
| Equality semantics on `Opaque` values | TS structural identity vs. Pant reference identity. Needs a fixture and a documented stance. | M1 gameplan |
| Interaction with `IsNullish` / list-lift encoding | `unknown \| null` after null-check is `unknown`. Likely composes as `[Opaque]`, but verify. | M2 gameplan |
| ~~Diagnostic surfacing for opaque-containing rules~~ | **Resolved (M3 gameplan)**: no opaque-origin diagnostic in M3 — the Opaque domain is visible in emitted Pant; revisit only if dogfood shows silent opacity is confusing. | Resolved 2026-06-21 |
| Cross-module monomorphization | Visibility constraints across dep modules. The survey (M5) measures the cost of *not* crossing modules. | M5 report informs M6 |
| Treatment of generic functions whose type parameters happen to bind to `any` / `unknown` at a call site | Out of scope for this workstream — generics handling is a separate problem. Recorded to avoid scope creep. | Out of scope |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| `any` and `unknown` share one mechanism, both lower to `Opaque` | TS's semantic distinction (any disables checking, unknown requires narrowing) does not transfer to Pant, which trusts neither. Diagnostics can still differentiate the origin if needed, but the encoding is uniform. |
| Floor + ceiling both in scope for this workstream | The two layers are conceptually one design — soundness floor plus precision ceiling — and surveying the ceiling's value (M5) is itself part of the answer. Splitting into two workstreams would hide the question "is the floor enough?" behind a separate planning step. |
| No global `Any` top-type with implicit coercions | Considered and rejected. A universal sort with coercions poisons every operation involving it (every Pant rule becomes trivially satisfiable on `Any` arguments). The opaque-sort-with-no-operations encoding preserves soundness because operations on `Opaque` either propagate opacity or are rejected at type-check, neither of which silently succeeds. |
| Cross-function-call narrowing not trusted by default (M4) | TS flow narrowing across call boundaries depends on assertion-function and predicate-type machinery that does not always survive ts2pant's intermediate representations. Conservative default: at every function boundary, parameters typed `any` / `unknown` re-enter as fresh `OpaqueValue`. Caller-side narrowing applies within the caller only. |
| Opt-in `"opaque"` policy before default flip (M2 → M3 split) | Lets us validate the encoding on a controlled subset before the corpus-wide regeneration. The observation window between M2 and M3 is the structural reason this workstream is not a single gameplan — it is exactly the kind of pause a gameplan cannot contain. |
| Resolve the opacity default inside `mapTsType` (M3, Design B) | `mapTsType` resolves an unspecified policy via `opts?.policy ?? DEFAULT_OPAQUE_POLICY`, giving one source of truth shared by the pipeline, direct callers, and tests — matching the DoD's "mapTsType's policy parameter default flips". Design A (flip the pipeline default only) was rejected: it would leave `mapTsType`'s literal default diverging from the product default. Cost: ~10 reject unit tests pinned to explicit `policy: "reject"`, which also keeps the reject path covered. |
| No opaque-origin diagnostic in M3 | M3 stays a pure default flip. Opacity is already observable in the emitted Pant (the `Opaque` domain), so it is not hidden; adding an informational hint would be a second behavior change with its own snapshot churn. Deferred, to revisit if dogfood shows the silent opacity is confusing. |
| Opaque-narrowing (M4) reads `getTypeAtLocation` directly, orthogonal to the guard-analysis AssumptionEnv | The AssumptionEnv tracks discriminant/non-null/predicate facts to discharge definedness obligations (partial-access safety); M4 is *sort selection* at a use site. TS's own intra-function flow narrowing is the authority for the narrowed type, so M4 reads `checker.getTypeAtLocation(useNode)` directly rather than inventing a redundant fact kind. The two narrowing layers are complementary, not the same mechanism. |
| Opaque-narrowing (M4) reuses the synthesized-opaque form's `sort` field | A narrowed-but-unconstrained value is represented as a fresh nullary rule `name => <narrowedSort>` — the existing opaque form already carries `sort` and the lowering/printer honor it, so no new L1 vocabulary is needed. Pant has no `Opaque`→concrete projection, so minting a fresh constant of the narrowed sort is the sound representation (consistent with the per-occurrence opaque-constant model). |
| Survey-before-implement for monomorphization (M5 → M6 split) | Two reasons: (a) the decision to proceed is conditional on the survey's verdict, which a gameplan cannot encode; (b) the survey itself produces useful diagnostic output even if M6 never lands, so M5 has independent value. |
| No dedicated design-doc milestone | Project convention: design questions live in the open-questions of the milestone they gate and are resolved during write-gameplan. Recorded here to short-circuit a recurring drafting mistake. |
