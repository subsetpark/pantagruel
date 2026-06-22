# Workstream: ts2pant Discriminated-Union Handling

## Vision

Give ts2pant first-class support for TypeScript discriminated unions: a value
of `type Shape = {kind:"circle";r} | {kind:"square";s}` translates to a single
Pantagruel domain with a discriminant rule and per-variant field rules guarded
by the discriminant literal, and `if (x.kind === "circle") … x.r …` /
`switch (x.kind)` narrowing discharges those guards so variant-field access is
provable. The end state turns today's `> UNSUPPORTED: ambiguous owner` refusal
into sound, entailing tagged-union semantics — unblocking self-translation of
ts2pant's own discriminated-union-heavy code (`IR1Expr`, `IR1Stmt`, the
translator result types) and any user code that models sum types this way.

## Current State

ts2pant has **no awareness of discriminated unions as a concept** and **no type
narrowing at all** (confirmed: narrowing is explicitly deferred at
`ir1-build.ts:1173` — "literal-union narrowing is deferred to a follow-up;
uniform rejection" — and `nullish-recognizer.ts` deliberately avoids relying on
TS flow-narrowing). Today each union member is encoded as its **own synthesized
record domain joined by Pant's sum `+`** (`tag x: KindRRec + KindSRec => …`),
and field access goes through the generic `resolveFieldOwner` /
`collectFieldOwners` path (`translate-types.ts` ~870/895):

- A field unique to one member resolves to that member's accessor and "works"
  (e.g. `radius x = kind-r-rec--r x`) — but possibly **unsoundly**, since the
  accessor is applied to the whole union.
- A field on ≥2 distinct owners (the discriminant `.kind`, or a field
  independently declared on each member) returns `ambiguous` and the access is
  **refused** (`ir1-build.ts:1346`, `translate-body.ts:1582`).

There is no discriminant detection, no literal-tag handling, and no
variant-membership concept. The closest existing machinery is the **Map
guarded-rule encoding** (`entriesKey c k` guards `entries c k`), which is exactly
the shape this workstream generalizes.

## Key Challenges

- **The encoding must be sound without narrowing, and entail with it.** The
  guarded-rule encoding (`shape--r s: Shape, shape--kind s = "circle" => Int`)
  is sound by construction even before narrowing exists — the guard simply isn't
  discharged, identical to how `.get(k)!` produces a guarded `entries` rule
  today. The open question is whether it **entails** under z3 once narrowing
  discharges the guard (string-literal-equality guards, variant disjointness).
  This is a measurement, not an assumption — hence the survey milestone.

- **Narrowing is greenfield and the hardest part.** Reading `x.kind` is trivial
  under this encoding (a plain rule), but the *value* of DUs is narrowing:
  inside `if (x.kind === "circle")`, the guard `shape--kind x = "circle"` must
  flow into the branch context so `shape--r x` is discharged. ts2pant has no
  flow-narrowing layer, and the any/unknown-opaque workstream already flagged
  that "TS flow narrowing … does not always survive ts2pant's intermediate
  representations." M3 therefore builds **local intra-function narrowing on top
  of the guard-analysis workstream's flow-narrowing layer**, rather than a
  bespoke mechanism (see Decisions).

- **It is a migration, not just an addition.** The tagged encoding replaces the
  `+`-of-records encoding for discriminated unions — including the currently-
  "working" unique-field case — so M1 is a behavior change to existing output,
  and the full retirement of `+`-records (for non-discriminated unions, nested
  unions, intersections) is staged into M4 to bound regression risk.

- **Structural detection only — no field-name special-casing.** Per a hard
  project constraint, the translator must NOT recognize `.kind` / `_tag` /
  `type` by name. "Discriminated" is a structural property: a field present on
  every union member whose type is a *distinct literal* on each member. Any
  union lacking such a field stays on the existing refusal/`+`-records path.

- **Variant-field guards interact with the SSA/mutation and switch paths.**
  Field access inside `switch (x.kind)` clauses and `if` branches must thread
  the discriminant guard through the same machinery that lowers conditionals and
  (eventually) mutations.

## Established Precedents

- **existing-implementation — In-repo Map guarded-rule encoding** —
  `tools/ts2pant/src/translate-types.ts` + `tools/ts2pant/tests/fixtures/constructs/expressions-map.ts`
  The direct template for the whole workstream: `Map<K,V>` on a field expands to
  `entriesKey c k: … => Bool` (membership predicate) + `entries c k: …, entriesKey c k => V`
  (value rule guarded by the predicate), and the `congruence` fixture shows it
  entails under z3. The DU encoding is the same shape — a discriminant rule plus
  per-variant field rules guarded by `discriminant = literal` — synthesized
  through the same synth-cell infrastructure (`cellRegisterMap`-style
  registration, `emitXSynthDecls`-style emission). Every milestone touches this.

- **algorithm — McCarthy theory of arrays + guarded (Dafny-style) preconditions** —
  https://doi.org/10.1145/321033.321034
  The semantic model for guarded field access: a variant field is a partial
  function defined only where its discriminant guard holds, mirroring
  McCarthy select guarded by a membership/precondition predicate (the same
  framing the Map encoding cites). Shapes M1's guarded rules and M3's
  guard-discharge obligation.

- **paper — Kroening & Strichman, Decision Procedures Ch. 4 (EUF)** —
  https://www.decision-procedures.org/
  The discriminant rule and per-variant field rules are uninterpreted functions;
  soundness rests on congruence, and the discriminant-literal guards add only
  sound equalities. The survey (M2) measures entailment against this floor; M3's
  narrowing must not introduce an unsound axiom.

## Milestones

### Milestone 1: du-tagged-encoding — ✅ COMPLETE (PRs #290–#291)

> **Being executed now**, ahead of further guard-analysis work: M1 is independent
> of guard-analysis (it is the encoding, not narrowing) and unblocks M2 and — with
> guard-analysis M3a — M3. Gameplan: INFRA detection + dormant tagged-encoding synth
> (mirrors the Map guarded-rule encoding), then a BEHAVIOR patch wiring detection
> into `mapTsType` + `collectFieldOwners`. Sound-by-guard (guards undischarged until
> M3), conservative-by-bail (non-discriminated unions keep the `+`-encoding).

**Definition of Done**:
- A TypeScript union detected as **discriminated** — structurally: a field
  present on every member whose type is a distinct literal (string/number/bool)
  on each member, with no field-name special-casing — synthesizes a single Pant
  domain with: a discriminant rule (`shape--kind s: Shape => String`) and, for
  each member, that member's field accessor rules **guarded by the discriminant
  literal** (`shape--r s: Shape, shape--kind s = "circle" => Int`). Registered
  via the synth-cell infrastructure and emitted alongside the existing
  record/map synth decls.
- Field access on a discriminated-union receiver emits the guarded rule
  application (discriminant read is unguarded; variant-field read carries its
  guard), replacing the `ambiguous owner` refusal **and** the current
  unique-field `+`-records accessor for discriminated unions.
- Non-discriminated unions, intersections, and nested unions are untouched —
  they keep the existing `+`-records encoding / ambiguous refusal.
- No narrowing: variant-field reads are well-formed but their guards are not yet
  discharged (sound-by-guard, identical to `Map.get` today). Fixtures
  type-check; entailment of variant-field assertions is not yet expected.

**Why this is a safe pause point**: Discriminated unions now emit a sound,
type-checking tagged encoding instead of a refusal; nothing regresses for
non-discriminated unions (untouched path). Variant-field assertions may not
entail yet, but no emitted document is unsound — the guards are present and
simply undischarged, exactly the established Map-encoding posture. If the team
stops here, ts2pant emits well-formed tagged-union Pant for every discriminated
union, a meaningful standalone improvement.

**Unlocks**: M2 can measure entailment of the new encoding and inventory the
narrowing patterns that would discharge the guards.

**Operator Actions Before Next Milestone**:
- Run `npm run -s test:unit` and `npm run -s test:integration`; confirm no
  regression on the non-discriminated-union corpus and that discriminated-union
  fixtures type-check.
- Spot-check 2–3 emitted tagged encodings against their TS source to confirm the
  discriminant detection is structural (not name-based) and the guards are
  correct.

**Established Precedents** (milestone-scoped): none beyond the workstream-level
Map encoding + McCarthy/Dafny guards, which this milestone consumes directly.

**Open Questions**:
- Synthesis home and naming: does the DU domain register through the same synth
  cell as records/maps, and what is the domain-name derivation (must avoid the
  field-name-based naming the current `+`-records path uses)? Resolve during
  M1's gameplan.
- Disjointness: should the encoding emit an explicit invariant that the
  discriminant's literal values are pairwise distinct across variants, or rely
  on congruence alone? Resolve during M1's gameplan.

---

### Milestone 2: du-entailment-narrowing-survey — ✅ COMPLETE (2026-05-28)

> Run read-only, no gameplan. Report: `tools/ts2pant/docs/du-entailment-narrowing-survey.md`.
> **Findings:** (1) M1 encoding entails under z3 when the discriminant guard is
> discharged (the post-narrowing target shape compiles cleanly); disjointness is
> free from function-value semantics, totality is NOT free (M3 must decide whether
> to emit a totality invariant — recommendation: emit it). (2) DU-relevant
> narrowing inventory: ~37 `switch (x.kind)` over structural DUs + 160
> `.kind === "<literal>"` ifs (excluding `ts.SyntaxKind` enum compares), dominated
> by ts2pant's own IR1 walkers. (3) M3 consumes the guard-analysis M3a
> `<property-access> === <literal>` fact unchanged — no DU-specific narrowing
> infrastructure needed. **Verdict:** proceed to M3, gated on guard-analysis M3a
> landing; emit-totality decision resolved in M3 gameplan.

**Definition of Done**:
- A read-only survey over the dogfood + constructs corpus: for each
  discriminated-union `@pant` assertion (and a set of authored probe
  assertions), classify under `pant --check` (z3) as type-checks / entails /
  blocked-on-narrowing / blocked-on-a-missing-encoding-invariant.
- An inventory of the **narrowing patterns** that actually appear: `if (x.kind
  === lit)`, `switch (x.kind)`, early-return guards on the discriminant, and
  whether they are intra-function (in scope) or cross-call/assertion-function
  (out of scope per the local-only decision).
- A committed survey report (under `tools/ts2pant/docs/`) with: per-pattern
  counts, the entailment verdict on the M1 encoding (does the guarded encoding
  entail once a guard is assumed?), and a concrete narrowing design proposal
  that names the guard-analysis flow-narrowing layer M3 will consume.
- No translator behavior change — measurement only.

**Why this is a safe pause point**: Nothing changed in emitted output; the
codebase is exactly as it was after M1 plus a report. The measure→decide
boundary that justifies splitting narrowing out from the encoding.

**Unlocks**: An evidence-based go/no-go and scope for M3, and confirmation that
the M1 encoding entails once guards are discharged (or a list of encoding
invariants to add first).

**Operator Actions Before Next Milestone** (the measure→decide boundary):
- **Review the survey report and decide whether to proceed to M3.** Proposed
  gating criterion (confirm during M2's gameplan): proceed iff (a) the M1
  encoding entails under z3 when a discriminant guard is assumed, and (b) ≥ a
  threshold of real intra-function narrowing sites exist in the corpus. If the
  encoding does not entail even with the guard assumed, do NOT start M3 — open an
  encoding-fix follow-up first.
- **Confirm the guard-analysis dependency is ready.** M3 consumes the
  guard-analysis workstream's **M3a `du-discriminant-narrowing-layer`** (the
  discriminant-narrowing fact `<property> === <literal>`); verify that milestone has
  landed (or sequence M3 after it). If it has not, M3 is blocked — do not start.
- **Abort condition**: if the survey shows the dominant blocker is
  cross-call/assertion narrowing (out of scope), reduce M3 to the intra-function
  subset or defer.

**Open Questions**:
- The exact gating threshold and the precise interface M3 will consume from the
  guard-analysis layer. Resolve during M2 (the survey produces them).

---

### Milestone 3: du-discriminant-narrowing — ✅ COMPLETE (PRs #299–#301, #303–#304)

> **Update (2026-05-28):** the guard-analysis M3a gameplan
> (`gameplans/ts2pant-du-discriminant-narrowing-layer.json`) ships the
> assumption-environment infrastructure AND the variant-field-rule discharge
> wiring AND a narrowing-aware fixture whose `@pant` annotations entail under
> z3. DU M3's scope correspondingly narrows to (a) the totality-invariant
> decision from the M2 survey §5 (whether to emit `all s: Shape | shape--kind
> s = "circle" or shape--kind s = "square"` per DU domain), and (b) DU-corpus-
> wide `@pant` entailment validation across the existing discriminated-union
> fixtures + dogfood (`DiscriminantLiteral`, `FieldOwnerResolution`, etc.).
> The "narrowing mechanism" half is landed by M3a — DU M3 is the validation +
> totality-decision milestone, not a new mechanism.

**Definition of Done**:
- **Local, intra-function** narrowing: inside `if (x.kind === lit)` /
  `switch (x.kind)` / early-return guards on the discriminant, the guard
  `shape--kind x = lit` is propagated into the branch context so variant-field
  reads (`shape--r x`) have their guards discharged and the corresponding
  `@pant` assertions entail under z3.
- Narrowing is built on the **guard-analysis workstream's flow-narrowing /
  predicate-following layer** (consumed, not reinvented); cross-call and
  assertion-function narrowing remain out of scope and refused, matching the
  any/unknown-opaque workstream's "cross-function-call narrowing not trusted by
  default" decision.
- The survey's flagged narrowing assertions now entail; the survey's before/
  after entailment set guards against regressions.

**Why this is a safe pause point**: Discriminated-union access is now provable in
the common narrowed patterns; un-narrowed access remains sound-by-guard. The
headline DU use case (`switch (x.kind)` returning per-variant fields) works
end-to-end.

**Unlocks**: M4 cutover — once narrowing is proven, the old `+`-records encoding
has no remaining advantage for discriminated unions and can be retired.

**Operator Actions Before Next Milestone**:
- Confirm the before/after entailment set from M2 shows no regression and the
  targeted narrowing assertions now entail.

**Established Precedents** (milestone-scoped):
- **existing-implementation — guard-analysis M3a env + discharge wiring** —
  `gameplans/ts2pant-du-discriminant-narrowing-layer.json` (the gameplan;
  may not survive past patch landing — once the patches land, the durable
  references are `tools/ts2pant/src/assumption-env.ts`,
  `tools/ts2pant/src/narrowing-recognizer.ts`, and the env-threading sites in
  `ir1-build.ts` / `ir1-build-body.ts`). M3 consumes the env's discharge
  result rather than building a bespoke narrowing mechanism. This was the
  **cross-workstream dependency**; M3a delivers it.

**Open Questions**:
- The precise propagation surface for the discriminant guard through ts2pant's
  IR/SSA (the any/unknown workstream flagged that flow narrowing "does not always
  survive" the IR). Resolve during M3's gameplan, informed by the guard-analysis
  layer's actual shape.

---

### Milestone 4: du-cutover — ✅ COMPLETE (PRs #307–#311)

> **Planned (2026-05-29).** Two "handled-or-refused" choices in the original
> DoD were resolved while planning: nested DUs are **handled** by recursive
> synthesis (not refused), and for non-discriminated unions only **field access**
> is refused — the `A + B` value encoding is **retained**. See Decisions Made.

**Definition of Done**:
- The `+`-of-records encoding is retired for all discriminated unions; the tagged
  encoding is the sole path. A detected DU that fails tagged registration is
  refused with a clear reason rather than falling through to `+`-records.
- Nested discriminated unions and discriminated unions inside other synthesized
  types (records, maps, arrays) are **handled** by recursive tagged synthesis
  (one shape-keyed domain shared with the same DU at top level); their narrowed
  variant-field reads entail under `pant --check`.
- Field access on a **non-discriminated** union is soundly refused (no
  unique-field accessor applied to the whole union — the unsoundness noted in
  Current State is eliminated); the `A + B` sum **value** encoding is retained
  for non-discriminated unions, and optionality (`T | null` → `[T]`) is
  untouched. Intersection field access remains sound (AND semantics) and is
  unchanged.
- Documentation under `tools/ts2pant/docs/` describes the tagged-union encoding,
  its narrowing, and the refusal taxonomy.

**Why this is a safe pause point**: Terminal milestone. Discriminated unions have
a single, sound, narrowing-capable encoding; the legacy path is gone; the
remaining union shapes are soundly refused. The DU story is complete.

**Unlocks**: Closes the workstream. Any remaining union expressivity (general
non-discriminated unions) is a separate effort with its own encoding question.

**Operator Actions Before Next Milestone**: none (terminal).

**Open Questions**:
- Whether non-discriminated unions get any encoding at all, or stay permanently
  refused. Default: stay refused (no special-casing, no unsound accessor).
  Revisit only with a separate workstream.

## Dependency Graph

```text
1 (du-tagged-encoding)            → []
2 (du-entailment-narrowing-survey) → [1]
3 (du-discriminant-narrowing)     → [2]  + cross-workstream: guard-analysis flow-narrowing layer
4 (du-cutover)                    → [3]
```

**Note**: Strictly sequential. M1–M2 can proceed immediately. M3 is additionally
**gated on the guard-analysis workstream's M3a `du-discriminant-narrowing-layer`**
(the discriminant-narrowing fact); if that milestone has not landed when M2
completes, M3 waits. M4 depends on M3's narrowing being proven.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Does the M1 guarded encoding entail under z3 once a discriminant guard is assumed? | The whole basis for M3. Measured, not assumed. | Milestone 2 (survey) |
| Gating threshold for proceeding to M3 | Count of real intra-function narrowing sites + encoding-entails verdict. | Milestone 2 |
| Interface M3 consumes from the guard-analysis flow-narrowing layer | Depends on that workstream being planned. | Milestone 2 / guard-analysis planning |
| Discriminant disjointness invariant: emit explicitly or rely on congruence? | Affects M1 encoding. | Milestone 1 |
| Fate of non-discriminated unions | ✅ Resolved (M4 planning, 2026-05-29): **field access** is refused (no unsound accessor); the `A + B` **value** encoding is retained, not wholesale-refused. | Milestone 4 |

## Decisions Made

These are decisions that did NOT produce a citable precedent — framing,
constraints, and rejected approaches. Accepted precedents (Map guarded-rule
encoding, McCarthy/Dafny guards, Kroening & Strichman EUF) live in `Established
Precedents`.

| Decision | Rationale |
|----------|-----------|
| No field-name / discriminant special-casing in the translation layer | Hard project constraint (2026-05-27): the translator must not recognize `.kind`/`_tag`/`type` by name. "Discriminated" is detected structurally (a field whose type is a distinct literal on every member). A name-based shortcut would unblock discriminant reads but leave narrowing unsound — worse than a clean refusal. |
| Guarded-rule encoding (not a `cond`-dispatch reduction) | A probe showed the current `+`-records encoding can't lower `x.kind` via cond-dispatch (circular: reading the discriminant is the access). The guarded-rule encoding (discriminant rule + per-variant guarded field rules) reads the discriminant directly and makes variant fields partial-on-the-guard — directly analogous to the proven Map encoding. |
| M3 narrowing is local intra-function only | Matches the any/unknown-opaque workstream's "cross-function-call narrowing not trusted by default" decision. Cross-call / assertion-function narrowing is out of scope; bounded and self-contained. |
| M3 narrowing depends on (consumes) the guard-analysis workstream's flow-narrowing layer | Chosen over a bespoke DU-only narrowing mechanism: narrowing is shared infrastructure, and ts2pant has none today. This introduces a cross-workstream dependency (M3 gates on guard-analysis), accepted to avoid duplicating flow-narrowing machinery that the guard-analysis roadmap must build anyway. |
| Encoding migration staged (M1 introduces tagged encoding for discriminated unions; M4 retires `+`-records) | The tagged encoding is a behavior change to currently-emitted output, including the currently-"working" unique-field case. Staging the cutover into M4 bounds regression risk and keeps every milestone a safe pause point. |
| Standalone workstream (not part of free-call-decl) | Discriminated-union handling is an independent capability; free-call-decl is complete at M2. |
| Multi-discriminant tie-break: pick the first qualifying field by **sorted field name** (deterministic) | Decided while planning M1's gameplan. When >1 field is a distinct literal on every member, a deterministic pick keeps detection total and stable (every discriminated union encodes); requiring exactly one would refuse legitimate multi-discriminant unions. Non-chosen qualifying fields still become per-variant guarded rules. |
| M4 retires `+`-records for discriminated unions and refuses **non-discriminated union field access only** — the `A + B` value encoding is kept | Decided while planning M4's gameplan (2026-05-29). The unsoundness in Current State is the unique-field accessor applied to the whole union — a field-access concern. Bare `A + B` is sound when no field is accessed, so refusing the value encoding too would over-refuse unions used soundly as opaque values for no soundness gain. Matches the DoD's "no silent unsound accessor." Optionality (`T \| null` → `[T]`) and intersection field access (sound by AND semantics) are unchanged. |
| M4 handles nested/embedded discriminated unions by **recursive tagged synthesis**, not refusal | Decided while planning M4's gameplan (2026-05-29). The Vision targets self-translation of ts2pant's own DU-heavy IR types (`IR1Expr`, `IR1Stmt`), which nest DUs; refusing nesting would leave the headline use case unmet at the terminal milestone. The synth machinery is already shape-keyed and recursive, so this is the same proven encoding rather than a new mechanism. |
