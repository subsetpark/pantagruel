# Workstream: ts2pant Body Completeness

## Vision

Make ts2pant's TypeScript body lowering accept the common, unambiguous
statement shapes that remain after the core IR, local-binding SSA, guard
analysis, for-of comprehension, and foreign-call milestones. The end state is
not "translate arbitrary JavaScript"; it is a disciplined expansion of
structural body coverage for code that already has a clear Pantagruel target:
nested pure block returns, callback blocks, record-return conditionals, local
collection builders, and bounded switch/iteration sequencing. Each admitted
shape lowers through named transformations already documented in
`tools/ts2pant/docs/transformations.md`, and unsupported shapes continue to
reject with precise diagnostics.

## Current State

The body-lowering work has been handled so far as standalone gameplans rather
than as a parent workstream:

- `gameplans/ts2pant-body-lowering-completeness.json` landed simple
  block-bodied early-return arms and switch block clauses.
- `gameplans/ts2pant-for-of-comprehension.json` landed the build-list
  `for-of` comprehension shape.
- `gameplans/ts2pant-foreign-accessor-body-lowering.json` and
  `gameplans/ts2pant-foreign-call-guard-admission.json` reduced foreign
  accessor/predicate rejection buckets.
- `workstreams/ts2pant-local-binding-ssa.md` completed the local-binding SSA
  substrate; `workstreams/ts2pant-guard-analysis.md` completed the guard and
  narrowing track.

After Milestone 1 landed, the read-only corpus diagnostic
(`tools/ts2pant/scripts/corpus-diag.mts --summary-only`) over
`tools/ts2pant/src` reports 829 top-level functions, 166 clean translations,
658 functions with UNSUPPORTED output, and 5 build errors. The top residual
buckets are heterogeneous:

- 76 discriminated-union registration failures — owned by the DU workstream,
  not this one.
- 59 `for-of loop is not a recognized build-list comprehension`.
- 57 `early-return predicate has side effects` and 33
  `early-return value has side effects` — guard/foreign-call follow-ons, not
  this workstream's core.
- 49 `expression statement before return`, usually local collection builders
  such as `lines.push(...); return lines`.
- 42 nested pure block-return failures:
  `if-with-return block must contain only const bindings followed by a return`.
- 22 `local bindings or multiple statements before return`.
- 19/14 switch default/case body-ending failures.
- 14 `statement is not supported by unified SSA body lowering`.
- 11 `branch call is not a recognized Map/Set effect`.
- Record-return conditionals moved down to residual split buckets:
  5 `record return branches must all return object literals with the same field
  set` and 2 legacy
  `record return combined with early-return arms or if/else branches`.

This workstream owns the structural body-lowering subset of those residuals:
the remaining nested block-return bucket, the local collection/sequencing buckets, and
bounded switch/iteration/record cleanup. It explicitly does not own DU
self-registration, foreign method synthesis, guard purity, or non-discriminated
union field access.

## Key Challenges

- **The buckets are not one feature.** Several high-count labels are symptoms
  of different transformations: nested if-conversion, local collection-builder
  recognition, loop/iterator lowering, switch value semantics, and record
  fieldwise conditionals. Trying to fold all of them into one gameplan would
  violate gameplan atomicity.

- **Local collection builders are stateful even in pure functions.**
  `const lines = []; lines.push(x); return lines` is pure at the function
  boundary but imperative internally. It cannot be admitted by pretending the
  expression statement is pure; it needs a local collection-builder model that
  lowers mutation into a list/set/map value.

- **Switch and iteration shapes have real semantic traps.** Fall-through,
  `break`, `continue`, `throw`, non-literal labels, and default-not-last cases
  are not equivalent to a simple `cond`. The workstream must admit only the
  subset with a clear value target, and it must keep unsupported cases visibly
  rejected.

- **Snapshot churn can hide regressions.** Many milestones change emitted Pant
  text for newly-supported fixtures. Existing successful fixtures should remain
  byte-identical unless a milestone explicitly owns a broader normalization
  change.

- **Body completeness overlaps other workstreams at the edges.** DU,
  guard/foreign-call, opaque narrowing, and local-binding SSA can all surface as
  body rejections. This workstream needs strong ownership boundaries so it does
  not absorb unrelated type-system or foreign-API work.

## Established Precedents

- **paper — Allen et al. 1983 if-conversion** —
  https://dl.acm.org/doi/10.1145/567067.567085
  Nested early returns, terminal branch blocks, and switch cases all reduce
  control dependence to data dependence when each branch has a value-position
  target. Milestones that turn statement control flow into `cond` values
  inherit this precedent.

- **paper — Flanagan et al. 1993 A-normal form / compiling with continuations** —
  https://dl.acm.org/doi/10.1145/155090.155113
  The body prelude scanner is effectively operating over ANF-like TypeScript:
  local bindings and simple statements before a terminal value. This precedent
  shapes the separation between pure value-producing preludes and stateful
  local collection builders.

- **algorithm — Capture-avoiding substitution** — null
  Local binding inlining and block-local substitution must use the existing
  hygienic substitution primitives (`substituteIR1ExprSubtree`,
  `substituteIR1StmtSubtree`, and the Pant wasm substitution API), never
  textual replacement. This applies to nested block returns, callback blocks,
  and any single-use optimization in later milestones.

- **paper — Cytron et al. 1991 SSA construction** —
  https://doi.org/10.1145/115372.115320
  Local sequencing and local collection builders should reuse the existing IR1
  SSA discipline rather than inventing a parallel state model. This precedent
  applies whenever the body path versions local scalar or collection state
  before emitting a final value.

- **pattern — Conservative recognizer with precise refusal** — null
  This is the project-level steering principle documented in
  `tools/ts2pant/AGENTS.md`: admit unambiguous TypeScript forms when there is a
  clear Pant target, and reject ambiguous forms with a diagnostic that explains
  the boundary. Every milestone in this workstream must preserve that behavior.

- **paper — Meijer, Fokkinga & Paterson 1991, Functional Programming with
  Bananas, Lenses, Envelopes and Barbed Wire** —
  https://maartenfokkinga.github.io/utwente/mmf91m.pdf
  Collection builders are catamorphisms over the source list: a for-of/forEach
  build-list is `map`/`filter`, a Set builder is the same recursion scheme
  observed by membership, and a commutative identity-bearing scalar fold is
  `foldr` with a monoid combiner (whose unit is the elided init). This frames
  M2's straight-line builders, M3's loop-backed builders and scalar folds, and
  the existing Structured Iteration Shape A/B/C lowerings under one scheme.

## Milestones

### Milestone 1: body-lowering-completeness-rd2 — COMPLETE (PRs #351–#355)

> Landed on master through commits `a61c356`, `2c4ba68`/follow-up Patch 2
> fixes, `9507077`, `29a6510`, and `3e28c3f` / PRs #351–#355. Post-landing
> diagnostic (`--summary-only`) shows the targeted nested block-return bucket
> dropped from 64 to 42, while record-return conditionals moved from the
> original 8-count legacy bucket to smaller, sharper residual buckets.

**Definition of Done**:
- `gameplans/ts2pant-body-lowering-completeness-rd2.json` is implemented.
- `tools/ts2pant/scripts/corpus-diag.mts` is committed as a read-only
  measurement tool.
- Nested pure block-return lowering exists as a shared helper around the
  current pure prelude machinery.
- Early-return arms, terminal if/else branch blocks, switch case/default
  blocks, and array callback block bodies can reuse the nested pure block
  lowering when their nested body is supported.
- Record-return conditionals whose arms all return object literals with the
  same explicit field set lower to fieldwise conditional equations.
- Negative fixtures for stateful local accumulator sequencing, fall-through
  switches, non-literal labels, effectful block consts, non-final returns, and
  mismatched record shapes remain unsupported.
- `just ts2pant-test-unit`, `just ts2pant-test-integration`, and the corpus
  diagnostic pass.

**Why this is a safe pause point**:
The milestone only admits shapes that already reduce to existing `cond`,
let-elimination, and record-return machinery. Unsupported stateful sequencing
continues to reject, so the translator gains coverage without pretending local
mutation is pure. If the workstream pauses here, the codebase has a strictly
more capable pure block-return recognizer and a committed measurement harness.

**Unlocks**:
The next milestone can focus on local collection-builder sequencing without
also carrying nested branch/callback cleanup. The post-M1 diagnostic gives a
fresh residual ranking before committing to M2's exact fixture set.

**Operator Actions Before Next Milestone**:
- Done: run `NODE_OPTIONS=--max-old-space-size=6144 npx tsx tools/ts2pant/scripts/corpus-diag.mts --summary-only`.
- Done: record post-M1 baseline: 829 functions, 166 clean, 658 unsupported, 5
  errors. Top structural body buckets: for-of non-comprehension 59,
  expression statement before return 49, nested block-return 42,
  local/multiple statements before return 22, switch default/case 19/14.
- Done: confirm the `if-with-return block...` and record-return buckets moved down.
- Before writing the M2 gameplan, run a focused shape survey over the 59 for-of
  and 49 expression-statement examples. If the for-of bucket is mostly local
  collection builders, keep M2 as planned and make it the shared builder target
  that M3 consumes; if it is dominated by a non-builder loop family, swap M2/M3
  or split out a measurement milestone.

**Open Questions**:
- Resolved by the existing rd2 gameplan: local accumulator sequencing is
  deferred out of M1.

---

### Milestone 2: local-collection-builder-sequencing — COMPLETE (PRs #357–#360)

> Landed on master via `gameplans/ts2pant-local-collection-builder-sequencing.json`
> (PRs #357–#360). Finite ordered list builders emit cardinality plus 1-based
> positional assertions over the declared return rule; straight-line Set `.add`
> builders emit membership assertions over the Set-as-list return value; Map
> builders are deferred to a Map-specific milestone.
>
> Post-M2 corpus diagnostic baseline (`--summary-only`): 844 top-level
> functions, 170 clean, 669 unsupported, 5 errors. Top structural
> body-completeness buckets: `for-of loop is not a recognized build-list
> comprehension` 52, `if-with-return block...` 43, `expression statement before
> return` 41, `local bindings or multiple statements before return` 22, switch
> default/case 19/14. (The 79-count `alias IR1ForeachCondStmt` and 59/33
> early-return-side-effects buckets are DU-encoding artifacts and guard/foreign
> follow-ons, not body-completeness-owned.) A shape survey of the 52 for-of
> failures: list-push-extended 30 (dominant), set-add 1, scalar-fold 3,
> control-flow 7, multi-collection 3, no-for-of/other 7 — which set M3's first
> loop family.

**Definition of Done**:
- The pure-body prelude recognizes bounded local collection builders such as
  `const xs = []; xs.push(e); return xs`, `const s = new Set<T>();
  s.add(e); return s`, and small straight-line variants with const-bound
  projected values.
- Ordered list lowering emits assertions over the declared `[T]` return rule:
  `#(f args) = N` plus one 1-based positional equation `(f args) i = value_i`
  per pushed value. It does not use list literals, synthetic finite tuples, or
  helper domains.
- Set lowering emits assertions over the returned Set/list value: membership is
  equivalent to the straight-line `.add` values, and empty Set builders emit
  universal non-membership. It does not track insertion order or duplicate adds
  beyond membership.
- Map builder construction remains unsupported in M2. It is deferred because
  Map construction must respect the guarded Stage A/Stage B membership/value
  rule-pair contract.
- Unsupported variants stay rejected: dynamic aliasing of the accumulator,
  unknown mutating method calls, mutation after the accumulator escapes, Set
  `.delete` / `.clear` builders, Map builders, nested loops not handled by the
  milestone, and ambiguous element order.
- The targeted `expression statement before return` bucket moves down in the
  post-M2 corpus diagnostic.
- `tools/ts2pant/tests/local-collection-builder.test.mts` covers positive
  ordered-list and Set-builder cases plus preserved negative cases.

**Why this is a safe pause point**:
The milestone turns one coherent stateful pure-body idiom into an explicit
value construction model. It does not relax the general expression-statement
gate; any statement that is not a recognized local builder remains rejected.
The codebase remains consistent because supported builders emit checkable
Pant values, while unsupported local mutation remains visible.

**Unlocks**:
Follow-on iteration completeness: once straight-line builders are expressible,
`for-of` and `forEach` variants can target the same builder representation.

**Operator Actions Before Next Milestone**:
- Done: re-ran the corpus diagnostic (post-M2 baseline recorded above) and split
  the 52 for-of failures by shape: list-push-extended 30, set-add 1,
  scalar-fold 3, control-flow 7, multi-collection 3, no-for-of/other 7.
- Done: M3's first loop family is the for-of list-build widening (largest family
  reusing the for-of `each` comprehension target); Set builders and commutative
  scalar folds added as adjacent reuse, no-identity reduces and forEach deferred.

**Open Questions**:
- Resolved by the M2 gameplan: finite ordered list builders use cardinality
  plus 1-based positional assertions over the declared return rule.
- Resolved by the M2 gameplan: M2 covers straight-line list and Set builders;
  Map builders are deferred to a later Map-specific milestone.

---

### Milestone 3: iteration-builder-completeness — PLANNED

> Planned by `gameplans/ts2pant-iteration-builder-completeness.json` (5 patches:
> 2 INFRA + 3 BEHAVIOR chained 2→3→4). Concrete scope, fixed by the post-M2
> survey: (a) widen the for-of build-list recognizer to admit pure loop-local
> `const` bindings before the push and compound/nested guards, reusing the
> existing `each binder in src, guards | proj` comprehension; (b) for-of Set
> builders via a membership-equivalence assertion over a `some` comprehension
> (`all e: T | e in (f args) <-> some n in src, guards | e = proj n`), sound
> under deduplication; (c) pure-body scalar accumulator folds for commutative
> identity-bearing OP ∈ {+, *, &&, ||} (and `count++`), reusing the Shape-C
> `combOP over each` reduce with identity-elision. No-identity / nullable
> reduces (conjoin/disjoin) and `forEach` callbacks are deferred (see Decisions
> Made and Open Questions).

**Definition of Done**:
- For-of list builders whose loop body has pure loop-local `const` bindings
  before the (optionally guarded) push, or compound/nested guards, translate to
  the `each binder in src, guards | proj` comprehension instead of rejecting as
  `for-of loop is not a recognized build-list comprehension`.
- For-of Set builders (`const s = new Set<T>(); for (const x of xs) [if (g)]
  s.add(proj(x)); return s`) translate to the membership-equivalence assertion
  over a `some` comprehension (never to a list-equality `each`).
- Pure-body scalar accumulator folds with a commutative identity-bearing
  combiner translate to `init OP (combOP over each n in src[, g] | f n)` with
  `init` elided at the combiner identity.
- Loop bodies with `break`, `continue` beyond the single leading-guard form,
  early `return`, `throw`, nested unsupported loops, accumulator aliasing/escape,
  no-identity/nullable reduces, non-commutative accumulations, Map builders, Set
  `.delete`/`.clear`, and multi-collection loops remain rejected.
- The targeted `for-of loop is not a recognized build-list comprehension`
  bucket moves down in the post-M3 corpus diagnostic (movement driven primarily
  by the list-build widening, the dominant family).
- Existing `expressions-for-of-comprehension.ts` behavior remains
  byte-identical unless M3 explicitly owns a more general canonical output.

**Why this is a safe pause point**:
M3 builds on an already-verified local builder target. It admits more loop
surface forms only when they lower to that target, and it leaves general loop
control flow to the completed general-loop SSA machinery or future milestones.

**Unlocks**:
Switch/statement-position cleanup can proceed with fewer loop-shaped false
positives in the diagnostic output.

**Operator Actions Before Next Milestone**:
- Re-run the corpus diagnostic and inspect the remaining switch buckets:
  `switch default must end with return`, `switch case must end with return`,
  `switch case label must be a literal`, and
  `statement is not supported by unified SSA body lowering: SwitchStatement`.
- Decide whether M4 should admit a bounded switch value shape or remain a
  measurement-only milestone if switch residuals are dominated by fall-through
  and non-literal labels.

**Open Questions**:
- Resolved by the M3 gameplan: for-of Set builders are in scope and lower to
  membership equivalence over a `some` comprehension, not list equality.
- Resolved by the M3 gameplan: commutative identity-bearing scalar folds
  (`+`, `*`, `&&`, `||`, `count++`) are in M3, lowering to the Shape-C
  `combOP over each` reduce. No-identity / nullable reduces (conjoin/disjoin),
  which need an option-typed fold, are deferred to a later milestone.
- Resolved by the M3 gameplan: forEach build-list callbacks and callback
  `return` semantics are deferred. M3 is scoped to `for-of` statements.

---

### Milestone 4: switch-body-completeness

**Definition of Done**:
- Switch bodies in value position support every bounded no-fall-through shape
  with a clear `cond` target: terminal return, block-return, and nested pure
  block-return case/default bodies.
- Switch statements in supported pure-body preludes and supported unified SSA
  body positions lower through the same switch-to-cond contract where the
  semantics are equivalent.
- Non-literal labels, default-not-last, fall-through-by-empty-case,
  break-only/throw-only value cases, and side-effectful discriminants remain
  rejected with existing or sharper diagnostics.
- The switch case/default residual buckets move down in the post-M4 corpus
  diagnostic.

**Why this is a safe pause point**:
The milestone extends the existing switch-to-cond contract without changing
the rejection policy for semantically ambiguous switch forms. Stopping here
leaves the translator with a stronger value-position switch lowering but no
unsound fall-through modeling.

**Unlocks**:
Final body-completeness cleanup can target the residual structural buckets
that remain after nested blocks, builders, loops, and switches have been
removed from the top ranking.

**Operator Actions Before Next Milestone**:
- Re-run the corpus diagnostic and classify the remaining structural buckets
  into: body-completeness owned, other-workstream owned, and intentionally
  unsupported.
- If body-completeness-owned residuals are no longer among the top buckets,
  skip M5 and mark the workstream complete after reconciling the acceptance
  suite.

---

### Milestone 5: residual-body-diagnostic-cleanup

**Definition of Done**:
- Every remaining structural body-lowering bucket above a small threshold
  chosen during M4 planning is either implemented, reclassified to another
  workstream, or documented as intentionally unsupported with a precise
  diagnostic.
- `tools/ts2pant/docs/transformations.md` and `tools/ts2pant/AGENTS.md`
  describe all body transformations added by this workstream.
- `tools/ts2pant/scripts/corpus-diag.mts` output clearly separates
  body-completeness residuals from DU, guard/foreign, opaque/type-mapping, and
  other-workstream residuals.
- The final corpus diagnostic no longer has a structural body-lowering bucket
  above the threshold that lacks an owner.

**Why this is a safe pause point**:
This is the terminal reconciliation milestone. It does not need to close every
possible TypeScript body shape; it needs to ensure every remaining rejection is
either small, intentional, or assigned to the right future workstream.

**Unlocks**:
The body-completeness workstream can be closed, and future dogfood expansion
can focus on DU registration, foreign method synthesis, guard effects, and
type/opaque precision rather than structural body sequencing.

## Dependency Graph

```text
1 (body-lowering-completeness-rd2) -> []
2 (local-collection-builder-sequencing) -> [1]
3 (iteration-builder-completeness) -> [2]
4 (switch-body-completeness) -> [3]
5 (residual-body-diagnostic-cleanup) -> [4]
```

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| What post-M1 bucket threshold defines "workstream complete"? | Suggested threshold: no body-completeness-owned normalized bucket above 10 functions, but this should be confirmed after M1/M2 because bucket normalization may split or merge shapes. | M4 operator review |
| ~~Should scalar local folds be part of `iteration-builder-completeness`?~~ | RESOLVED (M3 gameplan): yes for commutative identity-bearing folds (`+`, `*`, `&&`, `||`, `count++`) via the Shape-C `combOP over each` reduce; no-identity/nullable reduces (conjoin/disjoin) deferred to a later option-typed-fold milestone. The post-M2 survey found only 3 scalar folds in the for-of bucket, all no-identity. | Resolved |
| ~~Is `forEach` callback `return` behavior in scope?~~ | RESOLVED (M3 gameplan): deferred. No `forEach` instances in the for-of bucket; `forEach` routes through the mutating Shape A/B machinery, and callback `return` only exits the callback. Revisit once the for-of widening is the established target. | Resolved |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Scope is structural body lowering only. | DU registration, guard/foreign-call admission, opaque narrowing, and non-discriminated-union field access have their own workstreams or follow-ons. Mixing them here would make bucket movement hard to interpret and milestones too broad. |
| Measurement gates milestone boundaries. | Each milestone ends with a corpus diagnostic run. Later milestones are intentionally thinner because the residual ranking changes after each landing. |
| Local accumulator sequencing is not part of M1. | It is stateful local mutation with a pure function boundary. It needs a collection-builder model, not a larger pure block-return helper. |
| Conservative refusal remains the steering policy. | The goal is not arbitrary JS support; it is accepting idiomatic, unambiguous TS only when Pantagruel has a faithful target. |
| M2 ordered list builders use finite positional assertions. | Pantagruel has no list literal, and comprehensions over synthetic finite tuples would introduce unnecessary helper domains. Cardinality plus 1-based list-application equations preserves push order while constraining the declared return rule directly. |
| M2 includes Set `.add` builders but defers Map builders. | Set lowers to the existing `[T]` membership encoding, so membership equivalence is a faithful target. Map construction touches the guarded Stage A/Stage B partial-rule pair and needs a Map-specific milestone rather than being bundled into local builder sequencing. |
| M3's first loop family is the for-of list-build widening (loop-local consts + compound/nested guards). | The post-M2 survey of the 52 for-of failures found list-push-extended dominant (30); the M2 operator action mandated choosing the largest family that reuses the established builder target, and this reuses the for-of `each` comprehension with no new target-language construct. |
| M3 includes for-of Set builders, lowered to membership-equivalence over a `some` comprehension, not list equality. | A Set deduplicates and is order-agnostic; equating it to `each n in src | proj n` over-constrains the result when two distinct elements project equal. `all e: T | e in (f args) <-> some n in src, guards | e = proj n` is the faithful, checker-supported encoding and matches M2's Set-as-list membership philosophy. |
| M3 includes commutative identity-bearing scalar folds but defers no-identity reduces and forEach. | `+`/`*`/`&&`/`||` (and `count++`) form monoids whose unit is the elided init, mapping cleanly onto the Shape-C `combOP over each` reduce. conjoin/disjoin are null-seeded no-identity reduces needing an option-typed fold target; forEach routes through the mutating Shape A/B path. Both are deferred to keep M3 a clean, atomic extension of the comprehension target. |

## Definition of Done (Acceptance Suite)

The terminal acceptance suite is command-based because ts2pant is a local
translator. Assertions are black-box over fixture translation, emitted
Pantagruel text, snapshot output, and the corpus diagnostic; a verifier does
not need to read source to decide pass/fail.

- **DoD-1 — Nested pure block bodies translate**
  - **Assert**: A fixture function whose early-return arm contains a nested
    supported pure block, and a fixture function whose terminal if/else branch
    contains a nested supported pure block, emit Pantagruel `cond` values
    rather than `> UNSUPPORTED: if-with-return block must contain only const
    bindings followed by a return`.
  - **Verify by** `cmd`: Run `just ts2pant-test-unit` and inspect the
    `expressions-body-lowering-rd2.ts` constructs snapshot entries.
  - **Expected**: The targeted fixture snapshots contain equations with `cond`
    and no matching UNSUPPORTED line for the targeted functions.
  - **Traces to**: Milestone 1 — `tools/ts2pant/src/translate-body.ts`
    nested pure block-return lowering.

- **DoD-2 — Record-return conditionals lower fieldwise**
  - **Assert**: A record-return fixture with conditional arms returning the
    same object-literal field set emits one equation per record field with
    conditional RHS values.
  - **Verify by** `cmd`: Run `just ts2pant-test-unit` and inspect the
    `expressions-body-lowering-rd2.ts` snapshot entry for the record-return
    conditional fixture.
  - **Expected**: The snapshot contains field equations and no
    `record return combined with early-return arms or if/else branches`
    UNSUPPORTED line for the targeted fixture.
  - **Traces to**: Milestone 1 — `tools/ts2pant/src/translate-body.ts` and
    `tools/ts2pant/src/translate-record.ts`.

- **DoD-3 — Local collection builders translate without general expression-statement admission**
  - **Assert**: Dedicated local-builder fixtures cover an ordered list builder
    (`const xs = []; xs.push(...); return xs`) and a Set `.add` builder
    (`const s = new Set<T>(); s.add(...); return s`), while unrecognized
    expression statements, alias/escape cases, unknown mutating calls, and Map
    builders still emit UNSUPPORTED.
  - **Verify by** `cmd`: Run `just ts2pant-test-unit` and inspect the local
    collection builder tests in
    `tools/ts2pant/tests/local-collection-builder.test.mts`.
  - **Expected**: Positive list-builder fixtures typecheck and emit
    cardinality plus positional index assertions; positive Set-builder
    fixtures typecheck and emit membership-equivalence assertions; negative
    expression-statement and Map-builder fixtures retain UNSUPPORTED
    diagnostics.
  - **Traces to**: Milestone 2 — local collection builder lowering in
    `tools/ts2pant/src/translate-body.ts`.

- **DoD-4 — Builder-backed for-of variants and scalar folds translate**
  - **Assert**: In `tools/ts2pant/tests/iteration-builder.test.mts` over
    `tools/ts2pant/tests/fixtures/constructs/expressions-iteration-builder.ts`:
    (a) a for-of list builder with a loop-local const projection and one with a
    compound/nested guard emit an `each binder in src, guards | proj`
    comprehension; (b) a for-of Set builder emits
    `all e: T | e in (f args) <-> some n in src, guards | e = proj n`;
    (c) a scalar fold (`let total = 0; for (...) total += f(x); return total`)
    emits `(f args) = (+ over each n in src[, g] | f n)` (init elided), and an
    `&&` fold emits `and over each`. None emit
    `for-of loop is not a recognized build-list comprehension`.
  - **Verify by** `cmd`: Run `just ts2pant-test-unit` (and
    `just ts2pant-test-integration` for entailment) and inspect the
    iteration-builder test assertions.
  - **Expected**: Positive list/Set/scalar-fold fixtures typecheck and emit the
    forms above; negative fixtures (break, continue-beyond-guard, early return,
    throw, nested loop, accumulator alias/escape, Map builder, Set
    `.delete`/`.clear`, no-identity reduce, non-commutative accumulation) remain
    unsupported with precise diagnostics.
  - **Traces to**: Milestone 3 — iteration builder lowering in
    `tools/ts2pant/src/translate-body.ts` (`recognizeForOfPush`/`recognizeForOfPushBody`,
    `tryBuildForOfSetComprehensionReturn`, `tryBuildForOfScalarFoldReturn`).

- **DoD-5 — Switch value bodies preserve conservative switch semantics**
  - **Assert**: Supported no-fall-through switch value fixtures emit `cond`
    output, while fall-through, non-literal labels, side-effectful
    discriminants, and default-not-last cases remain unsupported.
  - **Verify by** `cmd`: Run `just ts2pant-test-unit` and inspect switch body
    fixture snapshots.
  - **Expected**: Positive switch fixtures contain `cond`; negative switch
    fixtures retain their precise UNSUPPORTED diagnostics.
  - **Traces to**: Milestone 4 — switch body lowering in
    `tools/ts2pant/src/ir1-build.ts`.

- **DoD-6 — Body-completeness residuals are measured and owned**
  - **Assert**: The final corpus diagnostic has no structural
    body-completeness-owned bucket above the workstream threshold that lacks an
    owner or intentional-unsupported note.
  - **Verify by** `cmd`: Run
    `NODE_OPTIONS=--max-old-space-size=6144 npx tsx tools/ts2pant/scripts/corpus-diag.mts`
    and compare the top buckets against the owner table documented by M5.
  - **Expected**: Every structural body bucket above threshold is either
    absent, below threshold, implemented, or explicitly assigned/documented;
    DU, guard/foreign, and type/opaque buckets are not counted against this
    workstream.
  - **Traces to**: Milestone 5 — `tools/ts2pant/scripts/corpus-diag.mts` and
    body residual documentation.

- **DoD-7 — Workspace-level verification passes**
  - **Assert**: The final body-completeness state passes the ts2pant unit and
    integration suites through the workspace-level task runner.
  - **Verify by** `cmd`: Run `just ts2pant-test`.
  - **Expected**: Exit code 0.
  - **Traces to**: Milestones 1-5 — all body-completeness translator changes.
