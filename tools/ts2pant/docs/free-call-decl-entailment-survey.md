# Free-Call-Decl Entailment Survey (Milestone 3)

**Workstream:** ts2pant Free-Call-Decl Synthesis — Milestone 3 (`entailment-survey`).
**Date:** 2026-05-27. **Solver:** z3 4.15.4 via `pant --check` (default bound 3).
**Status:** read-only measurement; no translator behavior changed.

## Purpose

Milestones 1–2 turned every structurally-accepted free/built-in call from
`> UNSUPPORTED: free-call-decl` into a *declared* Pantagruel reference — local
EUF heads for unknown user functions (M1) and qualified shared-module references
for stdlib calls (M2). Those are **type-checking** wins. This survey measures the
separate question — **entailment**: do the `@pant` assertions that involve a
free/built-in call actually *prove* under z3, or do they merely parse/type-check?
The answer drives the M4 go/no-go: M4 (`targeted-sound-axioms`) was gated on
finding **≥5 distinct assertions blocked solely on a soundly-modelable axiom for
a shared js-stdlib rule**.

## Method

Every `@pant`-bearing fixture function across `tools/ts2pant/tests/fixtures`
(13 functions; `src/` dogfood files carry no `@pant` annotations) was built
through the standard pipeline, emitted, and run through `pant --check`. Each
function is flagged for whether its assertion involves a free or built-in call.

## Results

| # | Fixture > fn | Involves call? | Call kind | `--check` |
|---|---|---|---|---|
| 1 | validate-helper.ts > deposit | **yes** | free user fns as guards (`validateAmount`, `requireNonNegativeBalance`) | **entails** |
| 2 | assert-guard.ts > deposit | **yes** | free `assert()` guard | **entails** |
| 3 | apply-fee.ts > applyFee | no | field mutation | entails |
| 4 | deposit.ts > deposit | no | field mutation | entails |
| 5 | max.ts > larger | no | own rule | entails |
| 6 | annotations.ts > add | no | arithmetic | entails |
| 7 | annotations.ts > sum | no | arithmetic | **not entailed** (see below) |
| 8 | annotations.ts > rangeCheck | no | comparison | entails |
| 9 | annotations.ts > deposited | no | identity | entails |
| 10 | expressions-map-params.ts > sumAt | **yes** | `Map.get` → synthesized Map encoding | **entails** |
| 11 | expressions-set.ts > both | **yes** | `Set.has` → `in` membership | **entails** |
| 12 | expressions-map.ts > congruence | **yes** | `Map.has` → membership predicate | **entails** |

### The five call-involving assertions

All five **entail**. None of them is closed by a js-stdlib EUF axiom; each entails
for a reason that already exists in the translator:

- **#1, #2 (guarded free calls).** The free user-function / `assert` calls are
  guards; the post-state proof rests on the action/guard modeling, not on any
  property of the called function. The opaque EUF head M1 synthesizes is not even
  load-bearing for the assertion.
- **#10 (`Map.get`), #11 (`Set.has`), #12 (`Map.has`).** These entail via the
  **structured collection encoding** — McCarthy select/store for `Map`
  (`entries`/`entriesKey` guarded rules) and `x in xs` membership for `Set`. That
  encoding is *not* one of the M2 dispatch EUF heads; it predates this workstream
  and is strictly higher-fidelity than an opaque head would be. (This is exactly
  why M2 deliberately left `Set.has` on `in` and did not create `JS_SET`.)

### The one non-entailment (#7) is not a stdlib gap

`annotations.ts > sum` asserts `sum a b >= a` for `sum a b = a + b`. This is false
for unconstrained `Int` (`b` may be negative) — a property-of-arithmetic
non-entailment, not a missing-axiom-on-a-stdlib-rule. No call is involved.

## Key finding: the M2 dispatch heads are unexercised

**No `@pant` assertion anywhere in the corpus references a shared js-stdlib EUF
rule** (`JS_MATH::*`, `JS_STRING::*`, `JS_ARRAY::from`, `JS_MAP::{values,entries,keys}`).
The fixtures that *use* those dispatched built-ins — `constMathMax`,
`constChainedPure`, `letMathMax`, `constStringMethod`, `methodCall`,
`constReplaceLiteral`, `constArrayFromMap`, `constMapEntries`, `chainedArrayCount`
— are **type-check-only fixtures with no assertion**. M2 made them *declared and
type-checking*; nobody has yet written an entailment goal over them.

## Per-rule blocked-assertion breakdown

| Shared js-stdlib rule | Assertions blocked solely on a sound axiom |
|---|---|
| `JS_MATH::max-of` / `min-of` / `abs` | 0 |
| `JS_STRING::*` (incl. `replace`) | 0 |
| `JS_ARRAY::from` | 0 |
| `JS_MAP::values` / `entries` / `keys` | 0 |
| **Total** | **0** |

## Verdict: do **not** proceed to Milestone 4

The M4 gating criterion (≥5 assertions blocked solely on a soundly-modelable
stdlib axiom) is met by **0** assertions — far below threshold. This is the
workstream's documented **abort condition**: the entailment gaps that a structural
axiom would close are not present, because (a) the dispatched-built-in fixtures
carry no assertions, and (b) the collection assertions that exist already entail
through the structured Map/Set encoding rather than an EUF head.

**The free-call-decl workstream is therefore complete at M2.** The end-to-end
story holds: structurally accepted → declared → type-checks, and *every*
call-involving assertion that exists today already entails. The "where it matters,
provable" clause has no unmet demand.

## Re-open trigger (for the record)

Revisit a *targeted* axiom (not a speculative milestone) only if a future `@pant`
assertion is written that is blocked **solely** on a sound structural property of a
dispatched head — e.g. `#(Array.from(m.values())) = <map cardinality>` (Map-size /
values-length) or `JS_ARRAY::from` length non-negativity. At that point add the
single axiom to the owning `samples/js-stdlib/*.pant` module with a soundness
justification, guarding before/after with this survey's entailment set. String
semantics (case folding, locale, surrogates) remain out of scope per the
`JS_STRING.pant` refusal.
