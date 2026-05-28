# Discriminated-Union Entailment & Narrowing Survey (DU M2)

**Workstream:** ts2pant Discriminated-Union Handling — Milestone 2
(`du-entailment-narrowing-survey`). **Date:** 2026-05-28.
**Method:** read-only probe over the M1 tagged encoding (`du-tagged-encoding`,
PRs #290 + #291) plus a corpus inventory of DU-relevant narrowing sites. No
translator behavior changed.

## Purpose

M1 introduced the guarded tagged-union encoding for structurally-detected
discriminated unions (`shape--kind s: Shape => String` plus per-variant
field rules guarded by `shape--kind s = "<literal>"`). The workstream's
operator-action gate for M3 is twofold:

1. **Does the M1 encoding entail under z3 once a discriminant guard is
   assumed?** — if not, M3 narrowing is moot until the encoding is fixed.
2. **Are there enough real intra-function narrowing sites to justify M3?**

This survey answers both, names the interface M3 will consume from the
guard-analysis workstream's M3a (`du-discriminant-narrowing-layer`), and
flags one encoding question that M3 will surface.

## (1) Encoding entailment under z3 — the headline

Authored probes against the M1-shaped encoding (`Shape` with two variants:
`circle`/`square`, shared field `shared`):

| Probe | Result | What it tells us |
|---|---|---|
| **Variant-field rule applied in a body whose head asserts the guard** (`f x: Shape, shape--kind x = "circle" => Int` with body `f x = shape--r x`) | ✅ `Entailed` | The guarded rule discharges cleanly when the caller carries the guard. **This is the post-narrowing target shape.** |
| **Shared-field rule with `or`-guard over all variant literals** (`shape--shared s: Shape, shape--kind s = "circle" or shape--kind s = "square" => String`) | ✅ `Entailed` | Multi-variant guards compose. |
| **Body equation with an undischarged guard** (`f x: Shape => Int` with body `f x = shape--r x`, no guard on the head) | ✅ `Entailed` (equation level) | McCarthy-style partial-function semantics: the equation is universally satisfiable; the guard is a *partiality* concern, not an equational obligation. The encoding is sound — un-narrowed variant-field reads do not produce a wrong fact, they just fail to discharge any obligation that requires the guard to actually hold. |
| **Negated disjointness** (`some s: Shape | shape--kind s = "circle" and shape--kind s = "square"`) | ✅ z3 reports `contradictory` | **Disjointness is free** from function-value semantics: `shape--kind` returns a single value, so two literals can't both hold. **No explicit disjointness invariant needs to be emitted.** |
| **Totality** (`all s: Shape | shape--kind s = "circle" or shape--kind s = "square"`, with `--check`) | ⚠️ Reported only as `jointly satisfiable`, not entailed | z3 will admit a model where the discriminant takes an unknown literal value (e.g., `"triangle"`). **Totality is NOT free** under EUF — an explicit invariant would be required to prove exhaustive `cond` dispatch over all variants. See §5. |

**Verdict for question (1): yes — the M1 encoding entails when the guard is
discharged.** The probe shape (`f x: Shape, shape--kind x = "circle" => Int`
with body `f x = shape--r x`) is exactly the post-narrowing target M3 will
synthesize: the discriminant-equality fact lives in the assumption environment
on entering the narrowed branch, the variant-field rule's precondition is
discharged from that environment, and z3 closes the obligation.

## (2) Corpus inventory

### Where M1's encoding lives today

| Layer | DU types encoded | Status |
|---|---|---|
| **Fixtures** (`tests/fixtures/constructs/`) | `Shape` (2 variants, shared field) | All 3 reader functions translate + type-check via `constructs.test.mts.snapshot`; the `ambiguous-owner` non-DU stays on the existing refusal path. |
| **Dogfood** (`tests/integration/dogfood.test.mts.snapshot`) | `DiscriminantLiteral` (3 string-kind variants over `"string"/"number"/"boolean"`), `FieldOwnerResolution` (3 string-kind variants `"resolved"/"none"/"ambiguous"`), `KeyLiteralRec` | Encoded in the snapshot; the `DiscriminantLiteral.value` field carries a heterogeneous variant type (`String + Int + Bool`) under an `or`-guard, which is the headline narrowing case for `switch (lit.kind)` walkers. |
| **Self-translation (not yet dogfooded)** | `IR1Expr`, `IR1Stmt`, `IR1Binding`, `OpaqueExpr`, … (~16 type defs in `ir1.ts` alone) | These would all be encoded by M1's structural detector. Their narrowing consumers (below) are the dense pattern. |

### Narrowing-pattern inventory (DU-specific subset)

Refining the workstream-wide counts from
[guard-narrowing-survey.md](./guard-narrowing-survey.md) to the DU-relevant
subset (excluding `ts.SyntaxKind` enum compares, which are TS-internal and not
structural DUs):

| Pattern | Workstream-wide count | DU-relevant subset | Notes |
|---|---|---|---|
| `switch (x.kind)` | 58 | **~37** | Switches whose `case` labels are bare string literals (IR1Expr/IR1Stmt/binding walkers, etc.). Remainder (~21) are over `ts.SyntaxKind` and excluded. |
| `if (x.kind === "<lit>")` | 169 | **160** | Counted `.kind === "<bare-string>"` directly; 72 additional `.kind === ts.SyntaxKind.X` excluded. |
| Discriminant in early-return guard | (in 169 above) | (subset) | Same lowering target as the `if`. |

**Reading:** discriminant narrowing is the densest real pattern in ts2pant's
own source (≥197 sites combined), heavily dominated by walkers over the
project's own IR1 union types. The fixture corpus is thin (1 DU type) — the
real consumer is self-translation once these walker functions are dogfooded.

## (3) Where un-narrowed obligations actually block

Three concrete examples from the dogfood corpus and fixtures show what M3 must
discharge:

1. **`literalExpr` in `translate-types.ts:567`** —
   `switch (lit.kind) { case "string": return ast.litString(lit.value); ... }`
   Inside the `"string"` arm, TS narrows `lit.value: string`, but the M1
   encoding gives `lit.value: String + Int + Bool` (the shared-field
   `or`-guard). The variant-specific argument-type obligation on
   `ast.litString` cannot discharge without narrowing.
2. **`FieldOwnerResolution` consumers in `ir1-build.ts:1584`/`:1587`** —
   `if (r.kind === "resolved") { ... r.owner ... }` — `r.owner` is only
   defined on the `resolved` variant; the M1 rule
   `field-owner-resolution--owner r: FieldOwnerResolution, field-owner-resolution--kind r = "resolved" => String`
   needs the `kind === "resolved"` fact to discharge.
3. **The Shape fixture's `readVariantField`** — translates today but the
   body equation `read-variant-field x = shape--r x` is entailed at the
   equation level only; any downstream property that depends on `shape--r`'s
   guard being satisfied would not discharge without narrowing. The fixture
   is intentionally minimal and demonstrates the encoding is sound, not that
   narrowing is unnecessary.

## (4) M3 design — the interface from guard-analysis M3a

The guard-analysis workstream's M2 survey already resolved the design
([guard-narrowing-survey.md §M3 design proposal](./guard-narrowing-survey.md)):
a **function-scoped assumption environment** that pushes a fact on entering
a narrowed branch and pops on exit. M3a will deliver the *discriminant fact*
keyed structurally as `<property-access> === <literal>`.

DU M3 consumes that layer with this contract:

- **Fact shape M3a emits, M3 reads.** A discriminant fact has the form
  `<receiver>.<property-name> === <literal>` where `<literal>` is a string,
  number, or boolean literal. M3a keys this structurally — never on `.kind`
  by name — so the DU narrowing path inherits the project's no-field-name-
  special-casing constraint for free.
- **Where DU lowering plugs in.** When lowering emits a variant-field rule
  application whose head precondition is `<domain>--<discriminant> s = "<lit>"`,
  it queries the assumption environment for a fact whose receiver matches the
  rule's receiver argument and whose property matches the encoded
  discriminant (resolved via the existing `DiscriminatedUnionSynthEntry`
  metadata — `discriminant` field). If the fact's literal matches the
  precondition's literal, the precondition is marked discharged; if absent
  or mismatched, it remains undischarged (sound-by-guard).
- **Switch lowering.** `switch (x.kind) { case "circle": ... }` lowers to a
  `cond`-dispatch; the `"circle"` arm pushes `x.kind === "circle"` into the
  environment for the arm's body. Default/`true =>` arm pushes the
  *negation* of all matched literals — relevant to (5) below.
- **Intra-function only.** Cross-call discharge (a helper that returns
  `lit.kind === "string"` and is called as a predicate) is out of scope
  for M3, matching both the DU workstream and the guard-analysis
  workstream's standing decisions.

No DU-specific narrowing infrastructure is needed; M3 is a consumer, not a
builder.

## (5) Open encoding question for M3 — totality

The M1 encoding emits no totality invariant. Practical impact:

- `if (x.kind === "circle") { ... } else { ... x.s ... }` — the `else`
  branch's reader `x.s` needs the guard `x.kind === "square"` to discharge.
  Under EUF without a totality invariant, the assumption-environment fact
  for the `else` is `~(x.kind === "circle")`, which does **not** entail
  `x.kind === "square"` — z3 can model `x.kind = "triangle"`.
- A `cond` dispatch over all known variants (`cond x.kind === "circle" =>
  ..., x.kind === "square" => ..., true => ...`) succeeds with a fallback
  arm but has no exhaustiveness proof without totality.

**Two viable M3 designs to weigh** (decide during M3's gameplan):

1. **Emit a totality invariant** at DU encoding time:
   `all s: Shape | shape--kind s = "circle" or shape--kind s = "square".`
   Cheap, sound (the TS union type guarantees it), and gives the `else`
   branch full discharge for free. Adds one universal per DU domain to
   every emitted document.
2. **Push the negated literal as the else-branch fact** in M3a and let
   downstream obligations live with non-exhaustive dispatch. Simpler
   encoding, but the `else`-branch discharge case stays unprovable until
   a totality invariant is added later.

**Recommendation: option 1 — emit the totality invariant.** It is sound
(TS's union type guarantees the cover), cheap (one universal), and removes
a class of `else`-branch obligations from M3's surface. The disjointness
half is already free from function semantics; the totality half should be
parallel. **Resolve during M3 gameplan.**

## Verdict

**Proceed to M3.** Both gating criteria pass:

- ✅ The M1 encoding entails when the discriminant guard is discharged
  (probe 1; the post-narrowing target shape compiles cleanly under z3).
- ✅ Real intra-function narrowing sites exist in quantity: ~37 DU-relevant
  switches + 160 `.kind === "<literal>"` ifs, dominated by ts2pant's own
  IR walkers and concentrated in the dogfood corpus once
  `literalExpr`/`FieldOwnerResolution`-shaped functions are dogfooded.

**M3 scope confirmation:** local intra-function narrowing only; consumes the
guard-analysis M3a `<property-access> === <literal>` fact unchanged;
narrowing the `else` branch hinges on the totality-invariant decision in
M3's gameplan. The any/unknown-opaque workstream's "cross-function-call
narrowing not trusted by default" constraint stands.

**Dependency status:** guard-analysis M3a is **not yet planned** as of this
report — DU M3 cannot start until it lands or is gameplanned with a stable
fact-key contract. M2 has produced everything it can; the next move on the
DU critical path is to schedule guard-analysis M3a (`du-discriminant-
narrowing-layer`).

## Caveats

- **Fixture corpus is thin (1 DU).** The encoding-entailment probes are
  authored directly against the M1-shape; the fixture acts as a sanity
  check that the pipeline emits that shape, not as a corpus measurement.
  Confidence is high because the encoding is mechanically synthesized from
  detection, not per-fixture-tuned.
- **Heterogeneous shared-field type** (`String + Int + Bool` for
  `DiscriminantLiteral.value`) — sound today, but `ast.litString(lit.value)`
  on the narrowed `value: String` is the obligation M3 must discharge. The
  argument-type rule will need to see the variant-narrowed type, not just
  the discriminant guard. This is M3 territory (the fact propagates; the
  type-refinement happens at the type-checker layer, not the encoding).
- **Multi-discriminant tie-break** (sorted-field-name pick, per the
  workstream's M1 decision) means a union with multiple qualifying fields
  encodes only one of them as *the* discriminant — non-chosen qualifying
  fields become per-variant guarded rules under the chosen discriminant's
  guard. M3 narrows on the chosen discriminant's predicate; secondary
  qualifying fields would not narrow (acceptable — the workstream's
  decision is deterministic by design).
- **Nested DUs / DUs inside other synth shapes** are M4 cutover scope and
  not measured here.
