# Workstream: ts2pant Imperative IR (IRSC-Faithful Normalization Layer)

## Vision

Build a TypeScript-faithful imperative intermediate representation layer
(Layer 1) between the TS AST and ts2pant's expression IR (Layer 2,
`IRExpr` in `src/ir.ts`). Normalization passes against Layer 1 collapse
operationally-equivalent TS surface forms — increment spellings, conditional
families, iteration families — into a small canonical vocabulary. Lowering
passes run against ONE canonical input shape per construct, so adding a new
TS spelling never again requires extending a recognizer.

Reference: Vekris/Cosman/Jhala, *Refinement Types for TypeScript* (PLDI
2016, [arxiv:1604.02480](https://arxiv.org/pdf/1604.02480)). Their FRSC→IRSC
SSA transformation is the precedent. ts2pant cited it but stopped short of
building it. This workstream commits to it.

**Two lowering targets, one IR.** L1 is shared, but its downstream targets
differ by program position:

- **Expression position** (pure path, value returns): L1 expression →
  L2 `IRExpr` → `OpaqueExpr`. Two-layer pipeline; L2 is the
  Pant-shaped expression IR.
- **Statement position** (mutating bodies, frame conditions): L1
  statement → `PropResult[]` directly via a single fold (`lowerL1Body`)
  that threads `SymbolicState`. No L2 statement vocabulary; the mutating
  output is a list of equations, not a unit-returning expression.

This asymmetry is intentional and was hard-won — see § "Architectural
Lessons" below.

## Current State (post-M5)

M1 (conditionals), M2 (assign + μ-search), M3 (iteration + mutation),
M4 (equality + nullish), and M5 (property access) have landed. Layer 1
IR is the canonical input shape for every mutating-body construct
ts2pant supports, plus the conditional / assign / iteration
expression forms, plus the equality / nullish equivalence class,
plus the property-access equivalence class.

**What's on Layer 1**:

- Expression forms: `var`, `lit`, `binop`, `unop`, `app`, `member`
  (M5 canonical property-access form, pre-qualified at build time),
  `cond`, `is-nullish` (M4 canonical Bool null/undefined test),
  `from-l2` (transitional adapter for sub-expressions outside the
  current milestone's normalization concern).
- Statement forms: `block`, `let`, `assign`, `cond-stmt`, `foreach`
  (with optional `body` and `foldLeaves` for Shape A + Shape B), `for`
  (declared, unused), `while` (μ-search only), `return`, `throw`
  (declared, unused), `expr-stmt` (declared, unused), `map-effect`,
  `set-effect`.

**What's on Layer 2** (post-M3): `IRExpr` only. The L2 statement
vocabulary (`IRStmt` with `Write`, `LetIf`, `Seq`, `Assert`) and its
companion `src/ir-subst.ts` were the speculative target of the
parallel-build PRs (#134/#135/#137) that got closed; PR #138 deleted
them once M3 confirmed nothing built or lowered them.

**What's still on the legacy path** (pre-M6):

- Pure-path `.reduce` chain fusion — `translateReduceCall` builds an
  L2 `eachComb` directly. Architecturally separate from M3's
  mutating-body foreach.
- `IRWrap` escape hatch in L2 — survives until M6.
- `from-l2` adapter — shrunk at M5 (the four documented property-
  access wrap sites consolidated into one `buildL1SubExpr` fallback
  that fires only for non-property sub-expressions); deleted at M6.

**Combined-shape recognizer precedent (M4 Patch 5).** The functor-lift
recognizer (`tryRecognizeFunctorLift` in `ir1-build.ts`) is the first
combined-shape recognizer crossing milestone boundaries — it matches
M1 Cond + M4 IsNullish in a single rewrite. The motivation is
load-bearing: Pant has no list literal, so the alternative
cardinality-dispatch lowering for null-guarded list-lifted conditionals
has no expressible target. Future milestones may need to follow this
pattern when Pant's expression vocabulary forces a multi-form match;
see "Architectural Lesson 5" below.

## Key Challenges

- **Equivalences that aren't equivalences.** Several surface forms look the
  same but aren't: `==` vs `===` with non-Bool operands; `switch` with
  fall-through vs if-chain; `.forEach(cb)` vs `for-of` when `cb` captures
  `this` or returns from an enclosing scope; `&&`/`||` as expressions on
  non-Bool values. The normalizer must refuse to canonicalize when it
  can't prove equivalence. Conservative-refusal logic is itself nontrivial.
- **Vocabulary lock-in at M1.** Layer 1 forms were decided up front.
  Forms can be *added* later (e.g., `IsNullish` at M4, `IR1FoldLeaf`
  at M3) but not changed. Mid-stream re-litigation is the failure
  mode the workstream is meant to prevent.
- **Iteration shapes mutation.** `Foreach` with a *statement* body
  (admitting `Assign`) is the canonical iteration form. Shape A (uniform
  iterator write), Shape B (accumulator fold), and `.reduce` desugaring
  all require statement-body iteration. This forced the former Stages
  9–11 mutating-path work to land *with* iteration normalization in M3.
- **Hard rule per equivalence class.** During migration, an entire
  equivalence class moves to the new path in one milestone — no
  half-migrated classes co-existing with legacy recognizers. Faster to
  ship, slower per milestone, prevents the cross-talk hazard the IR was
  meant to retire.
- **Opaque AST constraint unchanged.** All normalization happens on Layer 1
  before any `OpaqueExpr` exists. No syntactic peeking on `OpaqueExpr` from
  any normalization or lowering pass.

## Architectural Lessons

These lessons are written in retrospect after M3 absorbed three closed PRs
worth of false starts. They are the load-bearing architectural commitments
for any future milestone in this workstream.

### Lesson 1: parallel L2 statement vocabulary is a compatibility shim, not architecture

**The false start (PRs #134/#135/#137):** the obvious-looking next move
after M2 was to build out L2's `IRStmt` vocabulary (`Write`, `LetIf`,
`Seq`, `Assert`) as the lowering target for L1 mutating-body statements.
That was the framing inherited from the original Stages 9–11 plan: "L1
→ L2 → emit" symmetric with the expression pipeline.

**Why it failed:** the L2 statement forms turned out to mirror
`symbolicExecute`'s control-flow structure line-for-line. Building them
was zero abstraction value — the L2 layer didn't *normalize* anything
relative to L1 (the normalization had already happened at L1 build);
it just renamed the legacy mutation logic into a new vocabulary,
producing two parallel paths that did the same work. PRs #134/#135/#137
each added more parallel infrastructure without retiring any legacy
code, because retirement required *all* mutating constructs to land on
the L2 path simultaneously (the hard rule).

**The fix:** mutating-body lowering bypasses L2 entirely. `lowerL1Body`
threads the existing `SymbolicState` directly into `PropResult[]`. The
existing mutation primitives (`putWrite`, `mergeOverrides`,
`installMapWrite`, `installSetWrite`) — already debugged, already
producing correct frame conditions — are reused as-is.

**Generalizable principle:** if a proposed IR layer's forms map 1-to-1
to the legacy code's control-flow structure, you're building a shim,
not an abstraction. Either find a canonical form that collapses
multiple legacy forms into one, or skip the layer.

### Lesson 2: rip-out-first beats parallel-build for retiring legacy

**The false start:** all three closed PRs followed parallel-build
discipline — build the new path, run both paths under a feature flag,
verify byte-equality, then cut over. This is sound for *adding*
capability but not for *retiring* it: the hard-rule constraint forbids
half-migrated classes, so the parallel-build phase has to construct the
*entire* L2 vocabulary before the cutover lands. That made each PR
balloon while the test suite still ran the legacy path.

**The fix:** rip the legacy code out first (commit `987eef3` deleted
~700 LOC of mutating-body recognizers), watch what tests break, build
the minimum L1 form needed to make each one pass, commit the slice.
The 37 failing tests after the rip became the spec; each subsequent
slice picked one and built minimum viable replacement. PR #138 landed
in 5 slices over a single day, vs three closed PRs that never landed.

**Generalizable principle:** when retiring legacy paths, delete first,
let failing tests drive replacement. Tests are the spec; the legacy
code is not. This works *only* under the hard-rule discipline (one
class moves at a time), which sets a finite, knowable failure
boundary.

### Lesson 3: the pure/mutating asymmetry is real, not provisional

**The misframing in the original plan:** "Layer 1 → Layer 2 → OpaqueExpr"
was treated as a single uniform pipeline. Pure-path expression and
mutating-body iteration would both flow through L2.

**The reality:** the two paths produce different output shapes — pure
path produces a single `OpaqueExpr` (the function's return value);
mutating body produces a `PropResult[]` (one equation per modified
rule plus frames). The L2 IR shape (single-rooted `IRExpr` tree) fits
the first; it's a poor fit for the second.

**The architectural commitment** post-M3:

- Pure path: TS → L1 expr → L2 `IRExpr` → `OpaqueExpr`. L2 is Pant's
  expression IR; the lowering is mechanical pattern-match.
- Mutating body: TS → L1 stmt → `PropResult[]` via single fold over
  `SymbolicState`. No L2 statement vocabulary. The fold *is* the
  lowering.

Future milestones (M4, M5, M6) inherit this asymmetry. M4 (equality /
nullish) and M5 (property access) are expression-only and naturally
extend the L1 → L2 expression path. M6 (cleanup) deletes the dead
L2 statement vocabulary and the `IRWrap` escape hatch.

### Lesson 4: `from-l2` is the deferred-normalization knob, and it grew at M3

The `from-l2` adapter (an L1 expression form wrapping a pre-built L2
`IRExpr`) was originally framed as transitional ("shrinks at M3 (more
sub-expressions reach L1 natively); deleted at M6"). M3 *grew* its use
because mutating-body sub-expressions (receiver, value, condition,
iteration source) all translate through `translateBodyExpr` and arrive
as OpaqueExpr — wrapping via `from-l2` is the cheap way to embed them
in L1 without re-translating.

This is fine. The `from-l2` adapter lets the build pass defer
expression-level normalization (M4 / M5 territory) without blocking
statement-level normalization (M1 / M2 / M3). Its lifetime is bounded
by M4 + M5 landing — at that point sub-expressions reach L1 natively
and `from-l2` shrinks for real, then deletes at M6.

### Lesson 5: combined-shape recognizers are sometimes load-bearing

**The setup at M4**: the equality / nullish equivalence class has three
canonical L1 forms (`IsNullish`, `BinOp(eq | neq, …)`, and the
loose-eq rejection). Each one is the canonical lowering for one
syntactic class. So far, so milestone-shaped.

**The forcing function**: idiomatic null-guard-then-list-return TS —
`if (x == null) return []; return [f(x)];` and the equivalent ternaries
— would map naturally to a cardinality-dispatch lowering `cond
IsNullish(x) => [], true => [f((x 1))]`. But Pantagruel has no list
literal. There is no expressible Pant target for `[f((x 1))]`. Without
some other recognition path, idiomatic TS code in this shape simply
doesn't translate.

**The fix**: M4 Patch 5's functor-lift recognizer matches a *combined*
shape — M1's Cond on the outside, M4's IsNullish (or any nullish leaf
form) on the guard, with empty/single-element-projection branch
checks — and lowers to `each $n in x | f $n`. The recognizer crosses
two milestone boundaries by construction.

**Generalizable principle**: when Pant's expression vocabulary lacks
a target for a specific TS shape, a recognizer crossing milestone
boundaries can be the only translatable path. This isn't a violation
of the "hard rule per equivalence class" discipline — the equivalence
class is normalized correctly inside its own milestone (M4 still
canonicalizes nullish testing onto `IsNullish`); the combined recognizer
is a *secondary* lowering that fires for a specific cross-class shape
where the primary path would emit untranslatable Pant.

The litmus test: would the alternative lowering produce valid Pant?
If no, a combined recognizer is structurally necessary. If yes, the
combined match is an optimization that should be deferred until the
optimization is genuinely worth carrying.

M4's functor-lift recognizer is the precedent. Future milestones may
need to follow it; M5 (property access) doesn't obviously need one,
but the general principle is now on the table.

## Milestones

### Milestone 1: imperative-ir-conditionals — ✅ landed

**Status**: landed across four stacked commits on
`zax--ts2pant-imperative-ir-workstream`:

- Patch 1 (`9e3b185`) — L1 vocabulary (`ir1.ts`) + lowering (`ir1-lower.ts`)
  + unit tests. ~880 lines of new code, no plumbing changes.
- Patch 2 (`7600ec3`) — L1 builder (`ir1-build.ts`) +
  `isStaticallyBoolTyped` (`purity.ts`) + plumbing into `translate-body.ts`
  gated on `TS2PANT_USE_L1=1`. New L1-plumbing integration tests.
  Validation: byte-identical output for all 431 existing tests under both
  flag states.
- Patch 3 (`9bbb25c`) — hard-rule cutover. Flag deleted, legacy
  `translateIfStatement` and inline ternary handler removed. Net −257
  lines. 469 tests pass under always-on L1.
- Patch 4 — docs (this commit).

**Pessimism rate**: 0 — no existing fixture or dogfood case rejects under
the conservative-refusal policy. The `&&`/`||` Bool-type predicate
accepts every site reached by current fixtures because non-Bool
short-circuit isn't an L1-conditional form (it falls through to the
legacy `translateOperator` path, which Patch 3 left intact for non-
conditional uses including `.reduce` callbacks). Real-fixture pessimism
will surface only as new TS code drops in; revisit policy if that happens.

**Definition of Done**:
- New module `tools/ts2pant/src/ir1.ts` (Layer 1 IR types) declares the full
  Layer 1 vocabulary. Statements: `Block`, `Let`, `Assign`, `Cond` (multi-arm
  with optional else), `Foreach`, `For`, `While`, `Return`, `Throw`,
  `ExprStmt`. Expressions: `Var`, `Lit`, `BinOp`, `UnOp`, `App`, `Member`,
  `Cond`. (Forms unused in M1 are *declared* but unimplemented; vocabulary
  is locked here.)
- New module `tools/ts2pant/src/ir1-build.ts` translates TS AST → Layer 1
  for the conditional surface forms only. Other forms still go through the
  existing TS-AST → Layer 2 path.
- Conditional surface-form normalization handles: if-chains (single-armed,
  if/else, if/else-if/else), ternary chains (right-associative flatten),
  `switch` without fall-through (every case ends in `break`/`return`/
  `throw`; `default` last; non-fall-through cases collapse to one
  `Cond` arm), `&&` and `||` as expressions when both operands are
  statically Bool-typed. All collapse to one canonical `Cond` form.
- Strict reject (3(b)): `switch` with any fall-through case, `default` not
  last when case-set non-exhaustive, `&&`/`||` with non-Bool operands. Each
  rejection produces an UNSUPPORTED with a specific reason.
- New module `tools/ts2pant/src/ir1-lower.ts` lowers Layer 1 `Cond`
  (statement and expression) to Layer 2 `Cond`. Other Layer 1 forms not
  yet exercised.
- `tools/ts2pant/CLAUDE.md` IR Migration Status table updated: Stages 9–11
  marked superseded, pointer to this workstream. New "Imperative IR" section
  documents the Layer 1 vocabulary and the conditional canonical form.
- Hard rule honored: every conditional-shaped TS construct now goes through
  Layer 1. Legacy conditional handlers in `translate-body.ts` deleted.
- All existing `pant --check`-clean fixtures still pass; new fixtures cover
  the conditional surface forms and the rejection cases.

**Why this is a safe pause point**: Conditionals are a self-contained
equivalence class. Every other recognizer remains on the TS-AST → Layer 2
path. The Layer 1 vocabulary is declared but only one normalization pass
exercises it. Codebase translates the same set of TS programs as before
M1, plus newly-translatable ones (conditional families that weren't
previously recognized uniformly).

**Unlocks**: M2 (assign + μ-search) builds on the Layer 1 vocabulary that
M1 declares. The architecture is proven on a richer equivalence class than
PR #131's increment-only motivator, so M2 is incremental rather than
exploratory.

**Open Questions**:
- Switch exhaustiveness: M1 uses syntactic check (default-last). Type-based
  exhaustiveness (literal-union switch with no default) is deferred — leave
  as UNSUPPORTED with a reason.
- Real-fixture pessimism rate from conservative refusal: measured during M1.
  If a high fraction of `&&`/`||` uses involve non-Bool operands, M4 may
  need to land before M3.

---

### Milestone 2: imperative-ir-assign-mu-search — ✅ landed

**Status**: landed across four stacked commits on
`zax--ts2pant-m2-assign-musearch`:

- Patch 1 (`4675728`) — activate `ir1Assign` and `ir1While`; add
  `buildL1IncrementStep` covering all five `+1` spellings + non-`+1`
  forms; new unit tests for vocabulary activation and step
  normalization.
- Patch 2 (`ea32328`) — L1 builder (`buildL1LetWhile`) + L1 recognizer
  (`isCanonicalMuSearchForm`); plumb behind `TS2PANT_USE_L1_MUSEARCH`.
  Validation: byte-identical output across all 463 existing tests
  under both flag states.
- Patch 3 (`c3dc24b`) — hard-rule cutover. Flag deleted.
  `recognizeMuSearch` renamed to `recognizeLetWhilePair` and stripped
  of all μ-search semantics (step shape, predicate-references-counter)
  — those checks now live in `isCanonicalMuSearchForm` and the
  unified `translateMuSearchInit`. Legacy `translateMuSearchInitLegacy`
  deleted. Three new fixtures (`compoundIncrementStep`,
  `explicitIncrementStep`, `explicitIncrementStepFlipped`) verify the
  five-spelling collapse to one canonical form. Net **−64 lines**.
- Patch 4 — docs (this commit).

**Architectural payoff**: TS-AST has *no* μ-search semantics. The
recognizer is structural-only (let + while pair); the canonical-shape
check, predicate-references-counter check, and discrete-strategy
check all live at the L1 layer. The five `+1` surface spellings
collapse to a single L1 `Assign(Var(c), BinOp(add, Var(c), Lit(1)))`
that the recognizer pattern-matches once.

**Pessimism rate**: 0 — the increment normalizer's `+1` recognition is
strictly broader than legacy's `++`/`++i`-only, and dogfood translates
unchanged. The new failure messages for the named-fn-expression and
method-shadowing cases are more specific than the old "not a
recognized μ-search" (now "predicate does not reference the counter"
or "predicate has side effects") and tests were updated.

**M2 cleanup (post-cutover follow-up)**: in the same PR, four
additional commits move μ-search lowering entirely out of
`translate-body.ts`. Articulated principle: *"L1 should be entirely
concerned with the syntax of TypeScript; it should be completely
unopinionated and ignorant about how TypeScript's semantics are
translated into specific lowerings."* Concretely:

- L2 gains a new `comb-typed` form for source-less typed
  comprehension — the missing vocabulary that legacy
  `translateMuSearchInit` worked around by going directly to
  OpaqueExpr.
- `lowerL1MuSearch` lands in `ir1-lower.ts` carrying all μ-search
  semantics: canonical-shape pattern match, strategy validation,
  binder allocation (via callback), counter-binder substitution
  (via Pant's `substituteBinder` on the lowered OpaqueExpr).
- `buildL1LetWhile` adds the predicate-references-counter check
  as a structural sanity check on the let+while pair.
- `translateMuSearchInit` shrinks to a thin orchestrator that
  wires the lowering context and delegates to the L1 → L2 →
  OpaqueExpr pipeline. No Pantagruel-target awareness.

Snapshot byte-equality preserved across all 8 μ-search fixtures.
translate-body.ts net −60 lines. Side benefit: predicate is now
translated once at L1 build (rather than twice as in pre-cleanup
M2) — substitution happens on the lowered OpaqueExpr.

**Definition of Done**:
- `ir1-build.ts` extends to translate increment surface forms: `i++`, `++i`,
  `i--`, `--i`, `i += k`, `-=`, `*=`, `/=`, `%=`, `i = i ⊕ k` (commutative
  ops only), `i = k ⊕ i` (same). All collapse to canonical
  `Assign(target, value)` with the value reconstructed as the appropriate
  `BinOp`.
- `recognizeMuSearch` rewritten to match against Layer 1: pattern
  `Block([Let(i, init), While(p, Assign(i, BinOp(Add, Var(i), Lit(1))))])`.
  One pattern, no `isPlusOneStep` helper. PR #131's recognizer extension
  is structurally subsumed.
- All five `+1` step spellings produce byte-identical Pant output (snapshot
  equality with M1's pre-rewrite output).
- `ir1-lower.ts` extends to handle Layer 1 `Assign` in pure-path contexts
  (read-modify-write of a const-bound accumulator, μ-search counter). Full
  mutation semantics deferred to M3.
- Legacy increment recognizer code deleted; hard rule honored for the
  increment equivalence class.
- New fixtures cover non-increment uses of `Assign` reachable in pure-path
  (e.g., `let acc = 0; for (...) acc += ...; return acc` — but only the
  μ-search-shaped cases land here; the iteration body remains on the
  legacy path until M3).

**Why this is a safe pause point**: Assign and μ-search form a coherent
slice. The Layer 1 vocabulary is now exercised by two normalization classes,
proving the architecture composes. Iteration with `Assign` in body still
goes through legacy recognizers (Shape A/B are unchanged).

**Unlocks**: M3 — iteration normalization can now use `Assign` in body
position without inventing a new form. Mutation work (former Stages 9–11)
re-forms on top of `Assign`.

**Open Questions**:
- `i = f(i)` (arbitrary self-update, e.g. iterate-to-fixpoint): build as
  Layer 1 `Assign(i, App(f, [Var(i)]))` and let the lowering pass decide
  rejection. Recognizer for iterate-to-fixpoint is its own future work,
  out of scope.
- Decrement μ-search (`i--` with `while (P(i))`): canonical form admits
  it, but the SMT lowering for `min over each j: Int, j <= INIT, ~P(j) | j`
  needs verification on a fixture before declaring supported. M2 lands the
  syntax; the encoding is M2 if straightforward, deferred otherwise.

---

### Milestone 3: imperative-ir-iteration-mutation — ✅ landed

**Status**: landed across one rip-out commit and five build-up slices on
`zax--ts2pant-m3-rip` (PR #138).

The iteration + mutation classes flow through Layer 1: iteration surface
forms (`for (const x of arr) { … }`, `arr.forEach(x => { … })`) build to
canonical `Foreach(binder, source, body, foldLeaves)`, branched mutation
builds to canonical statement-position `CondStmt`. The mutating path
**bypasses L2 entirely** — `lowerL1Body` in `ir1-lower-body.ts` walks
the `IR1Stmt` tree and emits `PropResult[]` directly while threading
the existing `SymbolicState` (the same primitives the legacy mutating
path uses — `putWrite`, `mergeOverrides`, `installMapWrite`,
`installSetWrite`) so frame-condition emission is unchanged.

**Strategy:** rip-out-first. After two architectural false starts (the
parallel-build PRs #134/#135/#137 produced an L2 statement vocabulary
that mirrored `symbolicExecute` line-for-line — zero abstraction value),
the strategy switched to deleting legacy mutation handling first and
letting failing tests drive the minimum L1 build/lower needed to satisfy
them. The 37 unit-test failures after the rip became the spec for each
slice.

**Slice breakdown:**

- Rip (`987eef3`) — delete `translateForOfLoop`, `translateForOfLoopBody`,
  `translateForEachStmt`, `classifyLoopStmt`, `ShapeBLeaf`, `LoopStmtClass`,
  `FoldOps`, `COMPOUND_ASSIGN_TO_FOLD` (~700 LOC); replace
  `symbolicExecute`'s if-statement / for-of / forEach arms with
  `unsupported` stubs.
- Slice 1 (`1a92203`) — `buildL1IfMutation` for `if (g) { obj.p = v }`
  branched property writes; `lowerCondStmt` per write-key fork/merge.
- Slice 2 (`4251819`) — Map/Set effect calls (`m.set/.delete`,
  `s.add/.delete/.clear`) in if-branches via `ir1MapEffect` /
  `ir1SetEffect`; `mergeOverrides`-based merge in `lowerCondStmt`.
- Slice 3 (`504cb8d`) — nested ifs and compound assigns in branch
  bodies via build-pass desugaring (`a.p OP= v` → `a.p = a.p OP v`).
- Slice 4 (`caa86a7`) — Shape A iterator writes via `buildL1ForOfMutation`
  / `buildL1ForEachCall`; `lowerForeach` runs the body through a subState
  and emits per-iter `all binder in src | prop' obj = value` equations.
- Slice 5 (`ff7892d`) — Shape B accumulator-fold via `IR1FoldLeaf`
  carried alongside `Foreach.body`. Build-time subState lets Shape B
  `rhs`/`guard` translations observe in-iter Shape A writes; lower pass
  emits `prop' target = prior outerOp (combOP over each x in src[, guard]
  | rhs)` per leaf.

**Pure-path `.reduce` is unchanged.** It's expression-position (chain
fusion via `BodyResult.pendingComprehension`), architecturally separate
from the mutating-path foreach work — the existing `translateReduceCall`
still owns it.

**Outcome:**

- `ir1-build-body.ts` — TS AST → L1 statements (mutating body).
- `ir1-lower-body.ts` — L1 statements → `PropResult[]` via `SymbolicState`.
- `translate-body.ts` no longer carries iteration / branched-mutation
  recognizers; the if-statement / for-of / forEach arms in
  `symbolicExecute` are thin dispatchers to the L1 path.
- All 476 unit tests + 22 integration tests pass.

**Why this is a safe pause point**: Iteration and mutation are a coherent
unit (one shapes the other). Both pure-path and mutating-path now flow
through Layer 1 for the three normalization classes (conditionals,
assign/μ-search, iteration). Property access, equality, and nullish-check
normalization remain on the legacy expression path — but those are
expression-level orthogonal classes; the architectural backbone is
complete.

**Unlocks**: M4 (equality/nullish) and M5 (property access) become
optional, low-risk follow-ups. M6 (cleanup) deletes whatever legacy code
remains in `translate-body.ts`.

**Open Questions**:
- Index-for over `.length` with mutating writes through `arr[i]`: legacy
  code rejects this. L1 normalization could handle it via `Foreach(x,
  arr, Assign(Member(x, ...), ...))` if `arr[i] = …` is read back during
  the same iteration. Risk of subtle aliasing semantics; deferred unless
  a fixture demands it.

---

### Milestone 4: equality-nullish-normalization — ✅ landed

**Status**: landed across six stacked patches on
`ts2pant-m4-equality-nullish`.

The equality / nullish equivalence class flows through Layer 1.
Every TS expression in the class normalizes to one of three canonical
shapes — `IsNullish(operand)`, `BinOp(eq | neq, …)`, or the functor-lift
`each $n in x | f $n` — plus an unconditional rejection of any
surviving loose `==` / `!=`.

**Slice breakdown:**

- Patch 1 — Add L1 `IsNullish` form and mechanical lowering. Lowers to
  `binop(eq, unop(card, x), litNat(0))` — byte-identical to the shape
  `??` and `?.` already emit. No call sites yet; build-pass cutover
  follows in P2.
- Patch 2 — Recognize nullish surface forms via L1 `IsNullish`. The
  build pass intercepts `x == null`, `x != null`, `x === null`,
  `x === undefined`, the `!==` negations, the long disjunction
  `x === null || x === undefined`, the conjunction-negated long form,
  and `typeof x === 'undefined'` (and `!==`) before the generic
  binop dispatcher fires. Long-form recognition uses
  `structurallyEqualExpression` (not `node.getText()`) for
  whitespace-tolerant operand identity. Negative forms wrap as
  `unop(not, IsNullish(operand))`. Partial-match disjunctions recurse
  on `||`/`&&` operands.
- Patch 3 — Canonicalize strict equality; reject all surviving loose-eq.
  `===` / `!==` route through L1 `BinOp(eq | neq, …)` unconditionally.
  `==` / `!=` are rejected with a specific reason ("loose equality
  (== / !=) is unsupported; use === / !==") on the body / L1
  expression-translation paths in `translate-body.ts` and
  `translate-signature.ts`'s `translateExpr`, *after* the nullish
  recognizer has had a chance to fold `x == null`. The signature
  guard-classification path (`containsUnsupportedOperator` in
  `translate-signature.ts`, used by `classifyGuardIf` and
  helper-followability) is stricter — it rejects ALL loose equality
  including `x == null`, so an `if (x == null) throw` is *not*
  classified as a guard and stays in the body where the body
  translator's nullish recognizer handles it. No type-based exception
  on either path — the principle is rule-1 of § "Developer Steering
  Principles" in `tools/ts2pant/CLAUDE.md`.
- Patch 4 — Refactor the `??` builder to construct its guard via L1
  `IsNullish`. Snapshot byte-equality is the cutover gate; the
  OpaqueExpr is identical pre- and post-M4.
- Patch 5 — Functor-lift recognizer for null-guarded list-lifted
  conditionals. `if (x == null) return []; return [f(x)];` and the
  three other supported shapes lower to `each $n in x | f $n`. Crosses
  M1 (Cond) + M4 (IsNullish) by design — see Lesson 5 below.
- Patch 6 — Docs + dogfood verification (this commit).

**Pessimism rate (loose-eq rejection)**: N=5 loose-equality sites
across fixtures + dogfood, all consumed by the nullish recognizer.
0 rejected. **0%.** No real-fixture or dogfood site exercises a
non-nullish loose-eq that would surface the rejection path; rejection
is covered by synthetic cases in `tests/equality-canonicalization.test.mts`.

**Definition of Done**:

- [x] New Layer 1 primitive `IsNullish(expr)`. Surface forms
  `x == null`, `x === null`, `x === undefined`, the long-form
  disjunction, `typeof x === 'undefined'` and their negations all
  collapse to `IsNullish(operand)` (or `unop(not, IsNullish(…))`
  for negative polarity).
- [x] `BinOp(eq | neq, …)` is the canonical strict-equality form;
  `===` / `!==` always canonicalize.
- [x] Loose `==` / `!=` rejected unconditionally outside the nullish
  recognizer (no type-based exception).
- [x] `??` builder refactored to construct its guard via L1
  `IsNullish`; snapshot byte-equal pre/post.
- [x] Functor-lift recognizer (combined M1 Cond + M4 IsNullish shape)
  lowers null-guarded list-lifted conditionals to `each $n in x |
  f $n`. Four supported TS shapes (positive ternary, negated ternary,
  positive if-conversion, negated if-conversion); operand restricted
  to a simple identifier; multi-element non-empty branches
  deliberately rejected.
- [x] All 598 unit tests + 22 integration tests pass; `pant --check`
  passes for every M4 fixture function (equality-nullish + functor-lift).
- [x] Hard rule honored for the equality/nullish class — every
  nullish-shaped TS expression flows through L1; legacy direct
  equality-to-null path retired.

**Resolved open questions**:

| Question | Resolution |
|----------|------------|
| M4 standalone vs absorbed into M3 | Landed standalone. M3 was absorbed enough by iteration + mutation; equality/nullish is expression-only and a clean six-patch slice. |
| Loose-eq policy: type-based accept exception, or unconditional reject? | Reject all surviving loose-eq. Per § "Developer Steering Principles" rule 1, `==` is ambiguous in TS regardless of operand type — defining a "safe-loose-eq" subset would be exactly the guesswork the IRSC discipline rejects. The `x == null` carve-out is path-scoped: on the body / L1 expression-translation paths, Patch 2's nullish recognizer consumes it before the loose-eq dispatcher fires; on the signature guard-classification path (`containsUnsupportedOperator`), all loose equality is rejected unconditionally including `x == null`, so a guard with `x == null` falls through to the body where the body translator handles it. |
| Combined-shape recognizer scope (functor-lift crossing M1 + M4) | Included as Patch 5. Pant has no list literal — without the recognizer, idiomatic null-guards over list-lifted return types are untranslatable. Sets the precedent recorded in Lesson 5. |

**Why this is a safe pause point**: Equality and nullish are
expression-only; no statement-level forms touched. M5 (property
access) and M6 (cleanup) can land independently.

**Unlocks**: M6 cleanup can shrink `from-l2` further (sub-expressions
of nullish and equality forms now build natively on L1; property
access defers to M5).

---

### Milestone 5: property-access-normalization — ✅ landed

**Status**: landed across six stacked patches on
`ts2pant-m5-property-access`.

The property-access equivalence class flows through Layer 1. Every TS
property-access surface form ts2pant supports — dotted access (`obj.f`)
and string-literal element access (`obj["f"]`) — builds to canonical
L1 `Member(receiver, name)` with `name` pre-qualified at build time
via `qualifyFieldAccess`. The legacy direct-to-L2
`App(qualified-rule, [receiver])` construction path is retired across
all 9 documented sites in `translate-body.ts`, `translate-signature.ts`,
`ir-build.ts`, and `ir1-build-body.ts`. Computed element access
(`obj[expr]`) is rejected unconditionally with a specific
`unsupported` reason — the DoD's "unless type system resolves to a
known field set" path requires literal-union narrowing and is deferred
to a follow-up.

**Slice breakdown:**

- Patch 1 — Add `buildL1MemberAccess` helper (in `ir1-build.ts`) that
  consumes a TS `PropertyAccessExpression` (or string-literal
  `ElementAccessExpression`), calls existing `qualifyFieldAccess` to
  resolve the rule name, and constructs L1 `Member(receiverL1,
  qualifiedName)`. Cut over pure-path PropertyAccess sites in
  `ir-build.ts`, `translate-signature.ts`, and `translate-body.ts`.
  Land paren-stripping at every L1 build dispatch entry point so
  `(x.f)` and `x.f` produce identical L1 trees universally.
- Patch 2 — Cut over mutating-body PropertyAccess sites in
  `translate-body.ts` (assignment targets, Shape A iterator writes,
  Shape B fold leaves) and `ir1-build-body.ts` (L1 statement target
  prop names, `buildL1AssignStmt`). Mutating-body assignment targets
  are now L1 `Member(receiver, name)` — Invariant 5.
- Patch 3 — String-literal ElementAccess (`obj["f"]`) routes through
  `buildL1MemberAccess` and produces the same canonical L1 Member
  form as `obj.f` (one canonical form per construct). Computed
  element access (`obj[expr]`) is rejected with a specific
  `unsupported` reason; covered by a deliberate-reject fixture
  function plus unit tests.
- Patch 4 — Lift the functor-lift recognizer's operand restriction
  from "simple identifier only" (M4 P5) to accept Member operands.
  The eligibility check is now expressed in L1 terms (`Var` or
  `Member`) rather than TS-AST terms — `u.owner == null ? [] :
  [u.owner.name]` and the three other supported shapes translate.
  Member-operand projections must surface the operand structurally
  at the L1 level, since `ast.substituteBinder` substitutes by name.
- Patch 5 — Shrink `from-l2` wrap sites for property-access
  sub-expressions. The four mutating-body sites (`ir1-build-body.ts`
  fold-leaf target / rhs / guard, `buildL1AssignStmt` receiver) that
  wrapped property-access sub-expressions via `from-l2` now route
  through a `buildL1SubExpr` helper that calls `buildL1MemberAccess`
  for property-access shapes and falls back to `from-l2` only for
  non-property sub-expressions.
- Patch 6 — Docs + dogfood verification (this commit).

**Pessimism rate (computed-access rejection)**: 0 — fixtures and
dogfood use dotted access exclusively; no fixture or dogfood site
exercises computed `obj[expr]` that would surface the rejection
path. **0%.** Rejection is exercised by the deliberate-reject
`getByDynamicKey` fixture function plus unit tests in
`tests/ir1-build-member-access.test.mts`.

**from-l2 wrap sites post-M5**: 1 (down from 4); the four documented
property-access sub-expression sites now route through the
`buildL1SubExpr` helper, whose single from-l2 fallback fires only
for non-property sub-expressions (binop chains, generic call
results, etc.) awaiting M6 deletion of the form.

**Definition of Done**:

- [x] Layer 1 `Member(receiver, name)` is canonical for both
  `obj.name` and `obj["name-literal"]`. Member is L1-only — no L2
  `FieldAccess` form (Member lowers mechanically to
  `App(qualified-rule, [receiver])`, preserving the IRSC divergence).
- [x] Computed access `obj[expr]` is rejected unconditionally with a
  specific `unsupported` reason (deferred to a follow-up: the DoD's
  "unless type system resolves to a known field set" path requires
  literal-union narrowing).
- [x] Hard rule honored — every property-access TS construct flows
  through L1; the legacy direct-to-L2 `App(qualified-rule, [receiver])`
  construction path is retired across all 9 documented sites.
- [x] Mutating-body assignment targets are L1 `Member` (Invariant 5).
- [x] Paren-stripping (`(x)` ≡ `x`) is universal at L1 build —
  applied at every TS-AST → L1 dispatch entry point, not just
  inside `buildL1MemberAccess` (Invariant 4).
- [x] Functor-lift recognizer accepts Var or Member operands; the
  M4 P5 simple-identifier restriction is lifted (Invariant 7).
- [x] `.length` / `.size` cardinality dispatch on the six list-shaped
  TS types (Array, ReadonlyArray, Set, ReadonlySet, Map, ReadonlyMap)
  builds to L1 `Unop(card, receiver)` via `tryBuildL1Cardinality`,
  promoted from inline short-circuit. The dispatch fires *before*
  Member; the divergence is target-language-driven (Pant's
  cardinality primitive is `#x`, not a `length` / `size` rule)
  (Invariant 8).
- [x] All unit tests + integration tests pass; `pant --check` passes
  for every M5 fixture function.

**Resolved open questions**:

| Question | Resolution |
|----------|------------|
| M5 standalone vs absorbed into earlier milestone | Landed standalone post-M4. The architectural backbone (L1 `Member` form + `Member → App(qualified, [receiver])` lowering) was always present and waiting for the cutover; folding into M3 would have ballooned the rip-out-first slice (M3 absorbed iteration + mutation), and folding into M4 would have crossed two unrelated equivalence classes (equality/nullish vs property access) in one milestone — violating the hard-rule discipline at the milestone-naming level. Standalone M5 is a clean six-patch slice with reviewed-snapshot-diff as the gate. |
| String-literal vs computed element-access scope | String-literal `obj["f"]` collapses to the same canonical Member form as `obj.f`. Computed `obj[expr]` rejected unconditionally — the type-system literal-union narrowing path is deferred to a follow-up (soundness conditions on partial unions, branded strings, generics-resolving-to-literal-types are non-trivial). |
| Type-erasure wrappers (`as T`, `!`, `<T>x`, `satisfies T`) at L1 build | Not stripped. They are load-bearing at the TS-checker layer ts2pant translates against — `qualifyFieldAccess` calls `checker.getTypeAtLocation` on the asserted node to honor the user's contract that the receiver should be treated as the asserted type. Stripping would silently change the qualifier resolution. Symmetric `unwrapExpression` strip in `buildL1SubExpr` (Patch 5) is for *outer*-position wrappers around the sub-expression — wrappers around the receiver inside a property access stay put. |
| `qualifyFieldAccess` two-tier behavior (qualified rule names vs bare kebab names vs null) | Preserved as-is in M5. Resolved-owner cases produce qualified rule names (e.g., `account--balance a`); unresolved-non-ambiguous cases (built-ins, type parameters, anonymous-without-synth) fall back to bare kebab'd field names (e.g., `to-fixed n 2`); ambiguous unions return null and reject. The asymmetry is a deliberate two-tier EUF separation, not an inconsistency — see § "Divergence from IRSC" in `tools/ts2pant/CLAUDE.md`. |

**Why this is a safe pause point**: Property access is a
self-contained equivalence class. M6 (cleanup) can land
independently — the remaining `from-l2` sites are non-property
sub-expressions awaiting M6 deletion of the form.

**Unlocks**: M6 cleanup of the last legacy expression handling.
The `from-l2` adapter now wraps only non-property sub-expressions
(binop chains, generic call results) and becomes unreachable at M6
when `IRWrap` and remaining legacy expression-path fallbacks are
removed. (The L2 *statement* vocabulary was already deleted in M3 —
see "Already done in PR #138" under M6.)

---

### Milestone 6: legacy-recognizer-cleanup

**Definition of Done**:

*Pure-path cutover (was Stage 8 in the legacy IR Migration Status
table):*

- Promote `--use-ir` from opt-in to always-on, then delete the flag
  (env `TS2PANT_USE_IR=1`) and the `useIRPipeline()` predicate.
- Delete pure-path code in `translate-body.ts` subsumed by the L1 path.

*`IRWrap` removal:*

- Delete `IRWrap` form from `IRExpr`. The escape hatch should be
  unreachable once `from-l2` (its L1 counterpart) shrinks via M4 / M5.

*`translate-body.ts` shape:*

- Slim to: parameter / scope plumbing, sub-expression dispatch into
  `translateBodyExpr`, `SymbolicState` primitives (still load-bearing
  for `lowerL1Body`), and the thin `symbolicExecute` orchestrator that
  dispatches statement kinds to the L1 build/lower pair.
- All TS-AST → L2 direct expression paths consumed by L1 normalization.

*Docs:*

- Replace legacy IR Migration Status table in `tools/ts2pant/CLAUDE.md`
  with a post-cleanup architecture description anchored on this
  workstream.

**Why this is a safe pause point**: End state of the workstream. One
translation pipeline. L2 IRExpr is canonical for value-position; L1
statements lower directly to `PropResult[]` for effect-position. No
escape hatches. The recognizer-extension treadmill is retired.

**Unlocks**: Future TS surface support is a normalization-pass extension
(small, localized) instead of a recognizer addition.

**Already done in PR #138** (folded into M3 cleanup once it became
clear the L2 statement vocabulary was free to delete):

- L2 `IRStmt` ADT (`Write`, `LetIf`, `Seq`, `Assert`) + constructors
  removed.
- `IRBody`, `IREquation`, `IRAssertExit` + `lowerEquation` /
  `lowerAssert` helpers removed.
- `src/ir-subst.ts` deleted.
- `lowerL1Stmt` shell removed from `src/ir1-lower.ts`.

## Dependency Graph

```text
1 (imperative-ir-conditionals)        → []
2 (imperative-ir-assign-mu-search)    → [1]
3 (imperative-ir-iteration-mutation)  → [2]
4 (equality-nullish-normalization)    → [3]   (✅ landed standalone)
5 (property-access-normalization)     → [3]   (✅ landed standalone post-M4)
6 (legacy-recognizer-cleanup)         → [3, 4, 5]
```

PR #128 must merge before M1 begins. M1–M4 are strictly sequential because
each adds a class to the same Layer 1 vocabulary and the hard-rule
constraint forbids partial migration of a class. M5 ran independently
of M4 once M3 had landed.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| Decrement μ-search (`i--`) SMT encoding | Layer 1 admits it; SMT lowering for `max over each j: Int, j <= INIT, ~P(j) \| j` (the *greatest* `j ≤ INIT` satisfying `¬P` — dual of the increment case's `min over j ≥ INIT`) needs verification on a fixture. | When a fixture demands it |
| Index-for with mutating `arr[i] = …` writes | Aliasing semantics non-trivial. Deferred unless a fixture demands it. | When a fixture demands it |
| Computed `obj[expr]` with type-system literal-union narrowing | M5 rejects all computed element access unconditionally. The DoD's "unless type system resolves to a known field set" path requires walking the index expression's type, enumerating string-literal members, joining with the receiver type's declared fields, and dispatching per field — soundness conditions on partial unions, branded strings, and generics-resolving-to-literal-types-only-at-call-site are non-trivial. | When a fixture demands it |
| `.indexOf(x) === -1` array-absence recognition | Currently translates correctly via `===` canonicalization (EUF equality on the sentinel `-1`). Routing through `IsNullish` would be a stricter modeling of "absence" but isn't required for correctness. | When a fixture demands it |
| Enum-sentinel-as-nullish absorption (`x === SomeEnum.NONE`) | Typed equality on a named constant; works as-is via `===` canonicalization. Absorption into `IsNullish` would require user-declared "absence" values that ts2pant doesn't model today. | When a fixture demands it |
| Truthiness coercion (`Boolean(x)`, `!!x`) | Semantically distinct from nullish testing (truthy excludes `0`, `""`, `false`). May stay UNSUPPORTED indefinitely or become its own milestone. | Defer; revisit if a fixture demands it |

Resolved questions (kept for history):

| Resolved | Resolution |
|----------|------------|
| Conservative-refusal pessimism rate | M1, M2, M3, M4, M5 all measured 0% pessimism on existing fixtures. Policy 3(b) holds. |
| M5 standalone vs absorbed into earlier milestone | Landed standalone post-M4. The architectural backbone (L1 `Member` form + `Member → App(qualified, [receiver])` lowering) was always present; folding into M3 would have ballooned the rip-out-first slice, and folding into M4 would have crossed two unrelated equivalence classes (equality/nullish vs property access) in one milestone. Standalone M5 is a clean six-patch slice with reviewed-snapshot-diff as the gate. |
| `Reduce` as own L1 form vs desugared | Pure-path `.reduce` stays on `translateReduceCall` (chain-fusion via `BodyResult.pendingComprehension`); not absorbed into L1. The "desugared into `Let + Foreach + Assign`" framing in the original plan was wrong — chain fusion is expression-position, architecturally separate from M3's mutating foreach. |
| Frame-condition emission ordering | Frames flow through `state.modifiedProps` (Set with insertion-order iteration) — same path as legacy. No L1 → L2 lowering pass for frames; M3's `lowerL1Body` reuses the existing primitive. |
| M4 standalone vs absorbed into M3 | Landed standalone post-M3. M3 was absorbed enough by iteration + mutation; equality/nullish was a clean six-patch slice on its own. |
| Loose-eq policy at M4 | Reject all surviving loose-eq unconditionally. Per § "Developer Steering Principles" rule 1 in `tools/ts2pant/CLAUDE.md` — `==` is ambiguous in TS regardless of operand type, so steering the programmer to `===`/`!==` is the right move. |
| Combined-shape recognizer scope at M4 | Included as Patch 5 (functor-lift). Pant has no list literal; without the recognizer, idiomatic null-guarded list-lifted conditionals are untranslatable. Sets the precedent recorded in Architectural Lesson 5. |

## Decisions Made

Foundational (pre-M1):

| Decision | Rationale |
|----------|-----------|
| Layer 1 vocabulary locked at M1 | Mid-stream re-litigation of canonical forms is the failure mode the workstream is meant to prevent. Forms can be *added* later (e.g., `IsNullish` at M4, `IR1FoldLeaf` at M3) but existing forms cannot be changed. |
| Conservative refusal = reject the function (3(b)) | Cleaner architecturally; matches the "find equivalences that are actually equivalences" discipline. Pessimism measured per-class; policy revisitable per-class if real fixtures hurt. |
| Hard rule per equivalence class (4) | Faster per milestone, prevents the cross-talk hazard the IR was meant to retire. No half-migrated classes coexisting with legacy recognizers. |
| Mutation lands with iteration in M3 (5(b)) | `Foreach` body must be a *statement* (admit `Assign`) to support Shape A, Shape B, and `.reduce` desugaring uniformly. Iteration normalization without `Assign` would force splitting the form. |
| No `If(cond, then, else)` separate from `Cond` | Single-armed if = `Cond([(g, body)], None)`. Multi-armed normalizes to the same form. One downstream pattern. |
| No `Inc(target)` separate from `Assign` | `i++` builds as `Assign(i, BinOp(Add, Var(i), Lit(1)))`. The "+1 step" is a single arithmetic predicate at μ-search lowering, not a separate form. |
| `&&`/`\|\|` normalized only when Bool-typed | Truthy/falsy semantics on non-Bool values diverge from `Cond`'s Boolean guards. Conservative refusal on non-Bool. |
| `switch` fall-through rejected at L1 build | Each case must end in `break`/`return`/`throw`. Fall-through is a different control structure with no clean canonical form. |

Post-M3 (architectural lessons hardened into commitments):

| Decision | Rationale |
|----------|-----------|
| Mutating-body lowering bypasses L2 (Lesson 1) | L2 statement vocabulary mirrored `symbolicExecute` line-for-line — zero abstraction value. `lowerL1Body` threads `SymbolicState` directly into `PropResult[]`, reusing existing mutation primitives. |
| Rip-out-first for retiring legacy paths (Lesson 2) | Parallel-build under hard-rule discipline forces *all* of a class onto the new path before any cutover lands, ballooning PRs. Deleting legacy first lets failing tests drive minimum-viable replacement. |
| Pure / mutating asymmetry is permanent (Lesson 3) | Pure path returns one `OpaqueExpr`; mutating body returns `PropResult[]`. L2 `IRExpr` (single-rooted tree) fits the first; the second is a fold over `SymbolicState`. The two paths share L1 but diverge at lowering. |
| `from-l2` lifetime extends through M4 / M5 (Lesson 4) | The adapter grew at M3 (mutating-body sub-expressions arrive as OpaqueExpr from `translateBodyExpr` and wrap via `from-l2`). Shrinks for real once M4 / M5 bring sub-expressions onto L1 natively. |
| Workstream supersedes Stages 9–11 of CLAUDE.md IR Migration Status | M3 absorbed mutating-path SSA, frame conditions, and mutating-path cutover. Stage 8 (pure-path cutover) re-forms inside M6 alongside the L2 statement vocabulary deletion and `IRWrap` removal. |
