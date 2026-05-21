# Workstream: ts2pant General-Loop SSA

## Vision

Extend IR1 SSA from today's summary-based loop handling (μ-search, foreach
Shape A/B) to a unified treatment of general `for` and `while` loops. Bounded
loops lower to quantified Pantagruel equations; unbounded `while` lowers to
fixed-point / recursive rule definitions; `break` and `continue` are
first-class early-exit semantics. The end state is a single coherent loop SSA
machinery in which the prior workstream's μ-search and foreach forms are
specialisations of general-loop SSA rather than separate code paths.

## Current State

The IR1 SSA workstream (`workstreams/ts2pant-ir1-ssa.md`, M1–M7) is complete.
IR1 SSA is the production mutating-body boundary for scalar property mutation,
Map/Set semantics, and the three existing supported loop summaries. Loop
handling today consists of three dedicated paths in
`tools/ts2pant/src/ir1-ssa-loops.ts`: μ-search, foreach Shape A (quantified
writes), and foreach Shape B (accumulator folds). The IR1 statement vocabulary
already admits `for` and `while` shapes (constructed in
`buildSupportedSsaMutatingBody` in `tools/ts2pant/src/translate-body.ts`), but
`tools/ts2pant/src/ir1-lower-body.ts` rejects them with a diagnostic. An audit
of the `IR1Expr → IRExpr → OpaqueExpr` expression pipeline confirmed it
already carries loop guards, counter steps, and bound expressions without
restructuring; no expression-path cleanup is a prerequisite.

## Key Challenges

- **Two distinct Pant target shapes**. Bounded loops naturally lower to
  quantified equations (`∀ i ∈ 0..n`); unbounded `while` requires fixed-point /
  recursive rule definitions. These are not interconvertible, but they must
  share a unified IR1 SSA representation.
- **Loop-header phi nodes**. Structurally different from M2's branch joins:
  one input (the loop-back value) is defined *later* than the join itself.
  SSA construction must handle the forward reference.
- **Termination reasoning**. Bounded loops need an extractable termination
  metric (counter bound, ranking expression). The boundary between "we can
  prove this terminates" and "fall back to fixed-point" is load-bearing.
- **Fixed-point semantics in Pantagruel**. Emitting recursive rule definitions
  that pass `pant --check` is a novel idiom for this project; empirical
  validation per fixture is required and is a hard gate on the milestone
  that introduces them.
- **Summary-unification regression risk**. The existing μ-search and foreach
  summary lowering produces Pant output that the corpus depends on. Folding
  those into the general-loop machinery must preserve semantic parity.
- **break/continue interaction with loop headers**. Early exits change which
  writes reach the post-loop join. The continuation-merge model from the prior
  workstream's M2 is the starting point but must extend past plain branch
  joins.

## Established Precedents

- **algorithm — Cytron et al. SSA construction with loop-header phi placement**
  — https://doi.org/10.1145/115372.115320
  Standard algorithm for placing phi nodes at loop headers (dominance frontier
  of back-edges). The IR1 SSA builder must follow the classical two-pass
  approach: place the header join first with a placeholder loop-back input,
  emit the body, then back-patch the loop-back version. Applies across L1–L5.

- **paper — Cousot & Cousot abstract interpretation / fixed-point loop
  semantics** — https://doi.org/10.1145/512950.512973
  Classical reference for understanding loop semantics as least fixed points of
  monotone functions over program states. The unbounded-`while` lowering
  target — recursive Pant rule definitions — operates at this level:
  `loop-result = fix (λs. if guard(s) then body(s) else s)`. L4's Pant
  encoding must match this characterisation, verified by `pant --check`.

- **algorithm — Ranking functions / termination metrics (Floyd; Dijkstra)** —
  https://en.wikipedia.org/wiki/Termination_analysis
  For the bounded-loop fork (L2, L3): extract a value that strictly decreases
  each iteration and is bounded below. Counter loops yield this directly
  (`bound - i`); bounded `while` requires synthesis from the guard expression.
  Where no metric is extractable, route to fixed-point lowering instead of
  rejecting.

- **paper — IRSC (Vekris, Cosman, Jhala — PLDI 2016)** —
  https://doi.org/10.1145/2908080.2908110
  Already cited in `tools/ts2pant/CLAUDE.md` as the foundational reference for
  ts2pant's intermediate representation. Loop-header join vocabulary must
  remain IRSC-faithful: location SSA (not local-variable SSA), opaque versions
  allocated centrally, rule-oriented frames.

- **foundational mathematics — Tarski–Knaster fixed-point theorem** —
  https://doi.org/10.2140/pjm.1955.5.285
  Tarski, "A Lattice-Theoretical Fixpoint Theorem and Its Applications"
  (Pacific J. Math. 1955). The unifying theory behind L4's recursive
  Pant rule, the Cousot & Cousot fixed-point loop semantics already
  cited, abstract interpretation domains, inductive/coinductive
  predicates, Hoare loop invariants, Liquid Types refinement variables,
  Skolemization, and bisimulation. Every monotone operator on a
  complete lattice has a least and a greatest fixed point; the
  "uninterpreted invariant predicate" is the unknown in such an
  equation, the constraint system is the operator, the SMT solver
  computes the fixpoint. Knowing this is what makes the
  `define-fun-rec` / CHC / k-induction / BMC trade-off legible —
  they are four solvers over the same constraint system, differing
  in automation strength and predictability rather than in the
  underlying problem they solve. See
  `tools/ts2pant/AGENTS.md` § "Foundational Pattern: Uninterpreted
  Predicates and Implicit Definition" for the broader specialisation
  landscape and the Pantagruel-specific manifestation.

- **SMT spec — SMT-LIB recursive function definitions (`define-fun-rec`)** —
  https://smt-lib.org/papers/smt-lib-reference-v2.6-r2021-05-12.pdf
  Reynolds, Iosif, Tinelli et al.'s mechanism (SMT-LIB 2.5+) for declaring
  recursive functions in SMT. Z3 supports it with limited automation
  (built-in induction on select shapes only); CVC5 has stronger automation
  via DRAT-based induction. Direct encoding target for L4's "recursive
  Pant rule definitions": `loop-result s = cond guard(s) => loop-result
  (body s), true => s.` becomes one `define-fun-rec` in the lowered SMT.

- **technique — Constrained Horn Clauses (CHC) for program verification** —
  https://doi.org/10.1007/978-3-319-23534-9_2
  Bjørner, Gurfinkel, McMillan, Rybalchenko, "Horn Clause Solvers for
  Program Verification" (Fields of Logic and Computation II, 2015). Encode
  each loop as a recursive predicate over (pre-state, post-state) tuples;
  dispatch to Spacer (built into Z3) or Eldarica. The dominant modern
  technique for unbounded loop verification; substantially stronger
  automation than direct `define-fun-rec`. L4's encoding alternative if
  recursive-function dispatch proves too weak in practice — relevant
  to L4's open question on naming vs. inlining the recursive rule.

- **algorithm — k-induction** —
  https://doi.org/10.1007/3-540-40922-X_8
  Sheeran, Singh, Stålmarck, "Checking Safety Properties Using Induction
  and a SAT-Solver" (FMCAD 2000). Prove `P` holds at the initial state
  and that `P` at depths `n..n+k-1` implies `P` at `n+k`. Practical
  fallback when induction over the recursive function fails; routinely
  shipped behind a depth flag. Donaldson et al. (2011) refined the
  technique for software model checking. Relevant to L4's documented-
  timeout policy — k-induction at modest k is a reasonable acceptance
  criterion for fixtures whose direct recursive encoding times out.

- **algorithm — Bounded Model Checking (BMC)** —
  https://doi.org/10.1007/3-540-49059-0_14
  Clarke, Biere, Cimatti, Zhu, "Symbolic Model Checking without BDDs"
  (TACAS 1999). Unroll the loop N times and check the unrolled program.
  Pantagruel's existing `--bound N` flag is the natural surface. Directly
  relevant to L4's open question "unfold N iterations into a `cond` chain
  before falling back to a recursive tail rule" — that *is* BMC, named.
  The SMT-performance vs spec-readability trade-off the open question
  identifies is the same trade-off BMC literature has spent twenty years
  on; existing benchmarks (SV-COMP) are the empirical baseline.

- **spec-language design — Dafny invariant + variant** —
  https://doi.org/10.1007/978-3-642-17511-4_20
  Leino, "Dafny: An Automatic Program Verifier for Functional Correctness"
  (LPAR 2010). Reviewable spec-source surface for `while`: explicit
  `invariant I` clauses for partial correctness, `decreases V` for
  termination. Relevant to L4's operator-action requirement that "the
  recursive-rule output style is reviewable" — Dafny's convention is
  the closest peer to what a hand-written Pant spec for an unbounded
  loop would look like. External reviewer familiar with Pant should be
  given a Dafny-equivalent fixture to compare side-by-side.

- **spec-language design — Why3** —
  https://doi.org/10.1007/978-3-642-37036-6_8
  Filliâtre & Paskevich, "Why3 — Where Programs Meet Provers" (ESOP
  2013). Same invariant/variant shape as Dafny; dispatches to multiple
  SMT back-ends including Z3 via `define-fun-rec`. Useful as a
  cross-check for L4's encoding tractability — a Why3+Z3 baseline on a
  candidate fixture indicates whether `pant --check` should reasonably
  succeed on the same shape, or whether the recursive encoding alone
  is insufficient and a CHC backend is needed.

- **refinement system — Liquid Haskell refinement reflection** —
  https://doi.org/10.1145/3158141
  Vazou, Tondwalkar, Choudhury, Vekris, Newton, Jhala, "Refinement
  Reflection: Complete Verification with SMT" (POPL 2018). The closest
  existing system to what L4 will do: make recursive function bodies
  visible to the SMT solver as logical relations rather than
  uninterpreted symbols. The reflection mechanics (definitional
  axioms, β-normalisation in the SMT theory, PLE / proof-by-logical-
  evaluation) inform what Pant's recursive-rule lowering must surface
  to remain decidable. Co-authored by Jhala, also an IRSC author —
  the L4 encoding shape inherits both the IRSC SSA discipline and
  the refinement-reflection unfolding discipline.

- **technique — tail-recursive desugaring of `while`** —
  https://doi.org/10.1016/0304-3975(75)90017-1
  Plotkin, "Call-by-name, call-by-value and the λ-calculus" (TCS 1975).
  The standard CPS-style desugar: `while P body` ≡ `let rec loop s =
  if P(s) then loop(body(s)) else s`. Mechanical bridge from
  `IR1Stmt.while` to a recursive Pant rule definition; cited
  explicitly so L4's build pass doesn't reinvent the bridge. Composes
  with the SSA loop-header join from Cytron — the recursion's
  argument carries the location-SSA versions, and the call site
  is the back-edge.

- **recursion scheme — hylomorphism** —
  https://maartenfokkinga.github.io/utwente/mmf91m.pdf
  Meijer, Fokkinga, Paterson, "Functional Programming with Bananas,
  Lenses, Envelopes and Barbed Wire" (FPCA 1991). Already cited in
  `tools/ts2pant/docs/transformations.md` § "Structured Iteration"
  for the foreach handling (Shape A / Shape B / .reduce). L4 inherits the same paper for the
  unfold-then-fold framing — bounded loops are catamorphisms (fold
  side; cataphorism = the `foldr` already in use), unbounded loops
  are hylomorphisms (`unfold` via the loop guard, fold via state
  combination). Restating the connection at L4 keeps the
  recursion-scheme vocabulary consistent across the bounded and
  unbounded fork; useful when L6 unifies the summary path with the
  general-loop path.

## Milestones

### Milestone 1: general-loop-contract

**Status**: Landed in `ts2pant-general-loop-contract`.

**Definition of Done**:
`tools/ts2pant/src/ir1.ts` exports the SSA vocabulary needed for general
loops:

- Loop-header join type and constructor, with the two-pass back-patching
  protocol for the loop-back input.
- Loop body region type.
- Termination-metric value type, attachable to a loop-header join.
- Continuation handles for `break` and `continue`.

Constructors follow the M1 style of the prior workstream: opaque versions
allocated centrally; locations carried explicitly; no implicit Pant phi shapes
leak into the public surface. Documentation in `ir1.ts`,
`tools/ts2pant/AGENTS.md`, and this workstream describes the vocabulary
consistently (see `### General-loop SSA contract surface (L1)` in
`tools/ts2pant/AGENTS.md`). The existing `for` / `while` rejection in
`ir1-lower-body.ts` remains in place.

**Why this is a safe pause point**:
Type-level and documentation-level only, mirroring M1 of the prior workstream.
Production builders and lowerers continue to reject unsupported loop shapes.

**Unlocks**:
L2 implements the bounded-counter SSA builder against a stable vocabulary.
Later milestones implement against the same surface without expanding it.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- Exact type names and field layout for the loop-header join, loop body
  region, termination-metric value, and break/continue continuation handles.
  Must follow the M1 style and the Cytron back-patching protocol.
- Whether the loop body region carries an explicit list of mutated locations,
  or whether mutated-rule discovery is deferred to lowering (as in current
  scalar SSA).

---

### Milestone 2: bounded-counter-loop-lowering

**Status**: Landed in `ts2pant-bounded-counter-loop-lowering`.

**Definition of Done**:
The mutating-body build path emits IR1 SSA for bounded counter loops:

- `for (let i = 0; i < n; i++) { body }` and the equivalent shapes the
  gameplan settles on.
- Loop-header phi nodes constructed via the L1 vocabulary with the two-pass
  Cytron-style back-patching protocol.
- Termination metric extracted from the counter and the bound, attached to
  the loop-header join.

`tools/ts2pant/src/ir1-lower-body.ts` lowers the bounded-counter case to
quantified Pant equations. At least three representative counter-loop fixtures
exist in the test corpus; each passes `pant` typecheck and `pant --check` SMT
verification. Non-bounded-counter loops continue to reject with diagnostics at
least as clear as today.

(Pant SMT bounded-numeric over-each support was bundled with this milestone so
the emitted quantified equations verify end-to-end.) The concrete lowering
contract is documented in `### Bounded counter loop lowering (L2)` in
`tools/ts2pant/AGENTS.md`.

**Why this is a safe pause point**:
One new TS loop class is supported end-to-end. The summary-based μ-search and
foreach paths are unchanged. All other rejections remain in place.

**Operator Actions Before Next Milestone**:
- Run the full ts2pant integration suite; confirm zero regressions in
  prior-shipping fixtures.
- Inspect emitted Pant for the new counter-loop fixtures; confirm output
  passes both `pant` and `pant --check`.
- Decide whether L3's bounded-while lowering should reuse L2's quantification
  machinery as-is, or whether the implementation experience suggests revising
  the quantification target before proceeding.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- The exact Pant shape for the quantified-equation lowering. Worked example
  for one fixture, `pant --check`-verified, before the gameplan executes.
- Which counter-loop surface shapes are in this milestone's first slice
  (canonical `for (let i = 0; i < n; i++)` only, or also `i <= n`, decrement,
  step ≠ 1, etc.).
- Whether the termination metric is a first-class IR field or recomputed at
  lowering time from the counter and bound.

**Unlocks**:
Concrete evidence that the contract holds for the simplest case. L3 builds on
L2's quantification path.

---

### Milestone 3: bounded-while-lowering

**Status**: Landed in `ts2pant-bounded-while-lowering`. See
`tools/ts2pant/docs/intermediate-representation.md` section
`## Bounded while loop lowering (L3)` for the accepted ascending and
descending counter-desugar surface, peephole, and rejection contract.

**Definition of Done**:
The mutating-body build path emits IR1 SSA for bounded `while` loops — those
whose guard expression admits ranking-function extraction. Lowering reuses
the quantification machinery from L2, with guard-driven bound extraction
added. At least two representative bounded-while fixtures pass `pant` and
`pant --check`. `while` loops without an extractable ranking function continue
to reject (they are L4's responsibility).

**Why this is a safe pause point**:
All bounded loop classes (counter + bounded while) lower through one shared
quantification path. Fixed-point lowering is still deferred; unbounded loops
still reject; summary unification still deferred.

**Operator Actions Before Next Milestone**:
- Confirm bound-extraction does not silently misclassify an unbounded loop as
  bounded. Spot-check rejection diagnostics on unbounded fixtures from the
  corpus.
- Decide whether to proceed to L4 or pause the workstream — if the bounded
  subset covers the project's real-world TS, L4 can be deferred indefinitely.
  Note: deferring L4 also defers L6 (unification depends on the full general
  machinery existing).

**Open Questions** (resolved during `write-gameplan` for this milestone):
- The ranking-function extraction procedure. What guard shapes count as
  bounded? At minimum: monotone counter against a loop-invariant bound; the
  gameplan may extend the catalogue.
- Whether the user can override the inferred metric via a Pant comment
  annotation, or whether inference is the only source.
- Diagnostic shape for "looks like it should be bounded but inference failed"
  — distinct from "unambiguously unbounded".

**Unlocks**:
A single bounded-lowering family. L4 introduces a parallel fixed-point family.

---

### Milestone 4: fixed-point-while-lowering

**Status**: Landed in `ts2pant-fixed-point-while-lowering`. See
`tools/ts2pant/docs/intermediate-representation.md` section
`## Fixed-point while loop lowering (L4)` for the recursive-rule lowering
shape, the two build-path routes, and the documented per-fixture timeout
policy.

**Definition of Done**:
The mutating-body build path emits IR1 SSA for `while` loops without
extractable ranking functions, routing them to the fixed-point lowering
target. `tools/ts2pant/src/ir1-lower-body.ts` lowers the fixed-point case to
recursive Pantagruel rule definitions. At least three fixed-point fixtures
pass `pant` typecheck. Each `pant --check`s either to success or to a
documented SMT timeout with a written rationale for why the timeout is
acceptable for that fixture shape. A failure-mode test confirms genuinely
divergent loops (no fixed point) are detected by `pant --check` and surface a
clear diagnostic.

**Why this is a safe pause point**:
All currently-shippable `while` shapes lower. break/continue is still deferred
(loops without early exit only). Summary unification still deferred.

**Operator Actions Before Next Milestone**:
- Validate the recursive-rule output style with at least one external reviewer
  familiar with Pant. The recursive-rule idiom is novel for this project;
  spec readability is a first-class concern. This remains required
  post-merge as an out-of-band review action; it is not gated by this
  gameplan patch.
- Confirm `pant --check` SMT performance is acceptable across the full corpus.
  If solver timeouts become routine, pause the workstream and revisit
  ranking-function extraction in L3 — the bound-extraction net may need to
  catch more `while` shapes before they reach fixed-point.
- Decide whether L5 (break/continue) is a hard requirement or can be deferred
  to a follow-on workstream.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- The exact Pant shape for the fixed-point lowering. Worked example for one
  fixture, `pant --check`-verified or with a documented acceptable timeout,
  before the gameplan executes.
- Whether to unfold N iterations into a `cond` chain before falling back to
  a recursive tail rule. SMT performance vs. spec readability trade-off.
- Whether the recursive rule is anonymous (inlined at the use site) or named
  (extracted to a top-level rule with a synthesised identifier). Naming
  affects both readability and SMT behaviour.

**Unlocks**:
General `while` support. L5 extends the loop-body machinery; L6 begins
folding existing summaries.

---

### Milestone 5: loop-break-continue

**Definition of Done**:
IR1 SSA loop bodies admit `break` and `continue` statements, represented via
the continuation handles from L1. Both bounded-quantified and fixed-point
lowering paths emit Pant output consistent with the early-exit semantics the
gameplan settles on. The continuation-merge model extends the prior
workstream's M2 branch-continuation work without regressing M2 tests. At least
two break-bearing and two continue-bearing fixtures pass `pant` and (where
applicable) `pant --check`.

**`while (true) + break` event-loop idiom in scope.** L4 rejects literal-true
guards with a "no observable termination condition" diagnostic because the
recursive Pant equation `loop(s) = loop(body(s))` is non-informative under
SMT verification (infinitely many models, no further constraint). L5 unlocks
this idiom: when the body contains a reachable `break` (or `throw` / `return`)
guarded by a state-dependent condition, the early-exit becomes the loop's
observable termination condition. The fixed-point lowering encodes it as
`fix (λs. if ¬break(s) then body(s) else s)` (or the continuation-handle
equivalent — see Open Questions below). At least one of L5's break-bearing
fixtures must be a `while (true) { ...; if (cond) break; }` shape so the
diagnostic L4 surfaced for this idiom resolves to a translating fixture in L5.

**Why this is a safe pause point**:
Loop-shape coverage is feature-complete for this workstream's terminal scope.
Existing summary-based μ-search and foreach lowering remains unchanged.

**Operator Actions Before Next Milestone**:
- Run the full corpus and the prior workstream's regression suite. Confirm
  break/continue inside non-loop branch joins (M2 territory) still behaves
  correctly.
- Decide whether L6's unification is worth pursuing immediately or whether to
  ship the workstream here and defer unification to a follow-on. The
  unification's value is architectural cleanup, not new TS coverage.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- Whether `break` lowers as early termination of the fixed-point function
  (`fix (λs. if guard(s) ∧ ¬break(s) then body(s) else s)`) or as a
  continuation handle consumed by the post-loop join. Affects the L4
  fixed-point encoding retroactively if the latter is chosen.
- Whether `continue` is represented as a body-internal goto-to-header or as
  a body-internal short-circuit. Both encodings exist in the SSA literature.
- How nested loops scope their break/continue targets.

**Unlocks**:
Summary unification (L6) can target the now-complete general-loop machinery.

---

### Milestone 6: loop-summary-unification

**Definition of Done**:
The existing μ-search summary lowering in `tools/ts2pant/src/ir1-ssa-loops.ts`
is recharacterised as a specialisation of the general-loop machinery (bounded,
with the μ-search ranking metric and a single-equation body shape). foreach
Shape A and Shape B summaries are similarly recharacterised. The corresponding
builder paths in `translate-body.ts` route through the unified general-loop
builder. All prior μ-search and foreach fixtures pass with semantically
equivalent Pant output. Where output differs byte-for-byte, the change is
documented in the gameplan and explicitly reviewed; semantic equivalence is
verified by `pant --check` parity on the old and new output for each affected
fixture.

No new TS loop classes become supported or unsupported by this milestone.

**Why this is a safe pause point**:
One unified loop machinery exists. The prior dedicated summary paths still
physically exist as thin adapters atop the general machinery, ready to be
removed in L7. Reverting to the L5 state is a one-patch rollback.

**Operator Actions Before Next Milestone**:
- Walk the full ts2pant corpus. Confirm zero semantic regressions in
  prior-shipping fixtures.
- For every fixture whose Pant output drifted byte-for-byte, confirm
  `pant --check` accepts both old and new and that the new output retains the
  prior fixture's intent.
- If any fixture fails semantic-parity review, do not proceed to L7 — instead
  rework L6 to preserve semantically equivalent output for that fixture.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- The exact recharacterisation mapping: μ-search → which general-loop shape;
  foreach Shape A → which; foreach Shape B → which. Each mapping must be
  exhibited on at least one existing fixture before the gameplan executes.
- Whether byte-equivalent output is a hard requirement or whether
  semantically equivalent output (`pant --check` parity) is sufficient.

**Unlocks**:
Code removal in L7.

---

### Milestone 7: legacy-rip-and-invariants

**Definition of Done**:
The thin adapters left in place at the end of L6 are removed from
`tools/ts2pant/src/ir1-ssa-loops.ts`. The file is either deleted entirely or
retains only general-loop helpers. `tools/ts2pant/AGENTS.md` and
`workstreams/ts2pant-ir1-ssa.md` are updated to note that μ-search and foreach
summaries are now general-loop SSA specialisations.

New invariant tests cover the general-loop machinery:

- Every loop-header join has exactly one preheader input and one loop-back
  input.
- Every loop body resolves break/continue continuations to the correct
  post-loop or header join.
- Every bounded loop carries an extracted termination metric; every fixed-point
  loop does not.
- Every modified rule from a loop body suppresses frame generation exactly
  once.

Full ts2pant unit and integration suites pass.

**Why this is a safe pause point**:
The workstream is complete. There is one loop machinery. Legacy summary paths
are gone. Invariants are guarded by tests that target the unified abstraction.

**Unlocks**:
Future loop-shape work (do-while, labelled break, switch with fall-through)
builds directly on the unified machinery instead of extending special-case
paths.

## Dependency Graph

```text
1 (general-loop-contract) → []
2 (bounded-counter-loop-lowering) → [1]
3 (bounded-while-lowering) → [2]
4 (fixed-point-while-lowering) → [3]
5 (loop-break-continue) → [4]
6 (loop-summary-unification) → [5]
7 (legacy-rip-and-invariants) → [6]
```

Strictly sequential. The lowering family changes between L3 (bounded) and L4
(fixed-point), so L4 cannot start without L3's quantification machinery to
inherit. L5 (break/continue) and L6 (unification) both require the loop
machinery to be feature-complete before they touch it.

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| L2/L3 merge | Resolved: keep separate. The recognizer surface is structurally different between `for` and `let`-then-`while` shapes; the statement-list peephole is the only new build-pass machinery and does not merge cleanly with `buildL1ForCounterMutation`. See `tools/ts2pant/docs/intermediate-representation.md` section `## Bounded while loop lowering (L3)`. | Resolved in L3 |

Other design questions are scoped to specific milestones and resolved during
their respective `write-gameplan` invocations. See each milestone's
"Open Questions" subsection.

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| Two distinct lowering targets (quantified + fixed-point) instead of one | Quantified equations are clean and verifiable for bounded loops; fixed-point definitions are necessary for unbounded `while`. Forcing one shape onto both classes degrades either readability (quantification over unbounded ranges) or solver tractability (fixed-point for trivial counters). |
| Summary unification is in scope, not deferred | Leaving μ-search and foreach as parallel paths recreates the same architectural debt the prior workstream's M6 ripout eliminated. A single loop machinery is the goal. |
| break/continue is in scope, not deferred | Real TS code uses early exits routinely; deferring substantially limits the workstream's value. The continuation-merge machinery from the prior workstream's M2 provides the foundation. |
| No standalone design-doc milestone | The workstream itself is the design document. Design decisions that gate a specific milestone live in that milestone's "Open Questions" subsection and are resolved during `write-gameplan`'s open-question dialogue. Worked-example `.pant` files are test fixtures introduced by their owning milestone, not standalone artifacts. |
| Sequential, not parallel, milestone graph | Each milestone validates or extends the loop machinery's lowering surface. Parallel work risks landing inconsistent design assumptions in different milestones. |
| Carry forward IR1 SSA decisions | Location SSA (not local-variable SSA), opaque versions, rule-oriented frames, semantic parity rather than byte-for-byte output parity — all inherit from `workstreams/ts2pant-ir1-ssa.md` and are not relitigated. |
| No expression-path cleanup as a prerequisite | The audit confirmed `IR1Expr → IRExpr → OpaqueExpr` accepts loop guards, counter steps, and bound expressions without restructuring. Loop work pulls on the expression path as needed. |
