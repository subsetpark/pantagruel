# Workstream: ts2pant IR1 Capture-Avoiding Substitution

## Vision

Replace ts2pant's per-site hand-rolled IR1 substitution walkers with a single
capture-avoiding substitution primitive that every IR1-level rewrite consumes.
Each new substitution site (functor-lift Member operand rewriting, fixed-point
Member-read replacement, L5's pending break-substitution, future loop-summary
unification) plugs into one well-tested helper instead of re-deriving the
hygiene discipline from scratch. The end state is structural: hand-rolled
recursive substitution at the IR1 layer is forbidden and review-rejected, the
way PR #84's let-elimination post-mortem already disciplines OpaqueExpr-layer
substitution.

## Current State

ts2pant has two IR1-level substitution sites today, both hand-rolled:

- `substituteL1Subtree` in `tools/ts2pant/src/ir1-build.ts` — M5 P4 added this
  for the functor-lift recognizer's Member-operand rewriting (replace `e` with
  the comprehension binder `n` throughout the projection body).
- `substituteMemberReads` in `tools/ts2pant/src/ir1-ssa-fixed-point.ts` — L4
  Patch 3 added this for fixed-point lowering's `Member(receiver, prop) :=
  state-var` rewrite inside the recursive helper body.

Both walkers recurse over IR1 expressions and replace matching subtrees with a
fresh-named replacement. Both rely on per-site fresh-binder allocation against
the document `NameRegistry` to avoid capture. PR #226's review surfaced this
pattern explicitly: the wasm-side `ast.substituteBinder` (Bindlib-backed,
proven capture-avoiding) only substitutes Var-by-name on lowered `OpaqueExpr`
and cannot reach IR1 Member subtrees. The OCaml route is closed for IR1
substitution; the TS-side hand-rolled walkers stand in for it.

The general-loop SSA workstream's L5 (break/continue) will add a third site:
the fixed-point + break encoding `fix (λs. if guard(s) ∧ ¬break(s) then body(s)
else s)` requires substituting body subtrees with break-detection logic. L6
(loop-summary unification) will likely add more as μ-search and foreach
summaries get recharacterised against the general-loop machinery and their
operand rewrites land in the same place.

## Key Challenges

- **Binder-site enumeration must be exhaustive (IR1-only by design)**. The
  substitution primitive must know every IR1 form that introduces a binder
  (`IR1Expr.each` binder, `IR1Stmt.let` binder, `IR1Stmt.foreach` binder, any
  future `Forall` / `Exists` IR1 forms, the loop counter binder in
  `IR1Stmt.for` init). Missing one is a capture bug. The enumeration is the
  load-bearing contract. L2 (`IRExpr`) has its own binder vocabulary (`Let`,
  `Each`, `Comb`, `CombTyped`, `Forall`, `Exists`) but is deliberately out of
  scope — and is *structurally forbidden* from being a substitution target
  once the `ts2pant-l2-typed-mirror` workstream lands (L2 becomes a typed
  mirror with no in-place transformation). See "Decisions Made" for the
  rationale and the two routes future L2-touching substitution work can take.

- **Snapshot equivalence on migration**. Migrating the two existing sites must
  produce byte-identical Pant output on every fixture they currently touch.
  This is the only way to gain confidence that the new primitive is a faithful
  generalisation, not a subtle behavioral change. Snapshot tests gate the
  migration; any drift is a stop-the-line condition.

- **Property-based test coverage**. Capture bugs are silent — they only show
  up when the replacement happens to contain a free variable that collides
  with a binder in the haystack. Hand-written fixtures miss this; the
  systematic way is property-based testing (generate haystacks with random
  binder structure, generate replacements with random free-variable sets,
  assert the post-substitution expression has no captured references).

- **Discipline persistence**. The lesson degrades over time if not enforced.
  Future agents adding new substitution sites will reach for the existing
  walker patterns unless the discipline is documented prominently and
  reviewed. PR #84's post-mortem is the existing template; this workstream
  must add an analogous IR1-layer entry.

- **Scope ceiling: substitution, not full alpha-renaming**. The initial
  primitive substitutes a target subtree (Var or Member) with a closed
  replacement (no free vars whose names could be captured). General alpha-
  renaming of the haystack is out of scope unless a future site demands it.
  Going broader risks the same over-engineering the per-site walkers do.

## Established Precedents

- **convention — Barendregt convention (Barendregt 1984)** —
  https://en.wikipedia.org/wiki/Lambda_calculus#The_Barendregt_convention
  Keep bound and free variable names disjoint, so substitution cannot capture.
  Already cited in `tools/ts2pant/AGENTS.md` § "Opaque AST Constraint" and
  applied via the `$N` hygienic-binder scheme for internal binders. The IR1
  substitution primitive enforces a relaxed form of this: the replacement
  must be closed (no free vars beyond what's syntactically in the replacement
  expression itself), and the binder-site walk must respect shadowing.

- **paper — Baader & Nipkow, *Term Rewriting and All That* (Cambridge 1998)** —
  https://www.cambridge.org/core/books/term-rewriting-and-all-that/F825D78F9EE1C7C0BE2C61D04F89E6DA
  Already cited in `tools/ts2pant/AGENTS.md` for the functor-lift's L1
  operand-rewriting. Chapter 2 (positions, subterms, substitution) is the
  foundational reference: substitution is a partial function over term
  positions; binder scope partitions positions into "open under this binder"
  and "closed". The primitive's structural recursion follows this discipline.

- **pattern — Locally-nameless representation (Charguéraud 2012)** —
  https://boarders.github.io/posts/locally-nameless/
  Already cited in `tools/ts2pant/AGENTS.md` Key References. Mentioned here as
  the alternative we are *not* adopting: locally-nameless represents bound
  variables as de Bruijn indices and free variables as names, which trivially
  eliminates capture. We reject this for IR1 because it would require a
  refactor of the IR1 ADT and the IR1 builder, which is a much larger
  surgery than the substitution primitive justifies. Cited so the design
  decision is conscious.

- **library — Bindlib (OCaml)** — https://rlepigre.github.io/ocaml-bindlib/
  The OCaml-side substitution machinery Pantagruel's `lib/binder.ml` already
  wraps and that `ast.substituteBinder` ultimately calls. Cited as the
  reference model: the TS primitive aims for the same correctness guarantees
  (capture-avoidance by construction, scope-respecting walk) using TS data
  structures. We are not porting Bindlib to TypeScript — no comparable mature
  library exists — but the API shape (`bind_mvar` / `unmbind` / explicit
  binder objects) informs the TS module's interface.

- **pattern — Property-based testing for substitution correctness
  (QuickCheck-style)** — https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
  Capture bugs are silent under hand-written fixtures; the systematic way to
  surface them is generating IR1 expressions with random binder structure and
  random substitutions, then asserting structural properties (no capture, no
  binder leakage, no free-variable creation). The ts2pant test corpus does not
  currently use property-based testing; this workstream introduces one
  property-test file for the substitution primitive specifically. Library
  choice (`fast-check` is the obvious TS choice) is resolved during the
  gameplan's `write-gameplan` dialogue.

## Milestones

### Milestone 1: ir1-substitution

**Definition of Done**:

A new module `tools/ts2pant/src/ir1-substitute.ts` exports a single
capture-avoiding substitution primitive operating on IR1 expressions and
statements, and both existing hand-rolled IR1 substitution sites have been
migrated to it:

*The primitive:*

- `substituteIR1ExprSubtree(haystack: IR1Expr, needle: IR1Expr, replacement: IR1Expr): IR1Expr` —
  replaces every syntactic occurrence of `needle` (a Var or Member subtree)
  within `haystack` with `replacement`, respecting binder scope. Throws
  `CaptureRiskError` if `replacement`'s free vars would be captured by any
  haystack binder.
- `substituteIR1StmtSubtree(haystack: IR1Stmt, needle: IR1Expr, replacement: IR1Expr): IR1Stmt` —
  the same primitive over statements; threads block-scope binders (let,
  foreach, for-init) correctly.
- `freeVarsIR1Expr(expr): Set<string>` and `freeVarsIR1Stmt(stmt): Set<string>` —
  return the set of free variable names. Used by the primitive's capture
  check and by callers needing free-var queries directly.

The module enumerates every current IR1 binder site exhaustively:
`IR1Expr.each` binder, `IR1Stmt.let` name, `IR1Stmt.foreach` binder,
`IR1Stmt.for` init's counter binding. New IR1 binder forms (e.g., the
quantifier forms added by `workstreams/ts2pant-l2-typed-mirror.md` M1)
must extend this enumeration; the module's documentation flags this as
a load-bearing maintenance point.

*The migrations:*

- `substituteL1Subtree` in `tools/ts2pant/src/ir1-build.ts` is removed.
  The functor-lift recognizer (M5 P4 site) calls
  `substituteIR1ExprSubtree` instead.
- `substituteMemberReads` in `tools/ts2pant/src/ir1-ssa-fixed-point.ts`
  is removed. The fixed-point lowerer calls `substituteIR1ExprSubtree`
  instead.

The migrations are **snapshot-equivalent**: every fixture in
`tools/ts2pant/tests/fixtures/constructs/` that previously exercised either
substitution site (functor-lift fixtures: `expressions-functor-lift.ts`,
`expressions-functor-lift-property.ts`; fixed-point fixtures:
`functions-mutating-fixed-point-while.ts`) produces byte-identical Pant
output. `tools/ts2pant/tests/constructs.test.mts.snapshot` requires no
update; if it does, the migration has changed behavior and the gameplan
aborts.

*Tests:*

- Hand-written unit tests for each binder-site and shadowing case
  (~10-15 cases).
- Property-based tests via `fast-check`: generate random IR1 expressions
  with random binder structures, generate random Var-name substitutions,
  assert the post-substitution expression has no captured references and
  preserves all free-variable references that were not the substitution
  target.
- All tests in `tools/ts2pant/tests/ir1-substitute.test.mts`.

*Documentation:*

- `tools/ts2pant/AGENTS.md` and
  `tools/ts2pant/docs/intermediate-representation.md` gain an entry
  documenting the discipline: "all IR1 substitution goes through
  `substituteIR1ExprSubtree` / `substituteIR1StmtSubtree`; hand-rolled
  walkers in this layer are forbidden."
- The PR #84 post-mortem section in `AGENTS.md` gains a sibling entry
  citing PR #226 as the IR1-layer instance of the same lesson.

**Why this is a safe pause point**:

This is the workstream's only milestone. After it lands, the discipline is
established and enforced by the absence of the old walkers. Future
substitution sites (L5 break-substitution, L6 unification, the
`ts2pant-l2-typed-mirror` workstream's new binder forms) have one
clearly-named primitive to call. `git revert` of the gameplan restores the
hand-rolled walkers.

**Operator Actions After Milestone Lands**:
- Run the full ts2pant integration suite including `pant --check` integration
  tests. The migration should produce identical SMT results.
- Validate the AGENTS.md / IR doc additions read clearly to a reviewer
  unfamiliar with the workstream's history. The discipline is only useful if
  future contributors discover it.
- Confirm the next workstream that adds a substitution site (most likely
  `ts2pant-l2-typed-mirror` M1 or general-loop SSA L5) has the new primitive
  on its radar via `requiredContext` when its gameplan is written.

**Unlocks**:
Future workstreams that add IR1 substitution sites consume one well-tested
primitive instead of rolling their own. Reduces the surface for capture bugs
across the rest of the IR1 evolution (L5, L6, any future recognizers).
Specifically: `workstreams/ts2pant-l2-typed-mirror.md` M1 extends the
primitive's binder-site enumeration with new forms (`comb-typed` binder,
`forall` binder, `exists` binder, `comb`'s inner `each` binder) as L1
grows to absorb the L2 vocabulary. That workstream depends on this one
landing first.

**Open Questions** (resolved during `write-gameplan` for this milestone):
- The exact API surface — separate `Expr` / `Stmt` entry points (recommended)
  vs one polymorphic entry point.
- How to signal capture risk — throw `CaptureRiskError` (recommended) vs
  return tagged result vs alpha-rename automatically.
- Test file structure — single file with separate describe blocks
  (recommended) vs separate property-test file.
- Patch granularity — one patch per migrated site (recommended, parallelisable)
  vs both migrations bundled.

## Dependency Graph

- Milestone 1 -> []

Single milestone. The primitive's implementation and the migration of both
existing sites are atomic: no human pause, no observation window, no
decision-based-on-outcome between them. The orchestrator can execute every
patch back-to-back with no intervention; the dependency graph lives inside
the single gameplan via `dependsOn` between patches (primitive implementation
patch precedes the migration patches; the two migration patches run in
parallel).

## Open Questions

| Question | Notes | Resolve By |
|----------|-------|------------|
| TS property-test library | `fast-check` is the recommendation; confirm during the gameplan's `write-gameplan` dialogue whether the project wants to adopt it more broadly or scope it to this workstream's tests. | During gameplan |
| Substitution under non-closed replacements | The initial primitive rejects this case; revisit if a future workstream's substitution site needs it (alpha-renaming the haystack on capture risk). Not blocking. | If/when L5 / L6 / future workstream needs it |

## Decisions Made

| Decision | Rationale |
|----------|-----------|
| TS-side primitive, not OCaml-side via wasm | Lifting IR1 into OCaml-side representation and using Bindlib would be architecturally cleaner but requires duplicating the IR1 ADT in OCaml and crossing the wasm boundary for every builder call. The TS-side primitive's effort is comparable to a single careful module; the OCaml-side route is a multi-week refactor with little additional payoff. |
| Reject locally-nameless representation as the IR1 internal form | Locally-nameless eliminates capture by construction, but refactoring `IR1Expr.each` / `IR1Stmt.let` / `IR1Stmt.foreach` to use de Bruijn indices for bound vars while keeping named free vars is a much larger surgery than the substitution primitive justifies. The named-binder representation is widely understood by contributors and stays compatible with the existing build / lower / emit code paths. We accept the operational discipline (fresh-name allocation against scope, primitive walks respect shadowing) as the cost of the simpler representation. |
| Single primitive, not a library of substitution patterns | Initially the primitive handles Var-by-name and Member-subtree substitution because those are the two patterns we have. Resist the temptation to add `substituteIR1Let`, `substituteIR1Each`, etc. as separate APIs — one primitive that takes a needle and a replacement, parameterised by the comparison function, is enough. Adding APIs creates surface area without adding capability. |
| Property-based testing, not exhaustive enumeration | Hand-written fixtures miss capture bugs because they have to specifically construct them. Property-based testing surfaces them by generating diverse haystack / replacement combinations. The cost is one test file using `fast-check`; the gain is a real safety net for the primitive that hand-written fixtures cannot provide. |
| Migrations are snapshot-equivalent, not behaviorally-equivalent-via-test | Snapshot tests are byte-level — a single character of drift in emitted Pant fails the snapshot. This is the highest-precision regression signal we have; behavior-equivalent-via-test (running fixtures and checking they translate without errors) is too loose. Snapshot equivalence is the gating signal for the migration patches; any drift aborts the gameplan. |
| No standalone design-doc milestone | The workstream itself is the design document. Design decisions live in the workstream's `Established Precedents`, `Decisions Made`, and `Open Questions` sections, resolved during `write-gameplan`. |
| Single milestone (one gameplan), not two | An earlier draft of this workstream split the work into M1 (primitive lands dormant) and M2 (migrate existing sites). Re-examined against the gameplan atomicity criteria, the split could not be justified: no human intervention happens between primitive landing and migration, no observation window is needed (the primitive's correctness is established by tests within the same patches), and no decision is conditioned on M1's outcome (M2's migration is mechanical given the new API). The split protected against a hypothetical "the primitive turned out wrong" that's not differentiated by milestone boundary — if the primitive is wrong, snapshot tests catch it in the migration patches regardless. One atomic gameplan with the migration patches dependent on the implementation patch is the correct structure. |
| Scope: IR1 only, not IR2 | L2 (`IRExpr` in `tools/ts2pant/src/ir.ts`) has binder-introducing forms (`Let`, `Each`, `Comb`, `CombTyped`, `Forall`, `Exists`) — the same hygiene questions structurally apply. But no L2-internal substitution sites exist in production: every existing substitution lands at L1 (the sites this workstream centralises) or at L3 (`ast.substituteBinder`, Bindlib-backed). The `ts2pant-l2-typed-mirror` workstream commits L2 to being a typed mirror of OpaqueExpr — a typed staging area, not a transformation surface — and explicitly forbids in-place L2 transformation (including substitution) as a corollary of the typed-mirror principle. Future L2-touching substitution work therefore has exactly two routes: (1) route through L1 if expressible there (this primitive handles it), (2) route through L3 if the substitution is Var-by-name on lowered OpaqueExpr (`ast.substituteBinder` handles it). Extending the primitive to L2 is rejected by design — not deferred, not optional. Designing a generic-over-tree primitive upfront would either over-parameterise the API or force two implementations, neither of which earns the abstraction; and after the typed-mirror workstream lands, the question is structurally closed. |
