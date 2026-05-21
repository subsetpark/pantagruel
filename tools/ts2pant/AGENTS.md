# ts2pant Development Guide

## First Principles: This Is Program Translation, Not Novel Research

ts2pant is a **source-to-source program translator** from TypeScript to a specification
language (Pantagruel). Every transformation it performs — variable substitution, control
flow flattening, state update encoding, frame condition generation — has been studied
extensively in the programming languages and verification literature.

**Do not reason ad-hoc from first principles.** Before implementing any transformation:

1. **Name it.** Find the standard name for what you're doing (let-elimination,
   if-conversion, guarded commands, EUF encoding, etc.)
2. **Find the algorithm.** There is almost certainly a canonical algorithm in a textbook
   or paper. Use it. Do not invent a novel approach.
3. **Follow the invariants.** Standard algorithms come with known correctness conditions
   (e.g., Barendregt convention for substitution, congruence for EUF). Verify your
   implementation maintains them.
4. **Reference your sources.** When adding a new transformation, update this file with
   the relevant references so future agents can verify and extend the work.

Ad-hoc approaches produce subtle bugs that standard algorithms are specifically designed
to prevent. See the "PR #84 Post-Mortem" section below for a concrete example where
5 distinct bug categories arose from reimplementing capture-avoiding substitution without
following the literature.

### Key References

These cover the full scope of ts2pant's translation work:

| Topic | Reference | Relevance |
|-------|-----------|-----------|
| Let-inlining, substitution | Peyton Jones & Marlow, ["Secrets of the GHC Inliner"](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf), JFP 2002 | Capture-avoiding substitution, Barendregt convention, unique supply, inlining heuristics |
| A-Normal Form | Flanagan et al., ["The Essence of Compiling with Continuations"](https://dl.acm.org/doi/10.1145/155090.155113), PLDI 1993 | ANF as the representation TS code is approximately in; basis for early-return desugaring |
| If-conversion | Allen et al., "Conversion of Control Dependence to Data Dependence", POPL 1983 | Flattening control flow (early returns, if/else chains) into conditional expressions |
| Uninterpreted functions | Kroening & Strichman, *Decision Procedures*, 2nd ed., Springer 2016, Ch. 4 | EUF theory for translating function calls; congruence closure |
| SMT encoding (practical) | Bjorner, [*Programming Z3*](https://theory.stanford.edu/~nikolaj/programmingz3.html) | Practical guide to EUF, arithmetic theories, quantifiers in Z3 |
| Guarded commands | Dijkstra, ["Guarded Commands, Nondeterminacy and Formal Derivation of Programs"](https://dl.acm.org/doi/10.1145/360933.360975), CACM 1975 | Foundation for conditional mutation translation, weakest preconditions |
| Primed variables, frame conditions | Lamport, [*Specifying Systems*](https://lamport.azurewebsites.net/tla/book.html), Addison-Wesley 2002 | TLA+ approach to next-state relations, `UNCHANGED`, and the frame problem |
| Frame problem in specifications | Borgida et al., ["And Nothing Else Changes"](https://www.researchgate.net/publication/221555223_And_Nothing_Else_Changes_The_Frame_Problem_in_Procedure_Specifications), IEEE TSE 1995 | Definitive treatment of frame conditions in procedure specifications |
| Capture-avoiding substitution | [Locally Nameless Representation](https://boarders.github.io/posts/locally-nameless/) (Charguéraud 2012) | Alternative to named substitution; useful background for understanding why hygiene matters |
| Term rewriting | Baader & Nipkow, *Term Rewriting and All That*, Cambridge 1998 | Positions, subterms, first-order substitution; basis for the functor-lift's L1 operand-rewriting (`body[e := n]`) and the structural matcher in `substituteL1Subtree` |
| Partial functions / option types | [Dafny Reference Manual](https://dafny.org/dafny/DafnyRef/DafnyRef) | Nullable types, preconditions, `modifies` clauses — practical verification language patterns |
| IRSC / SSA-based IR | Vekris, Cosman, Jhala, ["Refinement Types for TypeScript"](https://arxiv.org/pdf/1604.02480), PLDI 2016 | Lifting surface-syntax recognizers into a typed intermediate representation via SSA-style translation; precedent for the IR introduced in §"Intermediate Representation" |
| SSA construction (φ placement) | Cytron, Ferrante, Rosen, Wegman, Zadeck, ["Efficiently Computing Static Single Assignment Form and the Control Dependence Graph"](https://doi.org/10.1145/115372.115320), ACM TOPLAS 1991 | The canonical SSA algorithm: dominance-frontier-based φ-function placement; two-pass back-patching for loop-header joins. Both ts2pant's branch joins (`ir1SsaJoin`) and loop-header joins (`ir1SsaOpenLoopHeader` / `ir1SsaCloseLoopHeader`) implement this discipline. See [`docs/intermediate-representation.md`](docs/intermediate-representation.md) § "SSA Foundations". |
| Memory-SSA / indirect-memory SSA | Chow, Chan, Liu, Lo, Streich, ["Effective Representation of Aliases and Indirect Memory Operations in SSA Form"](https://doi.org/10.1007/3-540-61053-7_72), CC 1996; Knobe & Sarkar, ["Array SSA Form"](https://doi.org/10.1145/268946.268956), POPL 1998 | Extending SSA past local variables to indirect-memory and indexed-memory accesses — the direct precedents for ts2pant's *location SSA* discipline. `IR1SsaLocation` versions one chain per (rule, receiver, optional key) tuple, not per TS variable name. See [`docs/intermediate-representation.md`](docs/intermediate-representation.md) § "SSA Foundations". |
| Partial correctness | Hoare, ["An Axiomatic Basis for Computer Programming"](https://doi.org/10.1145/363235.363259), CACM 1969 | Hoare triples `{P} c {Q}` and partial-correctness semantics — the foundation under every "rule with body propositions" the translator emits. Pant's mutating-body output (`prop' obj = ...` + frames) is the next-state form of Hoare's `Q` clause specialized to property-location postconditions. |
| Fixed-point computation | Tarski, ["A Lattice-Theoretical Fixpoint Theorem and Its Applications"](https://doi.org/10.2140/pjm.1955.5.285), Pacific J. Math. 1955 | The mathematical foundation behind uninterpreted invariant predicates, CHC, abstract interpretation domains, and inductive/coinductive predicates. Every monotone operator on a complete lattice has a least and greatest fixpoint; "the invariant" is the unknown in such an equation. See § "Foundational Pattern: Uninterpreted Predicates and Implicit Definition" below. |
| Constrained Horn Clauses | Bjørner, Gurfinkel, McMillan, Rybalchenko, ["Horn Clause Solvers for Program Verification"](https://doi.org/10.1007/978-3-319-23534-9_2), Fields of Logic II 2015 | Modern industrial form of "solve for the invariant predicate" — encode each loop as a recursive predicate over (pre-state, post-state); dispatch to Spacer (built into Z3) or Eldarica. Reserved for the general-loop SSA workstream's L4 milestone. |
| Refinement type inference | Rondon, Kawaguchi, Jhala, ["Liquid Types"](https://doi.org/10.1145/1379022.1375602), PLDI 2008 | Liquid types are CHC for type inference — refinement variables `κ` are uninterpreted predicates whose meaning is fixed by subtyping constraints. Same mechanism as CHC loop verification, applied at the type level. Co-authored by Jhala, also an IRSC author. |

## Foundational Pattern: Uninterpreted Predicates and Implicit Definition

A recurring technique across formal methods is to **name an unknown
predicate and define it implicitly through the constraints it must
satisfy.** The predicate has no body at the point of declaration; its
meaning emerges from the surrounding constraint system, and a solver
computes a witness. This is one of the most generalisable techniques
in mathematics and CS, appearing in dozens of guises:

- **Loop invariants** in Hoare logic — `I` is whatever makes
  `{I ∧ B} body {I}` and `{I ∧ ¬B} ⇒ Q` close.
- **Skolem functions** in first-order proof — `f` is whatever satisfies
  `∀x. P(x, f(x))` after eliminating an existential.
- **Type variables** in parametric polymorphism — `α` is whatever the
  caller picks; unification is the constraint solver.
- **Refinement variables** in Liquid Types — `κ` is a fresh
  uninterpreted predicate constrained by subtyping; the solver is a
  CHC engine.
- **Constrained Horn Clauses (CHC)** for program verification — each
  loop becomes a recursive predicate constrained by base and inductive
  clauses; Spacer / Eldarica solve.
- **Inductive predicates** in Coq / Agda — `P` is the least fixpoint
  of its constructors.
- **Bisimulation relations** in process calculi — `R` is the greatest
  relation satisfying the indistinguishability constraint.
- **Frame conditions** in separation logic — `F` is whatever is left
  over and commutes with the operation.
- **Abstract interpretation domains** — characterised by their Galois
  connection with the concrete domain, never constructed directly.

The unifying mathematics is **Tarski–Knaster**: every monotone operator
on a complete lattice has a least and a greatest fixed point. The
"uninterpreted predicate" is the unknown in a fixed-point equation;
the constraint system is the operator; the solver computes the
fixpoint. Three things make this pattern transformative:

1. **Specification / computation separation.** You specify a set (e.g.,
   the reachable states) by the property it must satisfy, without
   constructing it. The predicate can be substituted, conjoined, and
   reasoned about symbolically before it has any concrete meaning.
2. **Compositionality.** Frame rules, modular verification, and
   parametric reasoning all rely on being able to manipulate the
   uninterpreted predicate symbolically.
3. **Decoupled search.** The solver is a *generic* search engine over
   the predicate lattice. Swap Z3 for CVC5 for Spacer; the spec source
   doesn't change. This is also why `define-fun-rec` and CHC look
   identical at the spec level — they are two solvers over the same
   constraint system.

### Pantagruel's latent surface

The technique is already expressible in Pantagruel's existing syntax,
without any language extension:

- A rule declared `=> Bool` with no body propositions **is** an
  uninterpreted predicate.
- Universal quantification with implication
  (`all x: T | constraint -> predicate(x).`) is a universal Horn
  clause.
- Recursive Bool rules (a predicate that references itself in its
  body) close the inductive case of a CHC encoding.

Historically, ts2pant did not exploit this beyond standard EUF dispatch
(which is the special case of "uninterpreted predicate with no
recursive constraints"). L4 fixed-point unbounded-while lowering
(general-loop SSA workstream, `workstreams/ts2pant-general-loop-ssa.md`)
now realizes the pattern directly: the synthesized recursive Pant helper rule
is the implicitly defined predicate/function, the loop-body iteration is the
constraint system, and the SMT backend's `define-fun-rec` emission asks Z3 or
CVC5 to compute the fixed point. A future workstream target also fits:

- **Compositional under-specification.** A future Pant pattern for
  specifying a component's contract without supplying an
  implementation — declare the contract as a bodyless `=> Bool` rule,
  let the caller's proof obligations constrain it via Horn clauses.
  Not currently a translation target; relevant if hand-written Pant
  specs ever exercise modular verification.

### Recognising the technique in code

Whenever you see a bodyless rule declared `=> Bool` and the
surrounding propositions constrain it via universal Horn clauses
(`all x: T | premise(x) -> rule(x).`), you are looking at this pattern.
The current ts2pant emitter does not produce this shape (post-M6
mutating bodies emit equations + frames, not Horn clauses), but L4 now
produces the recursive-rule variant of the same fixed-point technique, and
hand-written Pant specs that exercise modular verification already could.

**Why this section exists.** The grounding for L4 is not "we are
inventing a recursive-rule lowering for `while`" — it is "we are
applying one of the most established techniques in formal methods to
a translation target that already supports it syntactically." A
future agent extending ts2pant past L4 should recognise the technique
by name and pick the encoding (recursive `define-fun-rec`, CHC,
k-induction, BMC unfolding) from the established prior art catalogued
in the workstream rather than reinventing from first principles.
See PR #84 post-mortem below for what happens when this discipline
slips.

## Developer Steering Principles

ts2pant is a translator, not a style guide. But it does have to make a
decision when a TS surface form doesn't unambiguously map to a Pantagruel
target — and the decision is the same in both directions: **be honest
about which side owes the work**.

The two-sided rule:

1. **When TS itself is ambiguous, ts2pant rejects and steers the
   programmer to an unambiguous TS form.** The rewrite is owed to the
   *TS* program, not to Pantagruel — TS would be clearer code if it said
   what it meant. Example: `==` is rejected unconditionally outside the
   nullish recognizer (M4 Patch 3). `a == b` between two arbitrary
   operands is ambiguous in TS regardless of where it ends up — does
   the author want value equality, or do they want JS coercion
   semantics? ts2pant doesn't guess; it asks the programmer to say
   `===` (or `!==`) and move on.

2. **When TS is unambiguous but doesn't fit a pantagruel-shaped
   recognizer, ts2pant adapts to recognize it.** The programmer should
   not be taxed by the tool's preferred input shape — refusing
   idiomatic, unambiguous TS purely to fit ts2pant's recognizer
   internals is the same kind of guesswork-tax in reverse. Example:
   `(a === null) || (a === undefined) || other` is unambiguous,
   idiomatic TS. The nullish recognizer recurses on `||`/`&&`
   operands and extracts nullish pairs opportunistically rather than
   forcing the programmer to rewrite into a recognizer-shaped subset.

**The test:** does the TS code's intent read clearly to a TS reader?
If yes, ts2pant adapts to recognize it. If no, ts2pant rejects with a
specific reason explaining the rewrite the programmer should make to
*their TS*. The principle is structural: the burden of clarity lives
with whichever side of the translation introduced the ambiguity.

**Precedent:** M4 (equality and nullish normalization) is where this
principle was articulated. `==` rejection (rule 1) sits alongside
partial-match disjunction recognition (rule 2) inside the same
milestone — they're two halves of one steering posture, not in
tension. Future milestones inherit this principle.

## Architecture

ts2pant translates TypeScript function bodies into Pantagruel propositions. The main
translation pipeline lives in `src/translate-body.ts`.

Two translation modes:
- **Pure functions**: `f(x) { ... return expr }` -> `f x = <expr>.`
- **Mutating functions**: `f(obj) { obj.prop = val }` -> `prop' obj = <val>.` + frame conditions

### Opaque AST Constraint

Pantagruel expressions are opaque wasm handles (`OpaqueExpr`). We cannot inspect or
traverse them from TypeScript. All substitution must go through `ast.substituteBinder()`,
which is a proper capture-avoiding substitution implemented in OCaml. This is why we
use hygienic `$N` names (Barendregt convention) rather than locally nameless / de Bruijn
indices — we cannot change the representation of bound variables in the opaque AST.

## Transformation Patterns

Each TS construct ts2pant translates has a named transformation pattern
with a standard algorithm and reference. The full catalogue lives in
**[`docs/transformations.md`](docs/transformations.md)**:

- **Let-Elimination** (const binding inlining) — Peyton Jones & Marlow JFP 2002.
- **If-Conversion** (early returns, multi-arm conditionals) — Allen et al. POPL 1983.
- **Uninterpreted Functions** (general function calls) — Kroening & Strichman Ch. 4.
- **Guarded Commands** (conditional mutations) — Dijkstra CACM 1975.
- **Option-Type Elimination** (`??`, `?.`, list-lift) — Dafny / Alloy.
- **Partial Rules** (`Map<K, V>` as guarded rule pair) — McCarthy theory of arrays.
- **Record Returns** (object-literal returns → per-field equations) — Kroening & Strichman Ch. 8.
- **Structured Iteration** (for-of / forEach / reduce → catamorphisms) — Meijer, Fokkinga, Paterson FPCA 1991.
- **Functor-Lift Recognizer** (null-guarded list-lifted conditionals) — Wadler POPL 1992; Baader & Nipkow.
- **Chain Fusion** (`.filter`/`.map`/`.reduce` → one traversal) — Wadler TCS 1990 (deforestation).
- **Kleene Minimization** (`while (P(i)) i++` μ-search) — Kleene 1936.

When adding a new transformation, update `docs/transformations.md` alongside
the code. The naming discipline (standard name + reference + invariants +
implementation pointer) is the load-bearing pattern — see § "First Principles"
above.

## Intermediate Representation

ts2pant's two-layer IR (L1 imperative + L2 expression), the IR1 SSA
framework that backs the mutating-body path, the SSA Foundations that
ground the design choices, and the per-milestone history of the
imperative-IR workstream all live in
**[`docs/intermediate-representation.md`](docs/intermediate-representation.md)**.

Read that file before extending the IR or touching SSA-bearing code:

- **Two paths, one Layer 1.** Pure → TS → L1 → L2 → `OpaqueExpr`; Effect →
  TS → L1 statement → IR1 SSA → `PropResult[]` (no L2 statement vocabulary).
- **IR1 SSA contract.** Location SSA (one version chain per
  `(rule, receiver, optional key)` tuple), branch joins, loop-header joins
  via Cytron-style two-pass open/close.
- **SSA Foundations.** Precedents are Cytron 1991 (φ placement), Chow 1996
  (indirect-memory SSA), Knobe-Sarkar 1998 (Array SSA), and IRSC. ts2pant
  diverges from IRSC's variable SSA to location SSA — see the IR doc for the
  rationale.
- **IRSC divergence.** IRSC's separation between FRSC (TS-shape) and IRSC
  (SSA-pure) serves a verification-soundness purpose because IRSC has a
  downstream consumer: the refinement-type checker. ts2pant has no
  L2-reading verifier; the FRSC/IRSC-style separation here is for type
  safety and inspectability only. L2 is a typed mirror of `OpaqueExpr`, not a
  transformation IR.
- **Imperative IR Workstream.** M1–M5 of the workstream landed; the doc
  records what each milestone delivered and the locked decisions.
- **General-loop SSA Workstream.** L1 (loop SSA contract), L2 (bounded
  counter `for` lowering), L3 (bounded `let`+`while` desugar covering
  ascending and descending counters), L4 (fixed-point unbounded `while`
  lowering through recursive Pant helpers emitted as `define-fun-rec`), and
  L5 (loop early-exit lowering) have
  landed — see `workstreams/ts2pant-general-loop-ssa.md`.

### General-loop SSA contract surface (L1)

The L1 contract lives in `src/ir1.ts` and is documented in
`docs/intermediate-representation.md` § "General-loop SSA contract surface
(L1)". The exported loop SSA surface includes `IR1SsaLoopHeaderJoin`,
`IR1SsaLoopBody`, `IR1SsaTerminationMetric`, `IR1SsaBreakHandle`,
`IR1SsaContinueHandle`, `IR1SsaReturnHandle`, `IR1SsaThrowHandle`, and the
constructor helpers `ir1SsaOpenLoopHeader`, `ir1SsaCloseLoopHeader`,
`ir1SsaLoopBody`, `ir1SsaTerminationMetric`, `ir1SsaBreakHandle`,
`ir1SsaContinueHandle`, `ir1SsaReturnHandle`, `ir1SsaThrowHandle`, and
`ir1SsaReturnValueLocation`.

`IR1SsaLoopBody` carries the loop-header joins, body writes, ordinary joins,
`breakHandles`, `continueHandles`, `returnHandles`, `throwHandles`, and the
optional `terminationMetric`. Non-null metrics route to bounded lowering;
null metrics route to fixed-point lowering.

### Loop early-exit lowering (L5)

Loop-internal early exits use the L1 handle lists. `break` handles are
consumed by the post-loop `cond`, `continue` handles feed the header phi
loop-back input, `return` handles feed the function-level return-value
`cond`, and `throw` handles become iteration-precondition guards conjoined
with the recursive rule guard. Bounded loops containing `break` or `return`
bump to fixed-point lowering; continue-only bounded loops stay on the L2/L3
quantified route. Labeled `break` / `continue` is deferred to M7 and must
reject with the explicit future-work diagnostic.

## PR #84 Post-Mortem: Why Standard Algorithms Matter

The initial const-inlining implementation used ad-hoc string-name substitution rather
than following the standard let-elimination algorithm. This produced 5 rounds of bug
fixes, each patching a symptom of the same root cause:

| Bug | Root Cause | Standard Prevention |
|-----|-----------|---------------------|
| freshBinder picks a name used by a const | No namespace separation | Barendregt convention: hygienic `$N` names |
| Forward const reference silently inlined | Validation interleaved with substitution | Separate TDZ check before translation |
| `balance` const collides with `.balance` accessor | String names collide with target language | Hygienic names cannot appear as property names |
| Property names flagged as variable references | Traversal visits all identifiers | Distinguish binding sites from use sites |
| Counter reuse in nested scopes | Counter from array.length after splice | Monotonic unique supply (closure-based) |

**Lesson:** Every one of these bugs has a known, named prevention in the PL literature.
The refactored implementation using `inlineConstBindings()` with `UniqueSupply` and
right-fold substitution makes all 5 structurally impossible. If you find yourself
inventing workarounds for similar issues, stop and find the standard algorithm.

**PR #226 (IR1-layer instance, 2026-05).** Per-site hand-rolled IR1 substitution
walkers (`substituteL1Subtree` in `ir1-build.ts`; `substituteMemberReads` in
`ir1-ssa-fixed-point.ts`) each carried the same capture-avoidance discipline as
separate per-site code, surfaced for review repeatedly. The fix mirrors PR #84's:
extract one centralised primitive (`tools/ts2pant/src/ir1-substitute.ts`) and
delete the per-site walkers. The canonical discipline is documented in
`tools/ts2pant/docs/intermediate-representation.md`. Standard algorithm:
capture-avoiding substitution with binder-site enumeration (Barendregt convention;
Baader & Nipkow Ch. 2). Future IR1 substitution sites consume the primitive instead
of rolling their own.

## Testing

Always invoke tests through `just` from the workspace root. The `package.json` `test:*` scripts are implementation detail (just dispatches to them) and skip the cross-language dep ordering — running `npm run test:integration` directly will fail with "pant binary not found" unless you've separately built it.

```bash
just ts2pant-test                      # unit + integration
just ts2pant-test-unit                 # unit only (no pant binary needed)
just ts2pant-test-integration          # builds pant first, then runs e2e + dogfood
just ts2pant-test-update-snapshots     # accept current output as new snapshots
just ts2pant-precommit                 # mirror what lefthook runs
```

### Test layout

Tests are split into two suites by their dependency on the pant OCaml binary:

- **`tests/*.test.mts` — unit suite.** Pure translation-logic tests. No pant binary, no dune, runs in a few seconds. Coverage is organised by translation layer: TS-AST recognizers (`translate-body`, `translate-signature`, `translate-types`, `annotations`, `purity`), L1 build (`ir1-build-*`, including `cardinality`, `functor-lift`, `increment`, `nullish`, `parens`, `property`, `state-aware-reads`, `call-purity`), L1 lower (`ir1-lower`, `ir1-printer`), IR1 SSA helpers (`ir1-ssa-scalars`, `ir1-ssa-collections`, `ir1-ssa-loops`, `ir1-ssa-counter-loop`, `ir1-ssa-lower`, `ir1-ssa-propresult-lowering`), IR1 SSA contracts and invariants (`ir1-ssa-contract`, `ir1-ssa-loop-contract`, `ir1-ssa-invariants`, `ir1-ssa-ripout`, `ir1-ssa-printer`), L2 build/lower (`ir-build`, `ir-equivalence`, `emit`), and snapshot/cross-cutting (`constructs`, `m2-musearch-l1`, `l1-plumbing`, `equality-canonicalization`, `name-registry`, `pant-wasm-bundle`, `ast-equal`, `builtins`, `extract`).
- **`tests/integration/*.test.mts` — integration suite.** Each test invokes the `pant` binary (via `assertPantTypeChecks` or `runCheck` from `tests/helpers.mts`): `e2e` (per-fixture end-to-end including `pant --check` SMT verification) and `dogfood` (translates ts2pant's own source with ts2pant).

The split exists for stability, not just performance. `getPantBin()` in `tests/helpers.mts` is **pure** — it never invokes a build. It reads `process.env.PANT_BIN` if set, otherwise checks that `${PROJECT_ROOT}/_build/default/bin/main.exe` exists, and throws with an actionable error otherwise. The build is the responsibility of the workspace-level `just build-pant` recipe (which `just ts2pant-test-integration` depends on). Stale-lock recovery + the `dune build` invocation live in the `build-pant` recipe in the root `justfile`, not in JS — orchestration belongs to the orchestrator.

**Why the split is load-bearing.** Node's test runner uses `--test-isolation=process` by default, spawning one subprocess per test file. The original `getPantBin()` ran `execSync("dune build bin/main.exe")` lazily inside every worker, so 9 fresh subprocesses raced for the same `_build/.lock`. A SIGKILL on any one worker (e.g. user aborting a hung pre-commit) left the lock in a poisoned state, and every subsequent build hung indefinitely. The pre-commit hook hung for 15+ hours during the μ-search PR before this fix landed; the post-mortem is the reason this section exists.

**Reintroducing the lazy-build pattern is forbidden.** If a new test needs the pant binary, route it through the integration suite — don't add an `execSync("dune build")` to `helpers.mts` or any `tests/*.test.mts` file.

**`> UNSUPPORTED:` skip path.** An emitted line beginning with `> UNSUPPORTED:` is a deliberate-rejection signal recognised by `emitAndCheck` (and mirrored on the IR-equivalence path); the document is not wasm-typechecked, but the snapshot still captures the rejection message verbatim. This lets fixtures that exercise an `unsupported`-PropResult emission stay in the snapshot suite without polluting `KNOWN_TYPECHECK_FAILURES` (which is reserved for fixtures whose *non-rejection* output the wasm checker rejects).

### Snapshot files

Each snapshot-based `*.test.mts` has a sibling `*.test.mts.snapshot` (Node test runner's snapshot format). When snapshot tests are moved between directories, their `.snapshot` files must move with them; the runner resolves snapshot paths from the test file's location, so a missing-snapshot error usually means a mismatched move.

Test structure:
- `tests/fixtures/constructs/` — TypeScript fixture files, one per construct category
- `tests/constructs.test.mts` — snapshot tests comparing emitted Pantagruel against expectations
- `tests/translate-body.test.mts` — unit tests for internal translation edge cases
- `tests/translate-signature.test.mts` — signature extraction and guard detection tests
- `tests/integration/e2e.test.mts` — end-to-end pipeline tests including `pant --check` verification
- `tests/integration/dogfood.test.mts` — translates ts2pant's own source with ts2pant; verifies through `pant`
