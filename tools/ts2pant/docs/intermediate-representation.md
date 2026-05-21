# ts2pant Intermediate Representation

> Companion to `tools/ts2pant/AGENTS.md`. Covers the two-layer IR
> (L1 imperative + L2 expression), the IR1 SSA framework that backs
> the mutating-body path, and the per-milestone state of the imperative-IR
> workstream. The SSA Foundations subsection is the theoretical grounding
> for L4 (fixed-point lowering) and any future SSA work; see also
> `workstreams/ts2pant-general-loop-ssa.md`.

ts2pant lowers TypeScript through a two-layer IR before emission:
**Layer 1** (`src/ir1.ts`) is a TS-shape imperative IR where
normalization passes collapse syntactically equivalent surface forms
(increment spellings, conditional families, iteration families) into a
small canonical vocabulary, and **Layer 2** (`src/ir.ts`) is a
Pant-shape expression IR. The pure / value-position path goes
TS → L1 → L2 → `OpaqueExpr`; the effect / statement-position path
goes TS → L1 statement → IR1 SSA → `PropResult[]` (no L2 statement
vocabulary by design).

The IR exists because pre-IR ts2pant accumulated ~18 surface-syntax
recognizers in `translate-body.ts` that cross-talked through
`BodyResult`, ad-hoc symbolic-execution state, and the document-wide
`SynthCell` / `NameRegistry`. Each new TS pattern (μ-search, optional
chaining, Set mutation, …) added another recognizer plus state-merge
logic; the PR #84 post-mortem in `AGENTS.md` is the canonical example
of what that produced. The structural fix — lift the recognizers
behind a typed IR so each surface pattern lowers via a TS→L1 rewrite
and the Pant emitter reads only IR forms — was motivated by **IRSC**
(Vekris, Cosman, Jhala, "Refinement Types for TypeScript", PLDI 2016):
*"FRSC, while syntactically similar to TS, is not entirely suitable
for refinement type checking in its current form, due to features like
assignment. To overcome this challenge we translate FRSC to a
functional language IRSC through a Static Single Assignment (SSA)
transformation"* — exactly ts2pant's situation.

The IR1 SSA workstream completed the lift by replacing
`SymbolicState` as the semantic model for mutating bodies with a
location-based SSA framework (see § "SSA Foundations" below).
Production mutating-body lowering is `buildSupportedSsaMutatingBody`
plus `lowerL1BodyToSsaProps`; `SymbolicState` survives only as a
build-pass coordination record (e.g., the const-binding inline chain
and the in-progress writes map), not as the semantic ground truth.
Deviations from IRSC live in § "Divergence from IRSC" — the most
load-bearing being ts2pant's choice of *location SSA* over IRSC's
variable SSA.

## Files

- `src/ir.ts` — `IRExpr` ADT with constructor helpers (Layer 2).
- `src/ir-build.ts` — TS-AST → L2 IRExpr (pure-path; always-on
  post-M6).
- `src/ir-emit.ts` — L2 IRExpr → `OpaqueExpr` lowering.
- `src/ir1.ts`, `src/ir1-build.ts`, `src/ir1-build-body.ts`,
  `src/ir1-lower.ts`, `src/ir1-lower-body.ts`,
  `src/ir1-ssa-scalars.ts`, `src/ir1-ssa-collections.ts`,
  `src/ir1-ssa-loops.ts` — Layer 1 (TS-shape imperative IR today;
  the SSA-bearing contract now lives in `src/ir1.ts`, while the
  scalar SSA builder/lowerer helper owns scalar property mutation,
  the collection SSA helper owns Map/Set mutation, and the loop
  summary helper owns μ-search plus supported foreach summaries.
  `translate-body.ts` builds supported mutating bodies through
  `buildSupportedSsaMutatingBody` and lowers them through
  `lowerL1BodyToSsaProps`, which returns final propositions plus frames
  from SSA `modifiedRules`).

## Two paths, one Layer 1

Post-workstream, ts2pant has exactly two translation paths and one
shared Layer 1 IR:

- **Pure / value-position** — TS → L1 expression → L2 `IRExpr` →
  `OpaqueExpr`. The single-rooted `IRExpr` tree fits "one expression
  out". Always-on; there is no opt-in flag and no legacy expression
  fallback for this path.
- **Effect / statement-position** — TS → L1 statement →
  `buildSupportedSsaMutatingBody` → `lowerL1BodyToSsaProps` →
  `PropResult[]` for supported SSA-backed bodies. Scalar property
  mutation, read-after-write, branch joins, and scalar early-exit merges
  route through `src/ir1-ssa-scalars.ts`; Map/Set mutation routes
  through `src/ir1-ssa-collections.ts`; and μ-search plus supported
  foreach summaries route through `src/ir1-ssa-loops.ts`. General loop
  SSA remains out of scope. The mutating output is a list of equations +
  frame conditions; no L2 statement vocabulary.

L2 is *expression-only* — there is no L2 `IRStmt`. The asymmetry is
intentional; see `workstreams/ts2pant-imperative-ir.md`
§ "Architectural Lessons" for the rationale.

## `IRExpr` — 10 forms

| Form | Lowers to | TS shapes that produce it |
|------|-----------|---------------------------|
| `Var(name, primed?)` | `ast.var` / `ast.primed` | identifier; primed for next-state references in mutating bodies |
| `Lit(literal)` | `ast.litNat` / `ast.litBool` / `ast.litString` | numeric / string / boolean literal |
| `App(head, args)` | `ast.app` / `ast.binop` / `ast.unop` (head-dispatched) | binops, method calls (receiver as first arg), qualified field accessors, builtins (`in`, `#`, `=`, comparison) |
| `Cond([(g, v)])` | `ast.cond` | ternary, early-return if-conversion, `??` lowering, conditional mutation merge |
| `Let(name, value, body)` | substituted out at emit (Pant has no `let`) | `const x = e1; ... return e2` const-binding inlining |
| `Each(binder, src, [g], proj)` | `ast.each` | for-of Shape A/B, `?.` lowering, `.filter`/`.map` chain |
| `Comb(combiner, init?, each)` | `ast.eachComb` (with optional binop fold for non-identity init when the combiner is a fold; min/max take no init) | `.reduce` (fold combiners), source-iterating min/max comprehensions |
| `CombTyped(combiner, binder, binderType, guards, proj)` | `ast.eachComb` over a typed binder (no source) | μ-search (`min over each j: T, guards \| j`) — the canonical lowering target produced by `recognizeAndLowerMuSearch` in `ir1-lower.ts` |
| `Forall(binder, type, guard?, body)` | `ast.forall` | `all x: T \| ...` quantifier emission |
| `Exists(binder, type, guard?, body)` | `ast.exists` | `some x: T \| ...` quantifier emission |

Both layers are **closed vocabularies.** Every `IRExpr` is one of the
ten canonical constructors above, and every `IR1Expr` is one of the
canonical L1 constructors. Constructions that fall outside the
canonical vocabulary reject as `unsupported`; there is no opaque
catch-all that would smuggle uncanonicalized TS shapes through the
pipeline.

## SSA Foundations

ts2pant's mutating-body lowering builds an SSA form **over memory
locations**, not over local variables. A location is a (rule, receiver,
optional key) tuple — `(balance, a)` for a scalar property, or
`(stringToIntMap, m, k)` for a Map entry. Each location gets its own
chain of versions; reads observe the dominating version; writes mint
a fresh version; branch joins introduce a φ-version when then- and
else-side versions differ; loop-header joins use a two-pass open/close
protocol. The contract surface (next subsection) is the concrete
realisation of this discipline.

**The precedents this work draws on:**

- **Cytron, Ferrante, Rosen, Wegman, Zadeck (TOPLAS 1991)** is the
  canonical SSA construction algorithm — dominance-frontier-based φ
  placement, with the two-pass technique for loop-header joins
  (place φ with placeholder back-edge; emit body; back-patch).
  ts2pant's `ir1SsaJoin` (branch joins, with degenerate-join collapse
  when then- and else-versions coincide) and `ir1SsaOpenLoopHeader` /
  `ir1SsaCloseLoopHeader` (loop joins, with `closed: bool` to prevent
  double-close) implement the algorithm directly.
- **Chow et al. (CC 1996), "Effective Representation of Aliases and
  Indirect Memory Operations in SSA Form"** extends SSA past scalar
  local variables to indirect-memory operations. An access like
  `a.balance` is conceptually `select(balance, a)` and the SSA
  version chain lives on the *result* of that selection. ts2pant's
  property SSA is the direct realisation: `IR1SsaPropertyLocation` is
  `(ruleName, receiver, property)`, and versions are minted per
  location, not per `a`.
- **Knobe & Sarkar (POPL 1998), "Array SSA Form"** does the same for
  indexed memory — one version chain per `(array, index)` tuple, so
  `arr[i] = ...` doesn't shadow `arr[j]`. ts2pant's Map and Set SSA
  (`IR1SsaMapValueLocation`, `IR1SsaMapMembershipLocation`,
  `IR1SsaSetMembershipLocation`) follow this discipline directly —
  `m.set(k1, v1)` and `m.set(k2, v2)` are writes to distinct locations
  and do not alias each other in the SSA graph.
- **IRSC (Vekris, Cosman, Jhala, PLDI 2016)** is the immediate
  TypeScript-side precedent for SSA in a translator targeting
  refinement verification. IRSC uses *variable* SSA in the FRSC→IRSC
  translation; ts2pant deliberately diverges to location SSA — see
  § "Divergence from IRSC" — because Pant's mutating-function
  semantics are next-state relations over named rules
  (`balance' a = ...`), and variable SSA would force a re-projection
  step at every emit boundary.

**Why location SSA, not variable SSA?**

The unit that needs primed/unprimed versions is the rule application,
not a TypeScript variable name. Two TS variables aliasing the same
property (`const ref = a; ref.balance = v;`) write to the same
location; the SSA framework must reflect that without a separate
aliasing analysis. Conversely, the same TS variable name flowing
through two different parameter positions in two separate function
calls should *not* share a version chain — versions are scoped to the
emission frame, not the TS lexical scope. Location SSA gets both
properties for free; variable SSA would need an alias analysis pass to
recover the first and a fresh-supply discipline to recover the second.

**Why two-pass for loop-header joins?**

Inside a loop body, reads of a mutated location must observe a version
that depends on the back-edge — but the back-edge version is the
result of emitting the body, which is also what the read appears in.
The Cytron two-pass protocol resolves this circularity: emit the
header join with a placeholder `loopBackVersion: null`, emit the body
referring to the join's `joinVersion`, then back-patch the loop-back
version. `closed: bool` on the header prevents accidental double-close
or use-before-close. The general-loop SSA workstream's L1 milestone
added this vocabulary (`ir1SsaOpenLoopHeader`, `ir1SsaCloseLoopHeader`)
and L2 wired it through `lowerCounterLoopL1Body`.

**Dominating reads and reaching definitions.**

`IR1SsaRead.dominated` records whether the read is dominated by the
write whose version it observes — the standard SSA reaching-
definitions discipline (Cytron 1991 §3). At branch arms: a read in
the then-arm that observes a then-arm write is dominated; a read
after the join that observes the join-version is dominated by the
join (which dominates post-join code); a read attempting to skip the
join and reach into one arm is not dominated and the invariants test
flags it as a malformed SSA program. The runtime contract is
`tests/ir1-ssa-invariants.test.mts`.

**Frame conditions via `modifiedRules`.**

Lamport's UNCHANGED idiom and Borgida's frame-problem treatment
(both cited in `AGENTS.md` § "Key References") carry over from the
pre-SSA emitter — but the modified-set computation is now driven by
SSA bookkeeping rather than by syntactic scanning of the emitted Pant.
The SSA-era invariant `framedRules = declaredRules \ modifiedRules`
is enforced at `IR1SsaProgram` construction. This is the structural
fix the IR1 SSA workstream landed: the modifiedRules set is
*constructed alongside* the writes, not derived by parsing the output.
A future change that touched a property without going through the SSA
write helpers would fail to register in `modifiedRules` and the
over-strict frame would contradict the actual write — that's the
failure mode the contract is defending against.

## IR1 Substitution Discipline

All IR1-level substitution goes through `substituteIR1ExprSubtree` /
`substituteIR1StmtSubtree` in `tools/ts2pant/src/ir1-substitute.ts`.
Hand-rolled walkers in this layer are forbidden — review will reject
any IR1 substitution that does not use the primitive.

When a future patch introduces a new IR1 binder form, the same patch
must extend the primitive's binder-site enumeration so capture-checking
remains exhaustive. The OCaml-backed `ast.substituteBinder` covers the
L3 lowered `OpaqueExpr` Var-by-name case; the TS-side primitive covers
IR1's Var and Member subtree cases. Together they enforce
capture-avoidance discipline across every substitution surface in
ts2pant.

## `IR1` SSA contract surface

Milestone 1 adds the SSA-bearing type vocabulary and constructor helpers
in `src/ir1.ts`. The key exported names are:

- Types: `IR1SsaLocation`, `IR1SsaVersion`, `IR1SsaRead`,
  `IR1SsaWrite`, `IR1SsaJoin`, `IR1SsaLoopSummary`, `IR1SsaProgram`,
  `IR1SsaValue`.
- Location helpers: `ir1SsaPropertyLocation`,
  `ir1SsaMapValueLocation`, `ir1SsaMapMembershipLocation`,
  `ir1SsaSetMembershipLocation`, `ir1SsaRuleOfLocation`.
- Version helpers: `ir1SsaInitialVersion`.
- Read / write / join helpers: `ir1SsaRead`, `ir1SsaWrite`,
  `ir1SsaJoin`, `ir1SsaLoopSummary`.
- Value helpers: `ir1SsaPropertyValue`, `ir1SsaMapSetValue`,
  `ir1SsaMapMembershipValue`, `ir1SsaSetMembershipValue`,
  `ir1SsaSetClearValue`.

These helpers establish the contract surface and enforce structural
choices like location-compatible versions and degenerate-join
simplification. They do not change the production builder/lowerer path
yet.

The runtime contract for this abstraction is
`tools/ts2pant/tests/ir1-ssa-invariants.test.mts`, which directly checks
fresh versions, location-compatible joins, dominating reads, frame
partitioning, and unsupported-construct rejection.

## General-loop SSA contract surface (L1)

The general-loop SSA contract milestone extends the dormant IR1 SSA
vocabulary in `src/ir1.ts`. The new exported names are:

- Types: `IR1SsaLoopHeaderJoin`, `IR1SsaLoopBody`,
  `IR1SsaTerminationMetric`, `IR1SsaBreakHandle`,
  `IR1SsaContinueHandle`.
- Constructor helpers: `ir1SsaOpenLoopHeader`,
  `ir1SsaCloseLoopHeader`, `ir1SsaLoopBody`,
  `ir1SsaTerminationMetric`, `ir1SsaBreakHandle`,
  `ir1SsaContinueHandle`.

Loop-header joins use a Cytron-style two-pass protocol. Builders call
`ir1SsaOpenLoopHeader` before emitting the loop body, producing a header
with a placeholder `loopBackVersion: null` and a fresh `joinVersion`.
Body emission should make reads of the mutated location observe that
`joinVersion`. Once the body has emitted its back-edge version, builders
call `ir1SsaCloseLoopHeader` to back-patch `loopBackVersion` and mark the
header closed. Closing validates location compatibility and double-close
throws.

`IR1SsaLoopBody.terminationMetric` is the bounded-vs-fixed-point
discriminant. A non-null metric marks a bounded loop; downstream lowering
will produce quantified equations. A null metric marks a fixed-point loop;
downstream lowering will produce recursive Pant rule definitions. This
milestone defines the vocabulary only and does not implement either
lowering path.

Break and continue handles are version snapshots at early-exit sites.
Later milestones will merge break snapshots at the post-loop join and
continue snapshots into the next iteration's header. The handle
constructors reuse the existing version object and validate that the
snapshot version is location-compatible.

These helpers establish the contract surface for general-loop SSA and
enforce structural choices like closed loop bodies, location-compatible
back-patches, and early-exit version snapshots. They do not change the
production builder/lowerer path yet; production builders and lowerers do
not construct or consume this new vocabulary in this milestone.

## Bounded counter loop lowering (L2)

The bounded counter loop milestone activates the L1 loop SSA vocabulary for
the canonical TS counter-loop surface implemented in
`src/ir1-ssa-counter-loop.ts`.

The quantified accumulator-fold path accepts loops of the form
`for (let COUNTER = INIT_LITERAL; COUNTER CMP BOUND_EXPR; COUNTER++) { BODY }`,
where:

- `CMP` is `<` or `<=`.
- `INIT_LITERAL` is a numeric literal.
- `BOUND_EXPR` is loop-invariant with respect to `COUNTER`.
- `BODY` is a single Shape B-like accumulator-fold assignment to a property
  location, optionally guarded by a single counter-dependent `if` without an
  `else`.

The TS builder may canonicalize equivalent `+1` counter steps before this
module sees them, but the SSA recognizer only accepts the canonical IR1 step
`COUNTER = COUNTER + 1`. Non-canonical init, condition, or step shapes remain
unsupported and should produce diagnostics instead of falling through to an
incorrect lowering.

SSA construction creates one `IR1SsaLoopHeaderJoin` per mutated location. The
builder opens each header with `ir1SsaOpenLoopHeader` before classifying the
body, makes the loop-body write observe the header's `joinVersion`, then
closes the header with `ir1SsaCloseLoopHeader` after the back-edge write
version is known. The emitted `IR1SsaLoopBody.terminationMetric` is the
explicit expression `BOUND_EXPR - COUNTER` with lower bound `0`, encoded with
`ir1SsaTerminationMetric`.

Accumulator folds lower to one Pant equation per mutated property:

```pant
acc--p' obj = acc--p obj OUTER_OP
  (COMB over each i: Nat0, i >= INIT, i CMP_OP BOUND | F(i))
```

The counter binder is the `over each` binder, the init and bound checks are
guards, and an optional TS `if (g(COUNTER))` body guard becomes an additional
over-each guard. Compound assignments use the same Shape B combiner intuition
as foreach accumulator folds: `+=`/`-=` use `+ over`, `*=`/`/=` use `* over`,
and boolean `&&=`/`||=` use `and`/`or` over. Simple counter-only assignments
that do not read their own target location are also accepted by this module;
they lower to last-iteration-wins `cond` equations rather than over-each
aggregates.

The recognizer rejects body shapes that cannot be represented by a single
quantified fold or last-iteration assignment: true recurrences that read the
target location through the loop-header value, multiple body mutations,
guarded bodies with `else`, side-effecting or non-counter steps, assignment
through the counter, unsupported accumulator operators, non-literal init
expressions, conditions other than `COUNTER < BOUND_EXPR` or
`COUNTER <= BOUND_EXPR`, and bounds that mention the counter. Recurrences are
explicitly deferred to L4 fixed-point lowering.

## Bounded while loop lowering (L3)

The bounded while milestone reuses the L2 counter-loop SSA and lowering path
for counter loops spelled as adjacent `let` plus `while` statements. The
mutating-body builder recognizes the pair and desugars it to an `ir1For`, so
`src/ir1-ssa-counter-loop.ts` remains the single lowering source for bounded
counter loops regardless of source spelling.

Accepted ascending forms are the structural twin of L2:

```ts
let i = 0;
while (i < n) {
  body;
  i++;
}
```

The `<=` condition variant is accepted as well. The initializer must be a
numeric literal, the bound expression must be loop-invariant with respect to
the counter, and the trailing body step must canonicalize to `COUNTER =
COUNTER + 1`.

L3 also accepts the symmetric descending forms:

```ts
let i = n;
while (i > 0) {
  body;
  i--;
}
```

The `>=` condition variant is accepted as well. For descending loops, the
bound expression must be a numeric literal, the init expression must be
loop-invariant with respect to the counter, and the trailing body step must
canonicalize to `COUNTER = COUNTER - 1`. The emitted termination metric is
`COUNTER - BOUND_EXPR` with lower bound `0`; this is the descending mirror of
L2's ascending `BOUND_EXPR - COUNTER` metric.

Descending accumulator folds lower to one Pant equation per mutated property
with the counter range encoded as over-each guards:

```pant
acc--p' obj = acc--p obj OUTER_OP
  (COMB over each i: Nat0, i <= INIT, i > BOUND | F(i))
```

For `>=`, the final guard uses `i >= BOUND`. Optional TS body guards become
additional over-each guards, as in L2. Descending simple assignments lower to
last-iteration-wins `cond` equations. For the strict `>` form, the selected
last counter value is `BOUND + 1`:

```pant
p' obj = cond INIT > BOUND => F(BOUND + 1), true => p obj
```

For `>=`, the selected last counter value is `BOUND`. Ascending simple
assignments keep the L2 last-iteration convention with the comparison and
endpoint determined by `<` versus `<=`.

The let-then-while peephole lives in `buildSupportedSsaMutatingBody`, where
the statement-list iterator can look ahead and consume two adjacent
statements. It matches a side-effect-free counter `let`, a following
`while`, and a trailing counter step in the while body. The builder removes
that trailing step from the body and emits `ir1For(initStmt, cond, step,
bodyWithoutTrailingStep)`. These shapes fall through unchanged and continue
to reject through the ordinary statement path: `let` with an effectful
initializer, `let` without a matching following `while`, `while` without a
preceding matched `let`, and `while` whose body does not end in a recognized
counter step.

At the L3 milestone, unsupported `while` loops rejected with one diagnostic:

```text
while loop is not a recognized bounded-counter shape; lift to L4 fixed-point lowering when that milestone ships
```

That message replaced the previous generic `loop assignment` diagnostic for
`WhileStatement` specifically. L4 now routes accepted unbounded `while` loops
to fixed-point lowering and uses the targeted diagnostics below for the
remaining rejected shapes. `ForInStatement` and `DoStatement` still reject with
the older generic loop-assignment message.

## Fixed-point while loop lowering (L4)

The fixed-point while milestone activates the L1 loop SSA vocabulary for
unbounded mutating `while` loops: loops whose termination depends on dynamic
state and whose ranking function cannot be recovered by the L2/L3 bounded
counter recognizers. L4 has two source routes:

- Adjacent `let` plus `while` pairs still try the L3 bounded-counter peephole
  first. If the pair is not a recognized ascending or descending counter loop,
  it falls through to fixed-point lowering instead of rejecting as an
  unsupported `let`.
- Bare `while (P) { BODY }` statements route directly to fixed-point lowering.

`do while`, labelled loops, `break`, and `continue` remain outside this
milestone. Pure expression-position `let` plus `while` μ-search lowering is a
separate expression path and is not changed by L4.

The recognizer is intentionally scalar. It accepts exactly one mutated property
location, requires the guard and update to be expressible over that location's
current value plus loop-invariant parameters, and rejects multi-location loop
bodies. Literal `while (true)` rejects in the build pass with:

```text
while loop has no observable termination condition (literal-true guard); rewrite with a guard that depends on mutated state
```

Multi-location bodies reject with:

```text
fixed-point while lowering supports single-location bodies only; this loop modifies N rules
```

Guards or updates that read outside the mutated property reject with:

```text
fixed-point while lowering supports guards and updates over the mutated property only
```

Successful lowering constructs an `IR1SsaLoopBody` with
`terminationMetric: null`. That null metric is the L1 discriminant for the
fixed-point family; bounded loops continue to carry a non-null
`IR1SsaTerminationMetric` and lower through quantified `over each` equations.
The fixed-point lowerer emits two `PropResult`s: a top-level
`kind: "rule-decl"` for the synthesized recursive helper, and a normal
`kind: "equation"` for the primed caller location.

The helper is named from the enclosing function with the `fn--loop-N` pattern,
using the shared name registry for collision suffixes. Its first parameter is
the current value of the mutated property; remaining parameters are the
loop-invariant values captured from the guard and update. The body is one
Pant `cond`: if the guard still holds, call the helper recursively on the
single-step updated value; otherwise return the current value.

Counter-like fixed-point increment:

```ts
while (a.size < cap) {
  a.size = a.size + 1;
}
```

```pant
fill-up-to--loop-0 value: Int, cap: Int => Int =
  cond value < cap => fill-up-to--loop-0 (value + 1) cap,
       true => value.

account--size' a = fill-up-to--loop-0 (account--size a) cap.
```

Decrement:

```ts
while (a.balance > floor) {
  a.balance = a.balance - 1;
}
```

```pant
drain-to--loop-0 value: Int, floor: Int => Int =
  cond value > floor => drain-to--loop-0 (value - 1) floor,
       true => value.

account--balance' a = drain-to--loop-0 (account--balance a) floor.
```

Accumulator-style update:

```ts
while (a.balance < target) {
  a.balance = a.balance + step;
}
```

```pant
adjust-balance-to--loop-0 value: Int, target: Int, step: Int => Int =
  cond value < target => adjust-balance-to--loop-0 (value + step) target step,
       true => value.

account--balance' a = adjust-balance-to--loop-0 (account--balance a) target step.
```

These examples deliberately use the same recursive-rule shape. L4 does not
pre-unroll a bounded number of iterations into a `cond` chain before falling
back to recursion.

Pant's SMT backend detects a rule whose body syntactically applies the same
rule and emits that declaration as SMT-LIB `define-fun-rec`. Non-recursive
rules keep the existing `declare-fun` plus universal defining-axiom emission.
This preserves the Pant surface while giving Z3 and CVC5 native recursive
function hooks for `pant --check`.

The fixture policy is per case: every accepted fixed-point fixture must pass
Pant typecheck, and each `pant --check` must either succeed or carry a
documented timeout rationale explaining why direct recursive-function solving
is currently acceptable for that loop shape. A fixture timeout is a solver
automation limitation, not permission to change the lowering to bounded
unrolling in this milestone.

## Mutating-body output (no L2 IR)

The mutating path emits `PropResult[]` directly:

- One `kind: "equation"` per modified rule (per-iter for Shape A
  foreach, accumulator-fold for Shape B, single-write for branched
  property mutation).
- One `kind: "assertion"` for empty-Set / empty-Map field
  initializers.
- One frame `kind: "equation"` per unmodified-but-in-scope rule
  (identity equation `prop' obj = prop obj`).

The supported path is `lowerL1BodyToSsaProps` in
`ir1-lower-body.ts`. It combines scalar, collection, and supported
loop-summary lowering results, carries final property writes and
modified rules, and appends frames for declared rules that were not
modified. Production mutating-body lowering is
`buildSupportedSsaMutatingBody` plus `lowerL1BodyToSsaProps`.

## Divergence from IRSC

Deliberate divergences, documented so a future agent doesn't "fix"
them back to paper-faithful IRSC.

**No L2 `FieldAccess` form.** ts2pant lowers `e.f` to L2
`App(qualified-rule, [e])` at construction time via
`qualifyFieldAccess`. Adding a L2 `FieldAccess` form would force
every L2 consumer to check both shapes and reintroduces the cross-
talk problem. M5 reaffirmed this divergence: the L1 `Member(receiver,
name)` form is *normalization*, not lowering — its job is to be the
canonical input shape for the lowering pass, not a 1-to-1 mirror of
L2. Member is L1-only; it lowers to `App(qualified-rule, [receiver])`
in `ir1-lower.ts:77-84`. Pre-qualifying at build time keeps
`qualifyFieldAccess`'s ambiguity-detection and inheritance-walking
logic centralized in one place.

**Cardinality dispatch is a non-Member path.** `.length` on Array /
ReadonlyArray and `.size` on Set / ReadonlySet / Map / ReadonlyMap
build to L1 `Unop(card, receiver)` via `tryBuildL1Cardinality` in
`ir1-build.ts`, NOT through `buildL1MemberAccess`. The dispatch
fires *before* Member during L1 build. Routing through Member would
lower to `App("length", [arr])` — i.e., the Pant text `length arr`,
an EUF uninterpreted function distinct from the actual list
cardinality `#arr`. Specs reasoning about array sizes would silently
fail under that lowering. Pant's primitive for cardinality is `#x`,
which `Unop(card, x)` covers; there is no canonical-Member route
that produces correct output. This is a target-language constraint,
not a style choice — a sibling deliberate divergence to the
no-`FieldAccess` rule.

(IRSC's SSA-over-program-names discipline does *not* apply here —
post-M3 there is no L2 statement vocabulary, and the mutating path's
write-key φ-merge is represented directly in IR1 SSA before final
emission.)

## Invariants

- **Opaque AST constraint.** Every property an IR pass needs to query must be
  a discriminator on the IR ADT, never on the lowered `OpaqueExpr`. No
  syntactic peeking on `OpaqueExpr` from any IR pass.
- **Hygienic binders.** IR binder names (`Let.name`, `Each.binder`,
  `Forall.binder`, `Exists.binder`) come from the document-wide
  `UniqueSupply` / `cellRegisterName`. They cannot collide with parameter
  names or accessor rules. `ast.substituteBinder` (Pant's
  capture-avoiding substitution at the wasm layer) relies on this.
- **L2 is expression-only.** `IRExpr` never carries a write. A method
  call that produces a state mutation is an L1 `assign` / `map-effect`
  / `set-effect` statement, not an L2 expression. The mutation only
  appears in `PropResult[]`.

## Scalar SSA routing boundary

Milestone 2 introduces a dedicated scalar SSA helper module,
`src/ir1-ssa-scalars.ts`, instead of broadening `ir1.ts` or moving the
entire mutating-body path at once. The helper is responsible for the
scalar-only cases that now execute in production:

- direct and compound property assignment
- property read-after-write
- supported `if` branch joins over scalar properties
- scalar early-exit continuation merges

Those cases lower through IR1 SSA and then emit the same primed
equations as before. Map/Set mutation now uses `src/ir1-ssa-collections.ts`;
loop-summary lowering now uses `src/ir1-ssa-loops.ts`.

## Final architecture (post-M6)

The imperative-IR workstream replaced the original 11-stage migration
plan. The end state is two translation paths sharing one Layer 1 IR,
with no migration flags, no escape hatches, and no parallel pipelines:

- **Pure / value-position** (function returns, `.reduce` chain
  results, conditional values, ternary arms, etc.) — TS AST → L1
  expression (`ir1-build.ts`) → L2 `IRExpr` (`ir1-lower.ts`) →
  `OpaqueExpr` (`ir-emit.ts`). Always-on; the `IRExpr` tree is the
  canonical output of every value-position TS construct ts2pant
  supports.
- **Effect / statement-position** (mutating-body assignments, Map/Set
  effect calls, branched mutation, iteration with statement bodies)
  — TS AST → L1 statement (`ir1-build-body.ts`) → `PropResult[]`.
  Scalar property mutation now routes through the dedicated SSA helper
  (`src/ir1-ssa-scalars.ts`), while Map/Set now routes through the
  collection SSA helper (`src/ir1-ssa-collections.ts`) and loop
  summaries route through `src/ir1-ssa-loops.ts`.
  `buildSupportedSsaMutatingBody` and `lowerL1BodyToSsaProps` are the
  production builder/lowerer pair for final emission and frame
  derivation. No L2 statement vocabulary.

Constructions outside the canonical L1 vocabulary reject with a
specific `unsupported` reason. The build pass either produces a
canonical L1 form or rejects — there is no opaque catch-all, no
opt-in legacy pipeline, and no parallel translation path.

Milestone 7 is the runtime-contract layer for this architecture:
`tools/ts2pant/tests/ir1-ssa-invariants.test.mts` is the authoritative
suite for SSA freshness, join compatibility, dominating reads, frame
partitioning, and unsupported-construct diagnostics.

For the per-milestone breakdown of how this state was reached, see
§ "Imperative IR Workstream" below and
`workstreams/ts2pant-imperative-ir.md`.

## Imperative IR Workstream

ts2pant has a layered architecture: **Layer 1** is a TS-faithful imperative
IR (`Block`, `Cond`, `Foreach`, `Assign`, `For`, `While`, `Return`, …) in
`src/ir1.ts` where normalization passes collapse syntactic equivalences
(increment spellings, conditional families, iteration families) into a
small canonical vocabulary. **Layer 2** is `IRExpr` in `src/ir.ts` —
Pant-shaped *expression* IR (post-M3, statement-position lowering bypasses
L2; see § "Two paths, one Layer 1" above). **Layer 3** is `OpaqueExpr`.

The IR1 SSA workstream does not replace this Layer 1 shape all at once.
Milestone 1 adds the SSA contract alongside the existing Layer 1
constructors so later milestones can migrate the builder and lowerer
without changing the surface review now.

Lowering for value-position: TS AST → Layer 1 (`ir1-build.ts`) → Layer 2
(`ir1-lower.ts`) → OpaqueExpr (`ir-emit.ts`).

Lowering for effect-position: TS AST → Layer 1 (`ir1-build-body.ts`) →
`buildSupportedSsaMutatingBody` → `lowerL1BodyToSsaProps` →
`PropResult[]` (with scalar property mutation handled by
`src/ir1-ssa-scalars.ts`, Map/Set mutation handled by
`src/ir1-ssa-collections.ts`, and loop summaries handled by
`src/ir1-ssa-loops.ts`).

The full milestone breakdown lives in
`workstreams/ts2pant-imperative-ir.md`. Decisions on canonical forms,
conservative-refusal policy, hard-rule-per-class migration, and the
`Foreach`-with-statement-body rationale are recorded there.

**M1 (imperative-ir-conditionals): landed.** Conditional value forms —
if-with-returns (single, two-branch, multi-arm chain), ternary chains
(right-associative flatten), switch without fall-through, `&&`/`||` when
both operands are statically Bool-typed — all collapse to a single
canonical `Cond([(g, v)], otherwise)`. The L1 path is always-on; the
legacy `translateIfStatement` and inline ternary handler are deleted.
Switch was previously fully unsupported; it now translates with the
caveat that every case must end in `return EXPR`, default is required
and last, and case labels must be literal. `&&`/`||` Bool-type detection
is in `purity.ts:isStaticallyBoolTyped` (apparent-type walk requiring
every union/intersection constituent to satisfy `BooleanLike`).

**M2 (imperative-ir-assign-mu-search): landed.** `ir1Assign` and
`ir1While` activated. Increment surface forms (`i++`, `++i`, `i--`,
`--i`, `i += k`, `i -= k`, `i = i ⊕ k`, `i = k ⊕ i` for commutative
`⊕`) build to canonical L1 `Assign(target, BinOp(<op>, target, <k>))`
via `buildL1IncrementStep` in `ir1-build.ts`. The five `+1` spellings
(`i++`, `++i`, `i += 1`, `i = i + 1`, `i = 1 + i`) produce *byte-
identical* L1 output — that's the M2 architectural promise. μ-search
recognition and lowering live entirely in the L1/L2 layers
(`isCanonicalMuSearchForm` and `lowerL1MuSearch` in `ir1-lower.ts`,
producing an L2 `comb-typed` expression that emits to OpaqueExpr in
`ir-emit.ts`). The TS-AST `recognizeLetWhilePair` is purely
structural (consumes the let + while pair) — `translate-body.ts`
carries no Pantagruel-target awareness for μ-search. Three
additional `+1` spellings now translate (was just `i++`/`++i`
pre-M2).

**M3 (imperative-ir-iteration-mutation): landed.** Historical M3 note:
branched mutation and iteration flow through Layer 1. The build pass
(`ir1-build-body.ts`) produces canonical L1 statement forms —
`cond-stmt` for `if`-with-mutation, `foreach` for `for-of` / `forEach`
— and the lower pass (`ir1-lower-body.ts`) does a single fold over the
L1, threading the collection SSA from `src/ir1-ssa-collections.ts` for
Map/Set effects and the loop-summary helper from `src/ir1-ssa-loops.ts`
for μ-search and supported foreach / accumulator-fold shapes, then
emitting `PropResult[]`. No L2 statement vocabulary. `Foreach.body`
(Shape A — uniform iterator writes) emits one
universally-quantified per-iteration equation per modified rule
(`all $N in src | prop' $N = …`), while `Foreach.foldLeaves` (Shape B
— accumulator folds `a.p OP= f(x)`) emits one *aggregated* accumulator
equation per leaf with a `comb over each $N in src[, guard] | rhs`
right-hand side. Both flow through the same fold but produce
distinct output shapes. Map/Set effects
inside branches (`m.set(k, v)`, `s.add(e)`, etc.) are first-class L1
forms (`map-effect`, `set-effect`). The legacy iteration recognizers
(`translateForOfLoop`, `translateForOfLoopBody`, `translateForEachStmt`,
`classifyLoopStmt`, `ShapeBLeaf`, `LoopStmtClass`, `FoldOps`) and the
pre-IR1-SSA mutating-body dispatcher (`symbolicExecute`) are all
deleted; the production path is `buildSupportedSsaMutatingBody` +
`buildSupportedSsaStatement` building canonical L1 forms, then
`lowerL1BodyToSsaProps` lowering them through the SSA helpers
(scalars / collections / loop-summary). Pure-path `.reduce` (chain
fusion via `BodyResult.pendingComprehension`) is expression-position
and architecturally separate; it stays on `translateReduceCall`.

**M4 (equality-nullish-normalization): landed.** Every TS expression
in the equality / nullish equivalence class flows through Layer 1.

*Canonical forms:*

- `IsNullish(operand)` — the canonical Bool null/undefined test.
  Recognized from `x == null`, `x != null`, `x === null`,
  `x === undefined` (and the `!==` negations), the long disjunction
  `x === null || x === undefined`, the conjunction-negated long form
  `x !== null && x !== undefined`, and `typeof x === 'undefined'` (and
  `!==`). Negated forms wrap as `unop(not, IsNullish(operand))`.
  Lowers mechanically to the cardinality-zero shape `#x = 0` —
  byte-identical to the pre-M4 hand-emitted shape `??` and `?.`
  already used. The operand may be any L1 expression (not just a
  `Var`). Long-form recognition uses a structural `structurallyEqualExpression`
  walk, not `node.getText()`, so whitespace/comment/quote-style
  variation in the source doesn't break recognition. Partial-match
  disjunctions (`(a === null) || (a === undefined) || other`) recurse
  on the `||`/`&&` operands and extract nullish prefixes
  opportunistically — see `AGENTS.md` § "Developer Steering Principles" rule 2.
- `BinOp(eq | neq, lhs, rhs)` — canonical strict equality. Produced
  unconditionally from `===` / `!==`. The L1 form admits arbitrary
  operands; sub-expressions are built natively on L1 in every
  position.
- Functor-lift `each n in x | f n` — the canonical lowering for
  null-guarded list-lifted conditionals (see `transformations.md`
  § "Functor-Lift Recognizer" for the four soundness conditions).
  Combined-shape match crossing M1 (Cond) + M4 (IsNullish);
  load-bearing because Pant has no list literal — the alternative
  cardinality-dispatch lowering has no expressible target.

*Equality rule:* `===` / `!==` always canonicalize through L1; `==` /
`!=` is rejected unconditionally outside the nullish recognizer. There
is no type-based "safe loose-eq" exception. The rejection is the rule-1
case of `AGENTS.md` § "Developer Steering Principles": loose equality
is ambiguous in TS itself between value-equality and JS-coercion
semantics, and ts2pant steers the programmer toward `===`/`!==` where
intent is unambiguous rather than guess.

The `x == null` carve-out is **path-scoped**, not global:

- *Body / L1 expression-translation path* (`translateBodyExpr` and
  `translateExpr` in `translate-signature.ts`) — the nullish recognizer
  fires *before* the loose-eq rejection dispatcher, so `x == null` and
  `x != null` are folded to `IsNullish` (or its negation) and translate
  successfully.
- *Signature / guard-classification path* (`containsUnsupportedOperator`
  in `translate-signature.ts`, used by `classifyGuardIf` and
  helper-followability checks) — strict: ALL loose equality (`==` /
  `!=`), including `x == null` and `x != null`, returns
  `containsUnsupportedOperator = true` and the guard is **not**
  classified as a guard. The if-statement stays in the body, where the
  body translator then folds it via the recognizer — so the runtime
  check survives, but it's no longer factored out as a precondition.

The asymmetry is deliberate: a guard is a *factored-out* runtime check
with no body, so misclassifying one would silently drop the check on
both sides; classifying conservatively keeps the if-statement intact.

*Sub-expression handling.* `IsNullish` operands and
`BinOp(eq | neq, …)` operands are arbitrary L1 expressions, built
natively in every position via the canonical L1 dispatch. There is
no escape-hatch wrapper; sub-expressions outside the canonical L1
vocabulary reject with a specific `unsupported` reason.

*Out of scope by design* (candidates for future milestones if real
fixtures demand them):

- `.indexOf(x) === -1` array-absence idioms — these are EUF equality
  on a sentinel value, not nullish testing. They translate correctly
  today via `===` canonicalization; they just don't route through
  `IsNullish`.
- `x === SomeEnum.NONE` enum-sentinel patterns — typed equality on a
  named constant. Works as-is via `===` canonicalization; absorption
  into `IsNullish` would require a notion of user-declared "absence"
  values that ts2pant doesn't have.
- `Boolean(x)` / `!!x` truthiness coercion — semantically distinct
  from nullish testing (truthy includes `0`, `""`, `false`). Tests
  presence in JS's truthy-coercion sense, not absence in the nullish
  sense. May stay UNSUPPORTED indefinitely or become its own
  milestone.

*Pessimism rate (loose-eq rejection):* N=5 loose-equality sites across
fixtures + dogfood, all consumed by the nullish recognizer. 0 rejected.
**0%.** No fixture or dogfood site exercises a non-nullish loose-eq
that would surface the rejection path; rejection is exercised by
synthetic cases in `tests/equality-canonicalization.test.mts`.

**M5 (property-access-normalization): landed.** Every TS property-
access surface form ts2pant supports flows through Layer 1.

*Canonical form:*

- `Member(receiver, name)` — the canonical L1 property-access form.
  Receivers are arbitrary L1 expressions; `name` is pre-qualified at
  build time via the existing `qualifyFieldAccess` (inheritance
  walking, union/intersection ambiguity detection, anonymous synth-
  cell fallback — all centralized in one place). Lowers mechanically
  to L2 `App(qualified-rule, [receiver])` in `ir1-lower.ts` —
  byte-identical to the pre-M5 hand-emitted shape on every fixture.
  Built by `buildL1MemberAccess` in `ir1-build.ts`, called from all
  9 documented build/translate sites in `translate-body.ts`,
  `translate-signature.ts`, `ir-build.ts`, and `ir1-build-body.ts`.

*Surface forms folded into Member:*

- Dotted access (`obj.field`) — TS `PropertyAccessExpression` with an
  `Identifier` / `PrivateIdentifier` argument.
- String-literal element access (`obj["field"]`, ``obj[`field`]``) —
  TS `ElementAccessExpression` whose argument expression is a
  `StringLiteral` or `NoSubstitutionTemplateLiteral`. Operationally
  equivalent to dotted access at the TS-checker layer; one canonical
  L1 form. Templates with substitutions (e.g., ``obj[`f${i}`]``) are
  not literal keys — they fall through to the computed-access reject.

*Computed element access rejected.* `obj[expr]` for any non-literal
`expr` rejects with a specific `unsupported` reason. The DoD's
"unless type system resolves to a known field set" carve-out (literal-
union narrowing on the index expression) is deferred to a follow-up:
the soundness conditions (partial unions, branded strings, generics-
resolving-to-literal-types-only-at-call-site) are non-trivial enough
to warrant their own slice.

*Type-erasure wrappers preserved.* `(x as T).f`, `x!.f`, `<T>x.f`,
`(x satisfies T).f` are **not** stripped at L1 build, despite being
runtime-erased. They are load-bearing at the TS-checker layer that
ts2pant translates against — `qualifyFieldAccess` calls
`checker.getTypeAtLocation` on the asserted node to honor the user's
contract that the receiver should be treated as the asserted type.
Stripping would silently change the qualifier resolution. Symmetric
strip-in-`buildL1SubExpr` (Patch 5) operates on *outer*-position
wrappers around the sub-expression, not on wrappers inside the
property-access receiver — those stay put.

*Paren-stripping is universal.* `(x)` and `x` produce identical L1
trees at every TS-AST → L1 dispatch entry point — `buildL1Expr`,
`buildL1Stmt`, and the per-kind builders all unwrap parens before
classifying. Downstream recognizers (functor-lift, cardinality, the
nullish recognizer) do not re-implement paren-stripping; they
classify against the unwrapped node uniformly.

*Functor-lift operand restriction lifted (M5 Patch 4).* The
recognizer's eligibility check is now expressed in L1 terms (`Var`
or `Member`) rather than TS-AST terms (`Identifier` only — the M4 P5
restriction). Idiomatic null-guards over Member operands —
`if (obj.field == null) return []; return [obj.field.name];` — now
translate. See `transformations.md` § "Functor-Lift Recognizer" for
the supported shapes.

*Cardinality dispatch is a deliberate non-Member path.* `.length` /
`.size` on the six list-shaped TS types (Array, ReadonlyArray, Set,
ReadonlySet, Map, ReadonlyMap) build to L1 `Unop(card, receiver)`
via `tryBuildL1Cardinality`, NOT Member. The dispatch fires *before*
Member; routing through Member would lower to `App("length", [arr])`
— EUF uninterpreted on a `length` rule, distinct from Pant's
cardinality primitive `#x`. Specs reasoning about array sizes would
silently fail. See § "Divergence from IRSC" above for the canonical
record.

*Mutating-body assignment targets.* The hard rule for the property-
access equivalence class extends to statement-position writes:
`buildL1AssignStmt` constructs `Assign(Member(receiver, name),
value)` where the target is a canonical L1 Member. Compound-assign
desugaring (`a.p OP= v` → `a.p = a.p OP v`) happens before Member
construction, so the Member form is the same whether the source was
a simple assign or compound assign.

*Sub-expression handling.* The four documented property-access
sub-expression sites in mutating-body recognizers
(`ir1-build-body.ts` Shape B fold target / rhs / guard,
`buildL1AssignStmt` receiver) flow through one `buildL1SubExpr`
helper that calls `buildL1MemberAccess` for property-access shapes
and the canonical L1 dispatch for the rest. There is no escape-hatch
wrapper for non-property sub-expressions — every sub-expression is
either a canonical L1 form or an `unsupported` rejection.

*qualifyFieldAccess two-tier behavior preserved as-is.* Resolved-
owner cases produce qualified rule names (`account--balance a`);
unresolved-non-ambiguous cases (built-ins, type parameters,
anonymous-without-synth) fall back to bare kebab'd field names
(`to-fixed n 2`); ambiguous unions return null and reject. The
asymmetry is a deliberate two-tier EUF separation — user-defined
types get qualified rule names because translate-types emits
accessor rules with bodies the SMT solver observes; built-in /
parametric types fall back to bare names because the SMT solver
treats them as EUF uninterpreted functions with congruence (the
right modeling for `arr.indexOf(x)`, `s.charAt(0)`, `n.toFixed(2)`,
etc.). Tightening to always-reject would break a substantial
fraction of TS code that translates fine today; collapsing to
always-qualify would pollute the namespace with bodyless synthesized
rules and lose nothing semantically.

*Pessimism rate (computed-access rejection):* 0 — fixtures and
dogfood use dotted access exclusively; no fixture or dogfood site
exercises computed `obj[expr]` that would surface the rejection
path. **0%.** Rejection is exercised by the deliberate-reject
`getByDynamicKey` fixture function plus unit tests.

**Layering principle.** This is the architectural commitment that
M2 ratifies and the M2 cleanup completes:

1. **L1** (`ir1.ts`, `ir1-build.ts`) — canonicalized TypeScript
   syntax. No Pantagruel-target awareness.
2. **L1 → L2 lowering** (`ir1-lower.ts`) — the *only* layer that
   knows Pantagruel-target patterns (μ-search recognition,
   counter-binder substitution).
3. **L2** (`ir.ts`) — Pantagruel-shaped expression IR. Includes
   `comb-typed` for source-less typed comprehension (μ-search's
   target).
4. **L2 → OpaqueExpr** (`ir-emit.ts`) — mechanical emission.
5. **`translate-body.ts`** — TS-AST orchestrator. Wires lowering
   contexts; no target-language semantics.

Deviations from this principle should be flagged in review and
either justified explicitly or refactored.

**Closed vocabularies on both layers.** Every `IR1Expr` is one of
the canonical L1 constructors, and every `IRExpr` is one of the ten
canonical L2 constructors. Sub-expressions outside the canonical
vocabulary reject with a specific `unsupported` reason — there is no
adapter that smuggles a foreign value into the IR.

**One pipeline, no flags.** The pure expression pipeline is
unconditional; there is no opt-in environment variable or runtime
predicate gating the canonical path. Every value-position TS
construct ts2pant supports flows through L1 → L2 → OpaqueExpr.

**Locked decisions** (re-litigation requires explicit user sign-off):
- Layer 1 vocabulary is locked at M1. Forms can be added in later
  milestones (e.g., `IsNullish` primitive at M4, `IR1FoldLeaf` at M3)
  but existing forms cannot be changed. M3 activated `assign`,
  `foreach`, `cond-stmt`, `map-effect`, `set-effect`; `for`, `throw`,
  `expr-stmt` remain declared but unused (constructors return them;
  lowerers reject).
- **Closed vocabularies on both layers.** Neither L1 nor L2 has an
  escape-hatch form. The build pass either produces a canonical
  L1/L2 value or rejects with `unsupported`.
- **Conservative-refusal policy 3(b).** When the build pass cannot prove
  an equivalence (switch fall-through, `==`-on-non-Bool, default-not-last,
  non-Bool short-circuit, non-literal switch case label, object-literal
  in a value-position arm), the function rejects with a specific
  `unsupported` reason rather than emitting potentially-incorrect Pant.
- **Hard rule per equivalence class** (workstream decision 4): every
  TS construct in a given equivalence class moves to the L1 path in
  one milestone. Half-migrated classes coexisting with legacy
  recognizers are forbidden — that is the cross-talk hazard the IR is
  meant to retire.
- **Mutating-body lowering bypasses L2** (post-M3 architectural
  commitment). The L2 statement vocabulary that the original Stages
  9–11 plan called for turned out to be a compatibility shim mirroring
  `symbolicExecute` line-for-line. The historical M3 implementation
  threaded `SymbolicState` directly into `PropResult[]`, reusing the
  existing mutation primitives. See
  `workstreams/ts2pant-imperative-ir.md` § "Architectural Lessons" for
  the full rationale.
