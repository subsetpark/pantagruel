# ts2pant Intermediate Representation

> Companion to `tools/ts2pant/AGENTS.md`. Covers the two-layer IR
> (L1 imperative + L2 expression), the IR1 SSA framework that backs
> the mutating-body path, and the per-milestone state of the imperative-IR
> workstream. The SSA Foundations subsection is the theoretical grounding
> for L4 (fixed-point lowering) and any future SSA work; see also
> `workstreams/ts2pant-general-loop-ssa.md`.

ts2pant lowers TypeScript through a two-layer IR before emission:
**Layer 1** (`src/ir1.ts`) is the canonical form after all source-level
recognition. Normalization passes collapse syntactically equivalent
surface forms (increment spellings, conditional families, iteration
families) into a small canonical vocabulary, including Pant-target forms
when source-level recognition needs them. **Layer 2** (`src/ir.ts`) is
L1's expression subset packaged as a typed `OpaqueExpr` mirror. Its
purpose is type-safe construction and snapshot-testable inspection, not
transformation. The pure / value-position path goes
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
  `src/ir1-ssa-counter-loop.ts`, `src/ir1-ssa-fixed-point.ts`,
  `src/ir1-ssa-foreach.ts` — Layer 1 (TS-shape imperative IR today;
  the SSA-bearing contract now lives in `src/ir1.ts`, while the
  scalar SSA builder/lowerer helper owns scalar property mutation,
  the collection SSA helper owns Map/Set mutation, and the loop
  helpers own bounded counters, fixed-point while, and supported
  foreach lowering.
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
  through `src/ir1-ssa-collections.ts`; bounded counter loops route
  through `src/ir1-ssa-counter-loop.ts`; fixed-point while loops route
  through `src/ir1-ssa-fixed-point.ts`; and supported foreach loops route
  through `src/ir1-ssa-foreach.ts`. The mutating output is a list of equations +
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
| `CombTyped(combiner, binder, binderType, guards, proj)` | `ast.eachComb` over a typed binder (no source) | μ-search (`min over each j: T, guards \| j`) — mirrored from L1 `comb-typed` after TS-AST → L1 recognition |
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
  `IR1SsaWrite`, `IR1SsaJoin`, `IR1SsaProgram`, `IR1SsaValue`.
- Location helpers: `ir1SsaPropertyLocation`,
  `ir1SsaMapValueLocation`, `ir1SsaMapMembershipLocation`,
  `ir1SsaSetMembershipLocation`, `ir1SsaRuleOfLocation`.
- Version helpers: `ir1SsaInitialVersion`.
- Read / write / join helpers: `ir1SsaRead`, `ir1SsaWrite`,
  `ir1SsaJoin`.
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

## Local-binding SSA

Local bindings in pure-function preludes and supported mutating bodies use
the same scalar SSA pipeline as property writes. The prelude scanner accepts
each `VariableDeclaration` in a `const` or `let` statement, preserves
declaration order, and emits one `IR1Let` per declaration. The
`inlineConstBindings` substitution path is gone: initializers are no longer
duplicated into the return expression, so `extractReturnExpression` no
longer asks `expressionHasSideEffects` whether a binding is safe to inline.
`var` remains rejected by design because its hoisting and function-scope
semantics do not match the block-local SSA model.

`IR1SsaLocation` includes a local binding variant:

```ts
{ kind: "local-binding"; name: string }
```

This location is versioned like `property`, `map-value`, and
`set-membership`, but it has no rule name, no primed counterpart, and no
framing obligation. A local-binding write records an
`IR1SsaLocalBindingValue`; reads of `IR1Var(name)` resolve to the current
version when `name` has a local-binding location in the scalar SSA state.

Mutable `let` reassignment lowers to `IR1Stmt.assign` whose target is
`IR1Var(name)`. `lowerScalarAssign` treats that target as a write to the
same `{ kind: "local-binding"; name }` location allocated by the declaration,
so every `x = e`, compound assignment, and `++`/`--` step bumps the local
binding's version exactly as property writes bump property-location
versions. Destructuring assignment lowers as a sequence of ordinary
var-target assigns, one per bound name, so it uses the same versioning
protocol after the TS binding pattern has been flattened.

Control-flow joins are shared with the rest of scalar SSA. Branch
reassignment flows through the cond-stmt machinery: each arm writes its own
local-binding version, and the join introduces the version selected by the
condition. Loop-bodied reassignment flows through the general-loop SSA
pipeline with the location class set to `local-binding`, so the loop summary
versions the local variable alongside property, map-value, and
set-membership locations without a separate loop mechanism.

Closure-captured reassignment is deliberately outside this pipeline. The
reassignment recognizer classifies it separately and rejects it before IR1
lowering with a dedicated diagnostic. The recognizer under-accepts
conservatively: ambiguous writes are treated as mutation candidates rather
than risking an incorrectly immutable local binding.

Lowering emits one body equation per versioned binding before the function's
return equation. A TypeScript chain like:

```ts
export function scorePlusOne(account: Account): number {
  const balance = account.balance;
  const next = balance + 1;
  return next;
}
```

lowers through:

```ts
IR1Let("balance", Member(Var("account"), "account--balance"))
IR1Let("next", Binop("+", Var("balance"), Lit(1)))
Return(Var("next"))
```

and emits the split Pant shape:

```pant
balance = account--balance account.
next = balance + 1.
score-plus-one account = next.
```

That split form is intentionally visible in snapshots. Existing
const-binding fixtures gain intermediate equations instead of preserving the
old inlined shape, and newly accepted call initializers that still lack Pant
declarations fail later as `free-call-decl` rather than as prelude purity
rejections.

## General-loop SSA contract surface (L1)

The general-loop SSA contract milestone extends the dormant IR1 SSA
vocabulary in `src/ir1.ts`. The new exported names are:

- Types: `IR1SsaLoopHeaderJoin`, `IR1SsaLoopBody`,
  `IR1SsaTerminationMetric`, `IR1SsaBreakHandle`,
  `IR1SsaContinueHandle`, `IR1SsaReturnHandle`,
  `IR1SsaThrowHandle`.
- Constructor helpers: `ir1SsaOpenLoopHeader`,
  `ir1SsaCloseLoopHeader`, `ir1SsaLoopBody`,
  `ir1SsaTerminationMetric`, `ir1SsaBreakHandle`,
  `ir1SsaContinueHandle`, `ir1SsaReturnHandle`,
  `ir1SsaThrowHandle`, `ir1SsaReturnValueLocation`.

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

Break, continue, return, and throw handles are version snapshots at
early-exit sites. Break snapshots merge at the post-loop join, continue
snapshots feed the next iteration's header, return snapshots feed the
function return-value continuation, and throw snapshots become
iteration-precondition guards. The handle constructors reuse the existing
version object and validate that the snapshot version is
location-compatible. Return values use a synthesized per-function
`return-value` location so return handles keep the same per-location shape
as break and continue handles.

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

## Loop early-exit semantics (L5)

L5 consumes the L1 handle lists for loop-internal `break`, `continue`,
`return` (bare and value), and `throw`. The lowering keeps the L4
recursive-rule shape for fixed-point loops and the L2/L3 quantified shape
for continue-only bounded loops; early exits are represented as
continuation snapshots rather than reconstructed from source positions.

Handle consumption is fixed by handle kind:

- `break` snapshots synthesize a post-loop `cond` over the captured
  location versions. For fixed-point loops, the recursive helper from
  § "Fixed-point while loop lowering (L4)" returns the break snapshot when
  the break guard fires; otherwise it follows the ordinary recursive step.
- `continue` snapshots thread into the loop-header phi's loop-back input.
  In the L2/L3 quantified route this is the "skip this iteration" case:
  the projection uses the previous accumulator value under the continue
  guard and the ordinary body value otherwise.
- `return` snapshots feed a function-level return-value `cond`. Bare
  returns carry only the function-exit guard; value returns also write the
  synthesized `return-value` location before the function-level emission.
- `throw` snapshots emit as iteration-precondition guards, conjoined with
  the recursive rule's existing guard. This matches the existing TS
  `if (bad) throw ...` precondition pattern outside loops; try/catch remains
  out of scope.

Routing is intentionally conservative. Any bounded counter or bounded while
loop containing `break` or `return` bumps to the L4 fixed-point route so
there is one termination-handling path. Continue-only bounded loops stay on
the L2/L3 quantified route because "skip this iteration" composes with the
existing over-each/fold shape. Throw is handled in the fixed-point route as
an iteration precondition.

The L4 literal-true rejection narrows accordingly: `while (true)` still
rejects when the body has no reachable `break` or `return`, but the common
event-loop shape `while (true) { ...; if (cond) break; }` now translates
because the break guard is the observable termination condition.

Labeled `break LABEL` and `continue LABEL` remain future work. They require
a label table and target-resolution pass that the L5 handle lists do not
need for unlabeled, innermost-loop exits, so the builder rejects labeled
forms with the M7 diagnostic.

## Loop summary unification (L6+L7)

L6 and L7 landed as one terminal milestone for the general-loop SSA
workstream. Foreach Shape A and Shape B no longer lower through a parallel
summary record; they now build ordinary `IR1SsaLoopHeaderJoin` +
`IR1SsaLoopBody` structures through `lowerForeachShapeAAsGeneralLoop` and
`lowerForeachShapeBAsGeneralLoop` in `src/ir1-ssa-foreach.ts`.

The `IR1SsaTerminationMetric` type is now a discriminated union. Counter
loops and bounded-while loops keep the numeric
`{ kind: "ssa-termination-metric"; expr; lowerBound }` variant. Foreach
uses `{ kind: "ssa-iterating-source-metric"; source }`, which records the
finite iteration source directly instead of inventing a counter binder.
Fixed-point while loops continue to carry `terminationMetric: null`.

Shape A remains the per-element quantified equation:

```pant
all x in arr | p' x = e x.
```

Each mutated location still gets a loop-header join so the loop-body
contract is uniform, but the close is degenerate: the header join's
`loopBackVersion` is the same SSA version as the per-iteration write. The
write does not feed a next iteration's value; the source-level meaning is
Pant's native `all x in arr` quantification.

Shape B remains the accumulator-fold equation:

```pant
p' a = p a + (+ over each x in arr | f x).
```

Here the write is the standard Cytron inductive case: the accumulator's
body write feeds back through the loop-header join, and lowering emits the
same combiner-over-each right-hand side as before. Shape B keeps the outer
operator and the comprehension combiner separate so non-commutative outer
updates remain explicit.

The legacy summary path is gone. `IR1SsaLoopSummary`,
`ir1SsaLoopSummary`, `IR1SsaProgram.loopSummaries`,
`lowerMuSearchSummary`, the foreach summary adapters, and the old loop
summary module were deleted. μ-search is recognized upstream while
building L1 (`buildL1MuSearchCombTyped`) and lowers as a typed
comprehension expression, not through loop SSA.

The terminal invariant suite now guards the unified abstraction:

- every loop-header join has one preheader input, one loop-back input, and
  is closed;
- every loop body resolves its break and continue handles to the expected
  join target;
- bounded counters and bounded while loops carry the counter metric,
  foreach Shape A/B carry the iterating-source metric, and fixed-point
  while carries no metric;
- every rule modified by a loop body appears in `modifiedRules` exactly
  once, so frame generation is suppressed exactly once.

Existing foreach fixture output stayed byte-equivalent across the
recharacterisation; the snapshot suite is the gate for that promise.

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
loop lowering results, carries final property writes and
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
loop lowering now uses `src/ir1-ssa-counter-loop.ts`,
`src/ir1-ssa-fixed-point.ts`, and `src/ir1-ssa-foreach.ts`.

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
  collection SSA helper (`src/ir1-ssa-collections.ts`); bounded counter
  loops, fixed-point while loops, and foreach loops route through their
  dedicated general-loop helpers.
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
`src/ir1-ssa-collections.ts`, bounded counters handled by
`src/ir1-ssa-counter-loop.ts`, fixed-point while handled by
`src/ir1-ssa-fixed-point.ts`, and foreach handled by
`src/ir1-ssa-foreach.ts`).

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
recognition now fires upstream while building L1: `recognizeLetWhilePair`
consumes the let + while pair and constructs L1 `comb-typed` through
`buildL1MuSearchCombTyped`. L1 → L2 then lowers that form mechanically to
L2 `comb-typed`, which emits to `OpaqueExpr` in `ir-emit.ts`. Three
additional `+1` spellings now translate (was just `i++`/`++i` pre-M2).

**M3 (imperative-ir-iteration-mutation): landed.** Historical M3 note:
branched mutation and iteration flow through Layer 1. The build pass
(`ir1-build-body.ts`) produces canonical L1 statement forms —
`cond-stmt` for `if`-with-mutation, `foreach` for `for-of` / `forEach`
— and the lower pass (`ir1-lower-body.ts`) does a single fold over the
L1, threading the collection SSA from `src/ir1-ssa-collections.ts` for
Map/Set effects and the foreach general-loop helper from
`src/ir1-ssa-foreach.ts` for supported foreach / accumulator-fold shapes, then
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
(scalars / collections / loops). Pure-path `.reduce` (chain
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

**Layering principle.** L1 is the canonical form after all source-level
recognition. L2 is L1's expression subset packaged as a typed
`OpaqueExpr` mirror; its purpose is type-safe construction and
snapshot-testable inspection, not transformation. All recognition and
target-shape decisions happen at TS-AST → L1; L1 → L2 is a mechanical
1:1 lowering.

1. **TS-AST → L1** (`ir1-build.ts`, `ir1-build-body.ts`) — source-level
   recognition, canonicalization, and target-shape decisions. This is
   where recognizers build canonical L1 forms such as `is-nullish`,
   `each`, or `comb-typed`.
2. **L1 → L2 lowering** (`ir1-lower.ts`) — mechanical expression mapping
   from canonical L1 forms to their L2 mirrors or to structural L2 shapes
   for already-canonical L1 forms.
3. **L2** (`ir.ts`) — typed mirror of the `OpaqueExpr` construction
   surface. Includes `comb-typed`, `forall`, and `exists` because L1 has
   matching canonical forms; excludes transformation-only machinery.
4. **L2 → OpaqueExpr** (`ir-emit.ts`) — mechanical emission.
5. **`translate-body.ts`** — TS-AST orchestrator. Wires lowering
   contexts; no target-language semantics.

### L1 → L2 Recognition Discipline

L1 → L2 carries no recognition decisions. New recognizers must fire at
TS-AST → L1 and produce canonical L1 forms (for example, `comb-typed`).
Review will reject any L1 → L2 transformation that is not a 1:1
mechanical mapping or a structural rewrite of an already-canonical L1
form.

### L2 Is Not A Substitution Target

L2 is not a substitution target. Any transformation that wants to rewrite
an L2 expression is asking the wrong question: either build the canonical
L1 form upstream and substitute there using the IR1 substitution primitive
from `workstreams/ts2pant-ir1-substitution.md`, or lower to `OpaqueExpr`
and use `ast.substituteBinder`. The typed-mirror principle has no
exceptions; substitution-disguised-as-construction is still a
transformation.

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
