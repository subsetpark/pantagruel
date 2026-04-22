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
| Partial functions / option types | [Dafny Reference Manual](https://dafny.org/dafny/DafnyRef/DafnyRef) | Nullable types, preconditions, `modifies` clauses — practical verification language patterns |

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

Each transformation ts2pant performs follows a standard algorithm. This section documents
the patterns in use and their invariants.

### Let-Elimination (Const Binding Inlining)

**Standard name:** Let-elimination / let-inlining (inverse of ANF conversion).
**Reference:** Peyton Jones & Marlow, JFP 2002.

Translating `const a = e1; const b = e2; return e3` into a single expression. The
implementation in `inlineConstBindings()` follows three phases:

1. **TDZ validation** (on TS AST, before translation). Reject forward/self references.
   This is a well-formedness check on the source program, not part of substitution.

2. **Translate initializers** (forward pass). Build `scopedParams` incrementally so each
   initializer only sees prior bindings. Use hygienic `$N` names from a `UniqueSupply`.

3. **Right-fold substitution** (inside-out). Substitute the last binding first, then
   second-to-last, etc. Each substitution flows through prior replacements automatically.

**Invariants:**
- `UniqueSupply` is monotonic — never derive counters from array lengths or mutable state
- Hygienic `$N` names cannot collide with property-accessor heads or freshBinder names
- One shared `inlineConstBindings()` for both pure and mutating paths — never duplicate

### If-Conversion (Early Returns, Multi-Arm Conditionals)

**Standard name:** If-conversion / control-flow-to-dataflow conversion.
**Reference:** Allen et al., POPL 1983; Flanagan et al., PLDI 1993.

Flattening `if (c1) return e1; if (c2) return e2; return e3` into `cond c1 => e1,
c2 => e2, true => e3`. Process the statement list **bottom-up**: the final `return e`
is the base case; each `if (c) return e` becomes one arm of a conditional whose
else-branch is the translation of remaining statements.

### Uninterpreted Functions (General Function Calls)

**Standard name:** EUF (Equality with Uninterpreted Functions).
**Reference:** Kroening & Strichman, Ch. 4; Programming Z3.

`foo(a, b)` becomes a function application. The only axiom is **congruence**: equal
arguments imply equal results. If a matching Pantagruel rule exists, the solver uses its
constraints; otherwise the function is universally quantified. No cross-function analysis.

### Guarded Commands (Conditional Mutations)

**Standard name:** Guarded command language / conditional next-state relations.
**Reference:** Dijkstra, CACM 1975; Lamport, *Specifying Systems* Ch. 2-3.

`if (cond) { obj.prop = val }` becomes `prop' obj = cond cond => val, true => prop obj`.
Frame conditions for conditionally-modified variables must include the identity case.
Use the **modifies-set** approach: identify which primed variables appear in the action
body, emit frame conditions (`prop' obj = prop obj`) for everything not in the set.

### Option-Type Elimination (Nullish Coalescing, Optional Chaining)

**Standard name:** Option/Maybe elimination.
**Reference:** Dafny Reference Manual (nullable types, preconditions).

`x ?? y` becomes `cond x ~= Nothing => x, true => y`. `x?.prop` becomes
`cond x ~= Nothing => prop x, true => Nothing`. This is the lifting encoding —
partiality expressed as conditional expressions over the `Nothing` value.

### Partial Rules (Map<K, V>)

**Standard name:** Precondition-guarded partial function; declaration guard;
McCarthy's theory of arrays for the synthesized owner sort.
**Reference:** Dafny Reference Manual (preconditions); Dijkstra, CACM 1975
(guards); Kroening & Strichman, *Decision Procedures* Ch. 7 (arrays as
`select`/`store` over a sort of map handles).

A `Map<K, V>` anywhere in the type language becomes a *pair* of Pantagruel
rules: a Bool-valued membership predicate and a `V`-valued rule guarded by it.
The owner domain depends on where the Map appears.

**Stage A — Map is a declared interface field.** The owner is the user's
interface; the rule name is the field name.

```text
entriesKey c: Cache, k: K => Bool.
entries c: Cache, k: K, entriesKey c k => V.
```

**Stage B — Map is anywhere else** (parameter, return type, nested inside
another Map's V, inside an array/tuple/union). The owner is a *synthesized*
domain, one per unique `(K, V)` per module; naming is `KToVMap` with
compound `K`/`V` mangled (`[String]` → `ListString`, `A + B` → `AOrB`,
`A * B` → `AAndB`).

```text
StringToIntMap.
stringToIntMapKey m: StringToIntMap, k: String => Bool.
stringToIntMap m: StringToIntMap, k: String, stringToIntMapKey m k => Int.
```

Both stages use the same encoding; only the owner differs. `.has(k)` →
membership predicate; `.get(k)` (or `.get(k)!`) → value rule. Pantagruel
stores declaration guards in `Env.rule_guards` and automatically injects
them as antecedents in SMT queries, so uses of the value rule are implicitly
conditioned on the membership predicate. Nested Maps register bottom-up via
recursive `mapTsType` calls: `Map<string, Map<string, number>>` emits
`StringToIntMap` first and then `StringToStringToIntMapMap` whose V
references it.

**Why this encoding, not `V + Nothing`?** Pantagruel has no first-class
`Nothing` expression value and no sum destructuring. With a sum-typed return,
`.has` has no clean translation and `.get` cannot be used in arithmetic /
comparisons without a lifting operation the language doesn't provide. The
guarded-rule encoding trades a small semantic gap (absent keys are
uninterpreted rather than explicitly `undefined`) for a much richer set of
usable specifications.

**Why synthesize a sort per `(K, V)`?** Following McCarthy's theory of
arrays: the synthesized sort is the array sort, distinct values of that sort
are distinct maps (EUF keeps their lookups independent — `sumAt m1 m2 k` is
sound because `m1 ≠ m2` does not imply `stringToIntMap m1 k = stringToIntMap m2 k`),
and the guarded `select` lookup is Dafny-style partial-function discipline.

**Scope:** reads and point-update mutation. Construction (`new Map()`)
and iteration (`.entries`/`.keys`/`.values`/`.forEach`) are unsupported.
See `tests/fixtures/constructs/expressions-map.ts` (Stage A reads),
`tests/fixtures/constructs/expressions-map-params.ts` (Stage B reads),
`tests/fixtures/constructs/expressions-map-mutation.ts` (Stage B
mutation), and `tests/fixtures/constructs/expressions-map-mutation-field.ts`
(Stage A mutation) for supported shapes.

#### Mutation (.set / .delete)

**Standard name:** Point update in McCarthy's theory of arrays (`store`);
TLA+ `[f EXCEPT ![m][k] = v]` with implicit frame.
**Reference:** Kroening & Strichman Ch. 7 (`select`/`store` axioms);
Lamport, *Specifying Systems* Ch. 2–3 (EXCEPT expression and next-state
relations).

`m.set(k, v)` and `m.delete(k)` emit one point-update per call using
Pantagruel's N-ary override `R[(m, k) |-> v]`. The override's ite
expansion (`(= (R_prime m1 k1) (ite (and (= m1 m) (= k1 k)) v (R m1 k1)))`
in SMT) carries the "everything-else unchanged" frame implicitly at
the exact override key — exactly TLA+'s EXCEPT semantics.

Each `.set` on a Map receiver modifies two rules: the value rule `R`
and its membership predicate `Rkey`. One quantified equation is emitted
per modified rule:

```text
all m1: T, k1: K
  | R' m1 k1 = R[(m, k) |-> v] m1 k1.
all m2: T, k2: K
  | Rkey' m2 k2 = Rkey[(m, k) |-> true] m2 k2.
```

For `.delete`, only the membership equation is emitted (with `|-> false`);
the value rule is not restated because Pantagruel's declaration guard
makes the value-rule body vacuous under false membership.

Multiple writes to the same rule inside one execution path accumulate
as distinct `(tuple |-> value)` pairs in a single override expression:
`m.set(k1, v1); m.set(k2, v2)` emits `R[(m, k1) |-> v1, (m, k2) |-> v2]`.
Conditional writes (`if (g) m.set(k, v)`) merge via `cond` over the
per-key override values, with the else-branch fallback being the
current symbolic value already accumulated for that `(m, k)` pair;
this is the pre-state `R m k` only when no earlier write to the same
key has occurred on the path. Quantifier binders come from
the document-wide `NameRegistry` so they stay unique against the
function's own params (which have already claimed `m`, `k`, etc.).

### Structured Iteration (for-of, forEach, reduce)

**Standard name:** Catamorphisms / structural recursion on lists.
**Reference:** Meijer, Fokkinga, Paterson, ["Functional Programming with Bananas, Lenses,
Envelopes and Barbed Wire"](https://maartenfokkinga.github.io/utwente/mmf91m.pdf), FPCA 1991;
Lamport, *Specifying Systems* (TLA+ `\A x \in arr` idiom for per-element next-state).

ts2pant accepts three shapes that are all instances of the catamorphism `foldr : (a -> b -> b) -> b -> [a] -> b`:

**Shape A — uniform iterator write (mutating).**
`for (const x of arr) { x.p = e(x) }` and `arr.forEach(x => { x.p = e(x) })` become
`all x in arr | p' x = e(x).` (universal-quantifier proposition form; binding is Pantagruel's
bare `x in arr` via `ast.gIn`, with empty `params`). The body runs through a fresh
sub-`symbolicExecute` with `x` bound, so conditional writes inside the loop pass through
the standard if-conversion machinery.

**Shape B — accumulator fold (mutating).**
`for (const x of arr) { a.p OP= f(x) }` becomes `p' a = p a OP (combOP over each x in arr | f(x)).`
Single-branch `if (g(x)) { a.p OP= f(x) }` folds `g(x)` into the comprehension as an extra
guard. Supported ops: `+=`, `-=`, `*=`, `/=`. Non-commutative outer ops (`-`, `/`) only appear
outside the comprehension — never as combiners.

**Shape C — pure reduce (expression).**
`arr.reduce((a, x) => a OP f(x), init)` becomes `init OP (combOP over each x in arr | f(x))`,
with `init` elided when it equals the combiner identity (0 for `+`, 1 for `*`, `true` for `&&`,
`false` for `||`). `reduceRight` is accepted only for commutative combiners; non-commutative
ops require acc on the left.

**Invariants:**
- Sub-execution uses a *fresh* `SymbolicState` — the iterator's writes must not leak.
- Shape B writes merge into the outer `state.writes` via `binop(outerOp, priorVal, eachComb)`
  so frame conditions for untouched properties still fire.
- `state.modifiedProps` (shared across state clones) tracks which primed rules were emitted,
  including Shape A equations that bypass `state.writes`; used to compute frame conditions
  correctly when Shape A and Shape B appear in the same body.

### Chain Fusion (.filter / .map / .reduce)

**Standard name:** Deforestation.
**Reference:** Wadler, ["Deforestation: Transforming Programs to Eliminate Trees"](https://homepages.inf.ed.ac.uk/wadler/papers/deforest/deforest.ps), TCS 1990.

`xs.filter(p).map(f).reduce((a, x) => a OP g(x), init)` fuses into a single traversal:
`init OP (combOP over each x in xs, p(x) | g(f(x)))`. Each `.filter`/`.map` returns a
`BodyResult` with `pendingComprehension = { binder, arrExpr, guards }` carrying the chain
state in deferred form; `r.expr` holds the current projection. `bodyExpr(r)` materializes
into `ast.each([], [gIn(binder, arrExpr), ...guards], projection)` at the chain boundary
(return statement, binop operand, const initializer, etc.). `.filter` extends `guards`;
`.map` rewrites the projection via `ast.substituteBinder(callbackBody, callbackBinder, receiver.expr)`;
`.reduce` fuses the pending chain into an `eachComb` instead of triggering materialization.

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

## Testing

```bash
cd tools/ts2pant && npm test                # all tests (node:test runner via tsx)
cd tools/ts2pant && npm run test:update-snapshots  # update snapshot expectations
```

Test structure:
- `tests/fixtures/constructs/` — TypeScript fixture files, one per construct category
- `tests/constructs.test.mts` — snapshot tests comparing emitted Pantagruel against expectations
- `tests/translate-body.test.mts` — unit tests for internal translation edge cases
- `tests/translate-signature.test.mts` — signature extraction and guard detection tests
- `tests/e2e.test.mts` — end-to-end pipeline tests including `pant --check` verification
