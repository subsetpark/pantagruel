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
| IRSC / SSA-based IR | Vekris, Cosman, Jhala, ["Refinement Types for TypeScript"](https://arxiv.org/pdf/1604.02480), PLDI 2016 | Lifting surface-syntax recognizers into a typed intermediate representation via SSA-style translation; precedent for the IR introduced in §"Intermediate Representation" |

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

**Pure-path scope.** `extractReturnExpression`'s prelude scan accepts a
`recognizeEarlyReturnArm` shape — `if (P) return E;` with no `else` and a body
that is exactly one return-with-expression — at any position before the
terminal statement, interleaved with const bindings and recognized μ-search
pairs. Each arm's predicate and value are translated under the scope visible
at its position (so they see prior bindings via hygienic `$N` substitution),
accumulated into `inlineConstBindings`'s `arms: [pred, val][]` field, and
materialized into a single `cond` whose catch-all is the terminal expression.
The if-with-else terminal-position handler is unchanged — it remains the
target for *complete-dispatch* `if/else` returning from both branches.

Constraints (mirrored by `extractReturnFromBranch` for terminal-position
branches): no else, single-return body, side-effect-free predicate and value,
TDZ-clean (no reference to bindings declared after the arm). Arms combined
with a record-typed return are not yet supported — per-field `cond`
decomposition would require every arm value to be an object literal of the
same shape, and is left as a follow-on to keep the if-conversion change
self-contained.

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

**Standard name:** Option/Maybe elimination via Alloy `lone` multiplicity.
**Reference:** Dafny Reference Manual (nullable types, preconditions);
Jackson, *Software Abstractions* 2nd ed. (Alloy multiplicities: `one`, `lone`, `some`).

Pantagruel retired `Nothing` from its user-facing type surface — there is
no writable "absence" type and no sum destructuring. Optionality is encoded
in the type language via **list-lift**: `T | null` / `T | undefined` maps
to `[T]`, a list of length 0 or 1 (Alloy's `lone` multiplicity). Type-level
unions fold the null marker into the list wrapper: `A | B | null` → `[A + B]`.

Under list-lift, `??` and `?.` have a universal lowering — the empty-list
cardinality test `#x = 0` replaces the absent `~= Nothing` check, and list
indexing `(x 1)` replaces singleton extraction:

- `x ?? y` with `x: [T]`:
  - `y: T` (non-nullable default) → `cond #x = 0 => y, true => (x 1)`
    (result `T`).
  - `y: [T]` (nested nullable) → `cond #x = 0 => y, true => x`
    (result `[T]`).
  - `x` not nullable in TS → `x` alone (`??` degenerates; no case-split).
- `x?.prop` with `x: [T]`, `prop: T => U` → `each t in x | prop t`
  (result `[U]`). Functor lift: empty stays empty; singleton becomes
  `[prop v]`. Chains `x?.a?.b` compose as comprehensions over
  comprehensions — each step lifts through the list.

Nullability is determined from the TS type at the AST location (presence
of `null` / `undefined` / `void` in the union), not from the emitted Pant
type — so we avoid misclassifying genuine arrays (`number[]`) as optional.
See `isNullableTsType` and the `QuestionQuestionToken` / `questionDotToken`
branches in `translate-body.ts`.

Optional parameters (`p?: P`) list-lift to `p: [P]` via `mapTsType`'s
union-with-`undefined` handling; `p ?? c` inside the body expands to the
cardinality case-split above, giving one list-lifted signature rather than
multiple arity overloads. This preserves one-source-one-target for
everything reached through the general lowering — no special-case
detection at the signature level.

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

**Why this encoding, not `[V]` list-lift?** `Nothing` is no longer part of
Pantagruel's user-facing type surface, and list-lift (`[V]`, length 0 or 1)
is the type-level answer for plain nullable unions — see "Option-Type
Elimination" above. For Map lookups it would still be unsatisfactory: `.has`
would force `#(get c k) = 1` case-splits at every use site, arithmetic on
`.get(k)` results would need an unfold-the-singleton idiom, and distinct
maps sharing a key would not automatically have independent lookups. The
guarded-rule encoding trades a small semantic gap (absent keys are
uninterpreted rather than explicitly "missing") for a much richer set of
usable specifications — declaration guards inject the membership antecedent
into SMT queries automatically, so `.get` participates in arithmetic and
comparisons without a lifting operation.

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

### Record Returns (Object-Literal Return Expressions)

**Standard name:** Observational specification / field-selector
axiomatization of records.
**Reference:** Kroening & Strichman, *Decision Procedures* Ch. 8
(recursive data structures — records as disjoint-range field selectors);
Dafny Reference Manual (function specifications via per-field
postconditions).

Pantagruel has no record-constructor expression syntax — interfaces are
opaque domains reachable only through per-field accessor rules. A pure
function that returns `{ f1: e1, f2: e2 }` therefore decomposes into one
equation per declared field of the return type, observing the function's
result through each accessor:

```text
f a: A, b: B => NamedInterface.
---
all a: A, b: B | f1 (f a b) = e1.
all a: A, b: B | f2 (f a b) = e2.
```

For a nullary `emptyRecord(): I` the quantifier collapses:

```text
emptyRecord => I.
---
f1 emptyRecord = e1.
f2 emptyRecord = e2.
```

**Empty-set initializer.** Pantagruel has no empty-list literal, so
`new Set()` (only; not `new Set(iterable)`) in a `Set<T>` / `ReadonlySet<T>`
/ `T[]` field position emits the membership-negation form rather than an
equation:

```text
all x: T | ~(x in f_i (f <args>)).
```

This is a universally quantified assertion, not an equation — hence the
new `assertion` kind on `PropResult` in `types.ts`.

**Anonymous record returns — synthesized domain per shape.** A function
whose return type is an inline object literal — `{name: string, reg: NameRegistry}`
— has no interface to decompose against. Mirroring the `Map<K, V>` synth
pattern, ts2pant synthesizes one Pantagruel domain per unique shape,
plus one accessor rule per field:

```text
NameRegRec.
name r: NameRegRec => String.
reg r: NameRegRec => NameRegistry.
registerShape r: NameRegistry, s: String => NameRegRec.
---
name (registerShape r s) = s.
reg (registerShape r s) = r.
```

- **Dedup key** = canonical shape string: fields sorted alphabetically
  by name, paired with their Pantagruel types, joined with `|`. Field-
  order permutations hash to the same key, so `{a, b}` and `{b, a}`
  share a domain.
- **Domain name** = sorted capitalized field names concatenated +
  `Rec` suffix. Empty shape (`{}`) → `EmptyRec`. Collisions resolve
  via `NameRegistry`'s numeric suffixing.
- **Nested shapes compose bottom-up.** `{outer: {inner: string}}`
  registers `InnerRec` first, then `OuterRec` whose `outer` accessor
  returns `InnerRec`. Nested object-literal initializers in body
  position recursively decompose into per-accessor equations
  (`inner (outer f) = "hi".`) since Pantagruel has no record-
  constructor expression.
- **Cross-module composition** uses Pantagruel's module namespacing
  (`ModA::NameRegRec` ≠ `ModB::NameRegRec`). Per-module synth is fine
  — lexical collision is handled by the import machinery at
  `lib/env.ml:213-265`; structural identification across modules is a
  checker concern, not ts2pant's.

**Requirements / rejections.**
- Return type must be an object type — a named interface/class/alias
  or an anonymous `__type`. Unions, function types, and other exotic
  shapes are rejected.
- Every declared field must be present in the object literal; extra
  fields are rejected.
- Property kinds accepted: `PropertyAssignment` with identifier or string
  key, and `ShorthandPropertyAssignment` (`{name}` sugar for
  `{name: name}`). Spread, methods, accessors, and computed keys are
  rejected.
- Initializer expressions go through the normal `translateBodyExpr`
  pipeline (const-inlining, etc.), so arithmetic and accessor reads
  work without extra plumbing. Nested object-literal initializers
  decompose recursively.
- **Known limitation**: accessor rule names are the field names
  directly, and the user's function parameters share that namespace.
  A field named `name` with a parameter also named `name` produces a
  Pantagruel error (parameter shadows accessor rule). Workaround:
  rename params, or use a named interface with distinct field names.

See `tests/fixtures/constructs/expressions-record-return.ts` for the
supported shapes and `tests/dogfood.test.mts` for the self-translation
baseline (`emptyNameRegistry`).

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

### Kleene Minimization (While-Loop μ-Search)

**Standard name:** Kleene μ-operator / unbounded minimization (with an
explicit lower-bound guard `j >= INIT`).
**Reference:** Kleene, *General Recursive Functions of Natural Numbers*,
Math. Ann. 112 (1936); Kroening & Strichman, *Decision Procedures* Ch. 4
(quantifier elimination over integer ranges).

`let counter = INIT; while (P(counter)) { counter++ }` is the canonical
"find the least integer ≥ INIT satisfying ¬P" pattern. Pure-body translation
recognizes this exact statement pair in the prelude scan and emits
`min over each j: Int, j >= INIT, ~P(j) | j` (binder type follows the
active `NumericStrategy`), which is Pantagruel's direct target for
μ-minimization (`ast.eachComb` with `combMin`). The loop counter is
replaced inside the predicate by a fresh comprehension binder, and the
resulting expression flows through the standard `inlineConstBindings`
substitution closure — so any post-loop reference to the counter inlines
the `min over each` directly into the consumer expression.

```text
let suffix = 1;
while (used.has(suffix)) { suffix++; }
return suffix;
// →
foo used = (min over each j: Int, j >= 1, ~(j in used) | j).
```

**Recognizer scope (`recognizeMuSearch` in `translate-body.ts`).** Conservative
syntactic match:
- `let` (not `const`/`var`), single declarator, simple identifier, any
  initializer (translated as the comprehension's lower-bound RHS).
- Immediately followed by a `while` whose body is exactly one statement: an
  `ExpressionStatement` wrapping `i++` or `++i` on the same identifier.
- Predicate must reference the loop counter as a *free variable*
  (otherwise the loop is a no-op or divergence, not a μ-search).
  `expressionReferencesNames` is scope-aware: parameters of nested
  function-likes — arrow functions, function expressions, object/class
  methods, getters/setters, and constructors — all shadow outer
  bindings. Plain identifiers, destructured object/array patterns,
  nested destructuring, and a named function expression's own name are
  all collected. Object-literal property keys and non-computed
  method-name tokens don't count as references.
- Init and predicate must be side-effect-free. The purity screen lives
  alongside the TDZ check in `inlineConstBindings`, not in the recognizer
  itself; it rejects assignments, bare `++`/`--`, and unknown-pure calls.
  `translateBodyExpr` has no handler for `++`/`--`, so without this screen
  `while (used.has(i++)) i++;` would silently lower to garbage.
- Active `NumericStrategy` must be discrete (`IntStrategy`). Under
  `RealStrategy` the comprehension would range over a dense domain while
  `counter++` enumerates `INIT, INIT+1, …`; `translateMuSearchInit`
  returns an explicit error in that case.

Compound bodies (`{ i++; foo(); }`), counter aliasing (`while (P) { j++; }`),
`const` counters, `i += 1` / `i = i + 1` updates, and bare `while` without a
preceding `let` all fall through to `extractReturnExpression`'s normal
rejection path. Extending the recognizer to cover those is straightforward
when a need arises; the canonical `i++` form covers `name-registry.ts`'s
`registerName` and `translate-signature.ts`'s `shortParamName`, the two
μ-search sites in ts2pant's own source.

**Comprehension binder allocation.** The comprehension's binder must
round-trip through Pantagruel's parser (`$N` from `freshHygienicBinder`
does not — `$` isn't legal). When a `synthCell` is plumbed through (the
normal pipeline), `cellRegisterName(synthCell, "j")` yields a kebab-cased,
collision-suffixed name (`j`, then `j1`, `j2`, …). Standalone test paths
without a synthCell fall back to `j${nextSupply(supply)}` — not globally
coordinated with the rest of the document, but locally collision-safe:
`translateMuSearchInit` iterates the supply in a `do`/`while` against
`new Set(scopedParams.values())` so the fresh binder cannot alias any
Pant name already bound in the current frame.

**SMT note.** `pant --check` accepts the emitted form via a sound Skolem
least-witness encoding (PR #126). On the checker side, `min over each j: T,
G | j` is supported for integer sorts `T ∈ {Nat, Nat0, Int}`, compiling to
a fresh Int constant `r` together with `(and <type-bound[r]> G(r))` and a
`(forall ((j Int)) (=> (and … (< j r) G(j)) false))` "no smaller witness"
assertion. ts2pant itself only has `IntStrategy` and `RealStrategy` (there
is no `NatStrategy`), so the translator-emitted μ-search form uses `Int`
in practice; the checker's `Nat`/`Nat0` support is exercised by hand-written
specs rather than ts2pant output. End-to-end verification of a μ-search
result is now in scope; consumers other than the μ-search aggregate (`#`,
`+/*/and/or/max over`, bare `each`, membership, subset) still require an
explicit upper-bound guard or a domain-typed iterator and emit a targeted
diagnostic otherwise.

## Intermediate Representation

ts2pant has accumulated ~18 surface-syntax recognizers in `src/translate-body.ts`
that cross-talk through `BodyResult`, `state.writes`, and the document-wide
`SynthCell` / `NameRegistry`. Each new TS pattern (μ-search, optional chaining,
Set mutation, …) adds another recognizer plus state-merge logic. PR #84's
post-mortem (below) names this exact failure mode.

The structural fix is to lift the recognizers behind a small typed
**intermediate representation** so each surface pattern lowers via a TS→IR
rewrite and the Pant emitter reads only IR forms. The reference precedent is
**IRSC** from Vekris, Cosman, Jhala, "Refinement Types for TypeScript"
([PLDI 2016, arxiv:1604.02480](https://arxiv.org/pdf/1604.02480)). Their
motivating quote — *"FRSC, while syntactically similar to TS, is not entirely
suitable for refinement type checking in its current form, due to features
like assignment. To overcome this challenge we translate FRSC to a functional
language IRSC through a Static Single Assignment (SSA) transformation"* — is
exactly our situation.

The IR is being introduced incrementally over 11 stages on a single branch.
Stage status lives in §"IR Migration Status" below; deviations from IRSC
live in §"Divergences from IRSC".

### Files

- `src/ir.ts` — `IRExpr` ADT with constructor helpers (Layer 2).
- `src/ir-build.ts` — TS-AST → L2 IRExpr (pure-path, gated by
  `--use-ir`).
- `src/ir-emit.ts` — L2 IRExpr → `OpaqueExpr` lowering.
- `src/ir1.ts`, `src/ir1-build.ts`, `src/ir1-build-body.ts`,
  `src/ir1-lower.ts`, `src/ir1-lower-body.ts` — Layer 1 (TS-shape
  imperative IR; see § "Imperative IR Workstream" below).

### Two paths, one Layer 1

Post-M3, mutating-body lowering bypasses L2 entirely:

- **Pure / value-position** — TS → L1 expression → L2 `IRExpr` →
  `OpaqueExpr`. The single-rooted `IRExpr` tree fits "one expression
  out". Pure-path is currently routed via `--use-ir` (env
  `TS2PANT_USE_IR=1`); always-on cutover is M6.
- **Effect / statement-position** — TS → L1 statement →
  `PropResult[]` directly via a single fold (`lowerL1Body`) over
  `SymbolicState` (in `translate-body.ts`). The mutating output is a
  list of equations + frame conditions; no L2 statement vocabulary.

L2 is *expression-only* — there is no L2 `IRStmt` post-M3. The
asymmetry is intentional; see `workstreams/ts2pant-imperative-ir.md`
§ "Architectural Lessons" for the rationale.

### `IRExpr` — 10 forms

| Form | Lowers to | TS shapes that produce it |
|------|-----------|---------------------------|
| `Var(name, primed?)` | `ast.var` / `ast.primed` | identifier; primed for next-state references in mutating bodies |
| `Lit(literal)` | `ast.litNat` / `ast.litBool` / `ast.litString` | numeric / string / boolean literal |
| `App(head, args)` | `ast.app` / `ast.binop` / `ast.unop` (head-dispatched) | binops, method calls (receiver as first arg), qualified field accessors, builtins (`in`, `#`, `=`, comparison) |
| `Cond([(g, v)])` | `ast.cond` | ternary, early-return if-conversion, `??` lowering, conditional mutation merge |
| `Let(name, value, body)` | substituted out at emit (Pant has no `let`) | `const x = e1; ... return e2` (Stage 6+) |
| `Each(binder, src, [g], proj)` | `ast.each` | for-of Shape A/B, `?.` lowering, `.filter`/`.map` chain |
| `Comb(comb, init?, each)` | `ast.eachComb` (with optional binop fold for non-identity init) | `.reduce`, μ-search (`Comb(min, ...)`) |
| `Forall(binder, type, guard?, body)` | `ast.forall` | `all x: T \| ...` quantifier emission |
| `Exists(binder, type, guard?, body)` | `ast.exists` | `some x: T \| ...` quantifier emission |
| `IRWrap(OpaqueExpr)` | identity | **migration-only escape hatch**; deleted at Stage 8 cutover |

### Mutating-body output (no L2 IR)

The mutating path emits `PropResult[]` directly:

- One `kind: "equation"` per modified rule (per-iter for Shape A
  foreach, accumulator-fold for Shape B, single-write for branched
  property mutation).
- One `kind: "assertion"` for empty-Set / empty-Map field
  initializers.
- One frame `kind: "equation"` per unmodified-but-in-scope rule
  (identity equation `prop' obj = prop obj`).

The fold is `lowerL1Body` in `ir1-lower-body.ts`, threading
`SymbolicState` from `translate-body.ts`. The state's primitives
(`putWrite`, `mergeOverrides`, `installMapWrite`, `installSetWrite`)
are reused from the legacy mutating path — frame-condition synthesis
is identical pre/post-M3.

### Divergence from IRSC

One deliberate divergence, documented so a future agent doesn't "fix"
it back to paper-faithful IRSC.

**No `FieldAccess` form.** ts2pant lowers `e.f` to `App(qualified-rule,
[e])` at construction time via `qualifyFieldAccess`. Adding a
`FieldAccess` form would force every consumer to check both shapes
and reintroduces the cross-talk problem.

(IRSC's SSA-over-program-names discipline does *not* apply here —
post-M3 there is no L2 statement vocabulary, and the mutating path's
write-key φ-merge happens inside `SymbolicState` rather than as an
L2 form.)

### Invariants

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
  appears as a side effect in `SymbolicState` / `PropResult[]`.

### Pure-path expression-IR migration (legacy plan)

The original IR migration plan (Stages 1–11) was partially superseded
by the imperative-IR workstream. Pure-path expression normalization
(Stages 1–8) survived as the L2 expression IR; the mutating-path
work (former Stages 9–11) re-formed inside the workstream. This
section documents the surviving Stage 1–8 status; the workstream
docs (§ "Imperative IR Workstream" below) cover the rest.

| Stage | Recognizer / scope | Status |
|-------|---------------------|--------|
| 1 | Foundation: types, build (Var/Lit/Identifier), emit, `--use-ir` flag, anchor fixture | ✅ landed |
| 2 | Optional chaining `?.` → `Each` | ✅ landed |
| 3 | Nullish coalescing `??` → `Cond` | ✅ landed |
| 4 | μ-search → `Comb(min, Each)` | ✅ landed via workstream M2 (`comb-typed` lowering in `ir1-lower.ts`) |
| 5 | `.length` / `.size` → `Unop(card, x)` | ✅ landed |
| 6 | Const-binding inlining → `Let` (pure path) | ✅ landed |
| 7 | Chain fusion → `Each` composition | ✅ tracked via IRWrap (anchors locked); native IR construction deferred — see "Note on chain fusion" below |
| 8 | Pure-path cutover — delete legacy code subsumed by IR | pending (re-forms inside workstream M6) |
| 9–11 | **Superseded** by `workstreams/ts2pant-imperative-ir.md`; mutating-path SSA, frame conditions, and final cutover re-formed inside workstream M3 (which **bypasses** the originally-planned L2 statement vocabulary — see workstream § "Architectural Lessons"). | superseded |

The `--use-ir` flag (env var `TS2PANT_USE_IR=1`) routes the pure path
through the IR pipeline. Default off until workstream M6 makes it
always-on. Per-stage gate is the `tests/ir-equivalence.test.mts`
smoke test (string-equal output between legacy and IR pipelines on
the anchor fixtures).

**Note on chain fusion (Stage 7).** `.filter`/`.map`/`.reduce` chains
already produce semantically-identical output through the `IRWrap`
fallback path: legacy `translateArrayMethod` and `translateReduceCall`
materialize an OpaqueExpr `each(...)` / `eachComb(...)`, which `ir-build`
wraps. Locked-in IR-equivalence anchors confirm byte-equality across all
shapes (`activeNames`, `nameLengths`, `highScores` from
`expressions-array.ts`; the full `expressions-reduce.ts` suite). Native
IR construction (a real `Each` IR node assembled in `ir-build`) is
**intentionally deferred** until workstream M6 — the existing 300+
lines of TS-AST inspection in legacy would translate to ~200 lines of
mechanical duplication producing identical output, with the
architectural payoff only at the always-on cutover.

### Imperative IR Workstream

ts2pant has a layered architecture: **Layer 1** is a TS-faithful imperative
IR (`Block`, `Cond`, `Foreach`, `Assign`, `For`, `While`, `Return`, …) in
`src/ir1.ts` where normalization passes collapse syntactic equivalences
(increment spellings, conditional families, iteration families) into a
small canonical vocabulary. **Layer 2** is `IRExpr` in `src/ir.ts` —
Pant-shaped *expression* IR (post-M3, statement-position lowering bypasses
L2; see § "Two paths, one Layer 1" above). **Layer 3** is `OpaqueExpr`.

Lowering for value-position: TS AST → Layer 1 (`ir1-build.ts`) → Layer 2
(`ir1-lower.ts`) → OpaqueExpr (`ir-emit.ts`).

Lowering for effect-position: TS AST → Layer 1 (`ir1-build-body.ts`) →
`PropResult[]` (`ir1-lower-body.ts`, threading `SymbolicState`).

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

**M3 (imperative-ir-iteration-mutation): landed.** Branched mutation
and iteration flow through Layer 1. The build pass (`ir1-build-body.ts`)
produces canonical L1 statement forms — `cond-stmt` for `if`-with-
mutation, `foreach` for `for-of` / `forEach` — and the lower pass
(`ir1-lower-body.ts`) does a single fold over the L1, threading the
existing `SymbolicState` from `translate-body.ts` and emitting
`PropResult[]`. No L2 statement vocabulary; the existing `SymbolicState`
primitives (`putWrite`, `mergeOverrides`, `installMapWrite`,
`installSetWrite`) are reused so frame-condition synthesis is
unchanged. `Foreach.body` (Shape A — uniform iterator writes) emits
one universally-quantified per-iteration equation per modified rule
(`all $N in src | prop' $N = …`), while `Foreach.foldLeaves` (Shape B
— accumulator folds `a.p OP= f(x)`) emits one *aggregated* accumulator
equation per leaf with a `comb over each $N in src[, guard] | rhs`
right-hand side. Both flow through the same fold but produce
distinct output shapes. Map/Set effects
inside branches (`m.set(k, v)`, `s.add(e)`, etc.) are first-class L1
forms (`map-effect`, `set-effect`). The legacy iteration recognizers
(`translateForOfLoop`, `translateForOfLoopBody`, `translateForEachStmt`,
`classifyLoopStmt`, `ShapeBLeaf`, `LoopStmtClass`, `FoldOps`) are
deleted — `symbolicExecute`'s if-statement / for-of / forEach arms are
thin dispatchers to the L1 path. Pure-path `.reduce` (chain fusion via
`BodyResult.pendingComprehension`) is expression-position and
architecturally separate; it stays on `translateReduceCall`.

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

**The `from-l2` adapter** (`ir1.ts`'s `IR1Expr.from-l2`) is the explicit
transitional mechanism for sub-expressions whose internal structure is
outside the current milestone's normalization concern. The adapter
wraps a pre-built Layer 2 `IRExpr` and lowers verbatim. M3 grew its
use (mutating-body sub-expressions like receivers, values, conditions
all arrive as OpaqueExpr from `translateBodyExpr` and wrap via
`from-l2`). Lifetime: shrinks once M4 (equality/nullish) and M5
(property access) bring sub-expressions onto L1 natively; deleted at
M6. Do not introduce new uses outside the build pipeline's scoped
sub-expression delegation.

**Locked decisions** (re-litigation requires explicit user sign-off):
- Layer 1 vocabulary is locked at M1. Forms can be added in later
  milestones (e.g., `IsNullish` primitive at M4, `IR1FoldLeaf` at M3)
  but existing forms cannot be changed. M3 activated `assign`,
  `foreach`, `cond-stmt`, `map-effect`, `set-effect`; `for`, `throw`,
  `expr-stmt` remain declared but unused (constructors return them;
  lowerers reject).
- **No `IR1Wrap` form.** Layer 1 is not an escape-hatch layer — the
  build pass either produces L1 or rejects with `unsupported`. The
  `from-l2` form is *not* a wrap-anything escape hatch; it's a scoped
  delegation point with a defined lifetime.
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
  `symbolicExecute` line-for-line. M3's `lowerL1Body` threads
  `SymbolicState` directly into `PropResult[]`, reusing the existing
  mutation primitives. See `workstreams/ts2pant-imperative-ir.md`
  § "Architectural Lessons" for the full rationale.

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

- **`tests/*.test.mts` — unit suite.** Pure translation-logic tests. No pant binary, no dune, runs in a few seconds. Files: `annotations`, `constructs`, `purity`, `translate-body`, `translate-signature`, `translate-types`, `wasm-ast`.
- **`tests/integration/*.test.mts` — integration suite.** Each test in this directory invokes the `pant` binary (via `assertPantTypeChecks` or `runCheck` from `tests/helpers.mts`). Files: `e2e`, `dogfood`.

The split exists for stability, not just performance. `getPantBin()` in `tests/helpers.mts` is **pure** — it never invokes a build. It reads `process.env.PANT_BIN` if set, otherwise checks that `${PROJECT_ROOT}/_build/default/bin/main.exe` exists, and throws with an actionable error otherwise. The build is the responsibility of the workspace-level `just build-pant` recipe (which `just ts2pant-test-integration` depends on). Stale-lock recovery + the `dune build` invocation live in the `build-pant` recipe in the root `justfile`, not in JS — orchestration belongs to the orchestrator.

**Why the split is load-bearing.** Node's test runner uses `--test-isolation=process` by default, spawning one subprocess per test file. The original `getPantBin()` ran `execSync("dune build bin/main.exe")` lazily inside every worker, so 9 fresh subprocesses raced for the same `_build/.lock`. A SIGKILL on any one worker (e.g. user aborting a hung pre-commit) left the lock in a poisoned state, and every subsequent build hung indefinitely. The pre-commit hook hung for 15+ hours during the μ-search PR before this fix landed; the post-mortem is the reason this section exists.

**Reintroducing the lazy-build pattern is forbidden.** If a new test needs the pant binary, route it through the integration suite — don't add an `execSync("dune build")` to `helpers.mts` or any `tests/*.test.mts` file.

### Snapshot files

Each snapshot-based `*.test.mts` has a sibling `*.test.mts.snapshot` (Node test runner's snapshot format). When snapshot tests are moved between directories, their `.snapshot` files must move with them; the runner resolves snapshot paths from the test file's location, so a missing-snapshot error usually means a mismatched move.

Test structure:
- `tests/fixtures/constructs/` — TypeScript fixture files, one per construct category
- `tests/constructs.test.mts` — snapshot tests comparing emitted Pantagruel against expectations
- `tests/translate-body.test.mts` — unit tests for internal translation edge cases
- `tests/translate-signature.test.mts` — signature extraction and guard detection tests
- `tests/integration/e2e.test.mts` — end-to-end pipeline tests including `pant --check` verification
- `tests/integration/dogfood.test.mts` — translates ts2pant's own source with ts2pant; verifies through `pant`
