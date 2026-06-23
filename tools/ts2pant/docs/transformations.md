# ts2pant Transformation Patterns

> Companion to `tools/ts2pant/AGENTS.md`. Each ts2pant transformation
> follows a standard algorithm from the PL / verification literature.
> This file is the catalogue: standard name + reference + invariants
> + implementation pointer for every pattern in production.
>
> When adding a new transformation, update this file alongside the
> code — see § "Reference your sources" in AGENTS.md.

## Let-Elimination (Const Binding Inlining)

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

## If-Conversion (Early Returns, Multi-Arm Conditionals)

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
with a record-typed return use the product-field variant described in
§ "Record Returns" below: every branch value must be an explicit object
literal with the same accepted field set, and each field receives its own
conditional equation.

## Recursive Pure Block-Return Lowering

**Standard name:** If-conversion plus let-elimination.
**Reference:** Allen et al., POPL 1983; Peyton Jones & Marlow, JFP 2002.

Nested value-position consumers such as branches, switch clauses, and array
callbacks need to treat a TS block as one expression without leaking
branch-local bindings. `lowerNestedPureBlockReturn()` is the shared helper for
that shape:

```ts
{
  const x = e1;
  if (p) return e2;
  return e3;
}
```

The helper first reuses `extractReturnExpression()` to recognize the same pure
body surface as the top-level translator. It then runs the common prelude
validator used by `lowerPreludeBindings()` so TDZ, forward-reference, and
side-effect checks stay aligned. Finally it lowers predicates and return
values through L1, builds one conditional value, and right-folds the local
bindings through `substituteIR1ExprSubtree()`.

**Invariants:**
- No partial output: unsupported statements return `null`.
- Let-elimination is capture-avoiding and uses the IR1 substitution primitive.
- Nested block locals never become Pant rule declarations or free variables.
- Conservative refusal covers stateful expression statements, effectful const
  initializers, non-final returns, throws, breaks, and unsupported terminals.
- Local accumulator sequencing such as `lines.push(...); return lines` is handled
  by the separate collection-builder model below.

## Local Collection Builders

**Standard name:** A-normal-form local builder recognition / effect
encapsulation.
**Reference:** Flanagan et al., PLDI 1993; Dijkstra, CACM 1975.

Some pure TypeScript functions are imperative internally but have a clear
value-level target:

```ts
const out: T[] = [];
out.push(e1);
out.push(e2);
return out;
```

Local collection builders are recognized as bounded ANF-like preludes with one
private mutable accumulator. The implementation entry points are
`tryBuildLocalListBuilderReturn()` and `tryBuildLocalSetBuilderReturn()`. They
do **not** lower `push` / `add` as pure expressions, and they do not synthesize
target-language collection literals.

### Ordered list builders

For `T[]` builders, ts2pant emits constraints over the declared return rule:
one cardinality assertion for the result and one positional list-application
assertion per pushed value. Positions are 1-based Pantagruel `Nat` indexes, so:

```ts
const out: T[] = [];
out.push(e1);
out.push(e2);
return out;
```

emits the assertion shape:

```text
#(f args) = 2.
(f args) 1 = e1.
(f args) 2 = e2.
```

The result remains the function's declared `[T]` rule. No list literal,
synthetic finite tuple, or helper domain is introduced. Ordinary const bindings
before the pushes are lowered through the existing local-rule prelude machinery
so pushed values can refer to hygienically scoped local names.

### Set builders

For `Set<T>` / `ReadonlySet<T>` builders, ts2pant uses the existing Set-as-list
encoding and emits membership assertions over the returned value. For direct
straight-line `.add` calls:

```ts
const out = new Set<T>();
out.add(e1);
out.add(e2);
return out;
```

the emitted assertion shape is a quantified equivalence:

```text
all x: T | x in f args <-> (x = e1 or x = e2).
```

Insertion order and duplicate insertion are intentionally not modeled beyond
membership. An empty local Set builder emits universal non-membership:

```text
all x: T | ~(x in f args).
```

### Exclusions

**Invariants:**
- The returned identifier is the same local empty-array accumulator that
  receives all pushes, or the same local empty-`Set` accumulator that receives
  all adds.
- List push order is source order; emitted positions are `1..N`.
- Set additions are membership-only; no insertion order or uniqueness axioms are
  emitted beyond the membership equivalence.
- The accumulator is not aliased, read by a guard or pushed/added expression,
  passed to another call, reassigned, or mutated by any method other than the
  admitted builder method (`.push` for lists, `.add` for Sets).
- Const bindings are allowed only before the first push/add; later consts are
  rejected to keep the model straight-line and unambiguous.
- Unsupported builder shapes remain diagnostics. General expression statements
  still flow through the normal prelude rejection path.

**Explicitly unsupported in M2:**
- Map builders such as `const m = new Map(); m.set(k, v); return m`. Map
  construction is deferred because it must respect the guarded membership/value
  rule pair described in "Partial Rules (Map<K, V>)" below.
- Set `.delete` and `.clear` builders. Straight-line local Set construction only
  admits `.add` calls against a fresh empty local Set.
- Accumulator aliasing or escape: returning an alias, assigning the accumulator
  to another variable, capturing it in a closure, passing it to another call, or
  reading it from a guard/value expression.
- Unknown mutating methods on the accumulator.
- Loop-backed builders outside the already-supported for-of build-list
  comprehension path. Broader `for-of` and `forEach` builder completeness is a
  separate iteration milestone.

## Uninterpreted Functions (General Function Calls)

**Standard name:** EUF (Equality with Uninterpreted Functions).
**Reference:** Kroening & Strichman, Ch. 4; Programming Z3.

`foo(a, b)` becomes a function application. The only axiom is **congruence**: equal
arguments imply equal results. If a matching Pantagruel rule exists, the solver uses its
constraints; otherwise the function is universally quantified. No cross-function analysis.

## Guarded Commands (Conditional Mutations)

**Standard name:** Guarded command language / conditional next-state relations.
**Reference:** Dijkstra, CACM 1975; Lamport, *Specifying Systems* Ch. 2-3.

`if (cond) { obj.prop = val }` becomes `prop' obj = cond cond => val, true => prop obj`.
Frame conditions for conditionally-modified variables must include the identity case.
Use the **modifies-set** approach: identify which primed variables appear in the action
body, emit frame conditions (`prop' obj = prop obj`) for everything not in the set.

## Option-Type Elimination (Nullish Coalescing, Optional Chaining)

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
indexing `(x 1)` replaces singleton extraction. Post-M4, the `#x = 0`
shape is the lowering of an L1 `IsNullish(x)` primitive — see
`intermediate-representation.md` § "Imperative IR Workstream" / "M4". The
OpaqueExpr is identical pre- and post-M4; only the construction route
changed (legacy `??` builder → L1 IsNullish → mechanical lower). On the
body/L1 expression-translation path, other null/undefined surface forms
(`x == null`, `x === null`, `x === undefined`, the long disjunction,
`typeof x === 'undefined'`) all flow through the same primitive. The
signature/guard-classification path is stricter — see
`intermediate-representation.md` § "Imperative IR Workstream" / "M4"
for the asymmetry.

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

**Null-guarded list-lifted conditionals** — TS code that pattern-matches
on a nullable receiver and returns a list-lifted projection
(`u == null ? [] : [u.name]`, `if (u === null) return []; return [u.name]`,
and the negated polarity variants) is a Pantagruel-untranslatable shape
without a recognizer: Pant has no list literal, so the alternative
cardinality-dispatch lowering `cond #u = 0 => [], true => [name (u 1)]`
has no expressible target. The functor-lift recognizer (M4 Patch 5) is
the canonical handling for this family — see § "Functor-Lift Recognizer"
below. The four supported TS shapes lower uniformly to `each n in u |
name n`, the same comprehension shape `?.` already uses.

## Partial Rules (Map<K, V>)

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

### Mutation (.set / .delete)

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

## Record Returns (Object-Literal Return Expressions)

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

**Fieldwise record conditionals.**

**Standard name:** If-conversion over product fields.
**Reference:** Allen et al., POPL 1983.

When a pure function returns a record through early-return arms or a terminal
`if/else` chain, ts2pant distributes the conditional over each record field:

```ts
if (flag) return { x: n, y: n + 1 };
return { x: 0, y: 1 };
```

becomes:

```text
x (pair n flag) = cond flag => n, true => 0.
y (pair n flag) = cond flag => n + 1, true => 1.
```

This is the product-field form of the same observational encoding used for
plain record returns. Pantagruel still has no record-constructor expression;
the conditional therefore appears only at accessor equations' RHS positions.

**Invariants / restrictions:**
- Every conditional arm and the default/fallthrough value must be an object
  literal accepted by the same field policy as `translateRecordReturn`.
- All arm literals must provide the declared field set exactly. Mismatches,
  extra fields, spreads, computed keys, methods, and accessors are rejected.
- Each scalar field initializer is translated through the normal pure body
  expression path before being placed under `cond`.
- Nested object-literal fields recurse fieldwise; mixed nested-record/scalar
  field arms are rejected.
- Empty `Set`/`Map` record initializers remain outside this conditional
  lowering because the plain record path emits quantified emptiness
  assertions, not RHS expressions that can be guarded by `cond`.

## Structured Iteration (for-of, forEach, reduce)

**Standard name:** Catamorphisms / structural recursion on lists.
**Reference:** Meijer, Fokkinga, Paterson, ["Functional Programming with Bananas, Lenses,
Envelopes and Barbed Wire"](https://maartenfokkinga.github.io/utwente/mmf91m.pdf), FPCA 1991;
Lamport, *Specifying Systems* (TLA+ `\A x \in arr` idiom for per-element next-state).

ts2pant accepts three shapes that are all instances of the catamorphism `foldr : (a -> b -> b) -> b -> [a] -> b`:

**Shape A — uniform iterator write (mutating).**
`for (const x of arr) { x.p = e(x) }` and `arr.forEach(x => { x.p = e(x) })` become
`all x in arr | p' x = e(x).` (universal-quantifier proposition form; binding is Pantagruel's
bare `x in arr` via `ast.gIn`, with empty `params`). The body is built into an
`IR1ForeachBody` (the Shape-A subset of `IR1Stmt`) with `x` bound, so conditional
writes inside the loop pass through the standard if-conversion machinery.

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

The unified loop milestone keeps the recursion-scheme framing explicit:
bounded loops are catamorphisms and lower through `IR1SsaLoopBody` with a
non-null `IR1SsaTerminationMetric` (counter metrics for counter/while,
iterating-source metrics for foreach Shape A/B). Fixed-point while loops are
hylomorphisms: the guard unfolds the state space and the recursive helper
folds it back to the post-state, so those loop bodies carry
`terminationMetric: null`. L6+L7 closed both halves under the same loop-body
contract.

**Invariants** (post-IR1-SSA):
- Shape A and Shape B both lower through
  `lowerForeachShapeAAsGeneralLoop` / `lowerForeachShapeBAsGeneralLoop` in
  `src/ir1-ssa-foreach.ts`. The old `IR1SsaLoopSummary` path is gone; both
  shapes now produce `IR1SsaLoopHeaderJoin` + `IR1SsaLoopBody` records plus
  the same emitted equations as before.
- Shape A's per-iteration writes degenerately close their header join:
  the `loopBackVersion` is the write's own version, because the per-element
  equation does not feed an accumulator into the next iteration.
- Shape B's accumulator-fold writes use the ordinary inductive close:
  the body write feeds back through the loop header and lowering emits
  `prop' target = prop target OP (combOP over each x in src[, guard] | rhs)`.
- Iter-binder writes scoped inside the iteration cannot leak past the
  comprehension — enforced structurally by `IR1ForeachBody` typing
  (Shape A bodies are a restricted subset of `IR1Stmt` that excludes
  `return`/`throw`/nested loops/`let`/`expr-stmt`).
- Shape B `foldLeaves` carry the `outerOp` binop and the comprehension
  `combiner` separately, so frame conditions for untouched properties still
  fire via the modified-rules set.

## Functor-Lift Recognizer

**Standard name:** Functor lift / `Maybe` (option) `fmap`. The list-lift
encoding makes `T | null` into `[T]` (length 0 or 1); the recognizer is
exactly `fmap : (a -> b) -> Maybe a -> Maybe b` specialized to the
list-as-Maybe representation. The substitution half of the
transformation (`body[e := n]`) is first-order term rewriting on the
L1 IR — see § "Operand-Substitution Rule" below. The metavariable `n`
denotes a parser-roundtrippable comprehension binder allocated via
`allocateLiftBinder` / `cellRegisterName` (`n`, `n1`, `n2`, …); it is
*not* the `$N` internal-hygienic class which exists only pre-emission.
**Reference:** Wadler, ["The Essence of Functional Programming"](https://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps),
POPL 1992, §2 (the Maybe monad and `fmap`); Hutton & Meijer,
["Monadic Parsing in Haskell"](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf), JFP 1998
(structural lift over `Maybe` as the canonical idiom for null-guarded
projections); Baader & Nipkow, *Term Rewriting and All That* (Cambridge
1998), ch. 2 (positions, subterms, and substitution — the rewriting
half of the transformation, described below).

`if (x == null) return []; return [f(x)];` and the equivalent ternary /
negated forms lower to `each n in x | f n`. This is *not* an
optimization — Pantagruel has no list literal, so the alternative
cardinality-dispatch lowering `cond #x = 0 => [], true => [f (x 1)]`
has no expressible Pant target. Without the recognizer these
idiomatic null-guards reject; with it they translate uniformly through
the same comprehension shape `?.` already produces.

**Why this lives here.** The recognizer is itself an instance of the
"recognize idiomatic TS" side of the Developer Steering Principles
(see `AGENTS.md`). Idiomatic null-guard-then-list-return is unambiguous TS;
forcing the programmer to rewrite as `?.`-chains or comprehensions
would tax the user for a Pant-vocabulary gap that the translator can
absorb structurally. Combined-shape recognizers crossing milestone
boundaries (this one straddles M1 Cond + M4 IsNullish) are sometimes
load-bearing; see `workstreams/ts2pant-imperative-ir.md`
§ "Architectural Lesson 5".

**Soundness conditions** (`tryRecognizeFunctorLift` in `ir1-build.ts`):

1. **Guard is a leaf nullish form** — `x == null`, `x === null`,
   `x === undefined`, `typeof x === 'undefined'`, or any of their
   negations. Composite guards (long-form disjunction, parenthesized
   chains, `&&`-conjuncts) fall through; only the leaves of M4's
   nullish recognizer are eligible here.
2. **The "empty side" branch is empty-equivalent** — `[]`, `null`, or
   `undefined`. (Polarity follows the guard: a positive guard puts the
   empty side on the then-branch; a negated guard puts it on the
   else-branch.)
3. **The "present side" branch is single-element-producing of the
   operand** — after stripping a single-element array wrapper (`[u.name]`
   → `u.name`), the expression must reference the operand and not be a
   multi-element construction. Multi-element array literals, spreads,
   and known multi-producing array methods (`.concat`, `.flat`,
   `.flatMap`, `.filter`, `.map`, `.slice`, `.splice`) are
   deliberately rejected — `each` over a length-≤1 list cannot soundly
   produce multiple output elements per input.
4. **The conditional's static result type is list-lifted** — `T[]`,
   `T | null`, `T | undefined`, or `T | null | undefined`. Computed
   from the `ConditionalExpression` for ternaries; from the enclosing
   function's return type for if-conversion entry points.

**Supported TS shapes.** The operand may be a simple `Identifier`
(`Var`) or a property-access / string-literal element-access chain
(`Member`); transparently-wrapped variants of either are accepted
because the recognizer's outer boundary (`unwrapTransparentExpression`)
and the recursive Member-chain build (`buildL1MemberOrVarForLift`)
both strip the same wrapper set — parens, `as` casts, non-null
assertions (`!`), and `satisfies` — at every level of the chain, so
eligibility is driven by the operand's L1 shape rather than its
TS-AST spelling. `qualifyFieldAccess` is still given the parens-
stripped (but type-erasure-preserving) receiver, so a user's `as T`
cast continues to drive qualifier resolution. Member-operand support
landed in M5 P4 (lifting the M4 P5 simple-identifier restriction);
the eligibility check is now expressed in L1 terms (`Var` or `Member`)
rather than TS-AST terms. Member-operand projections must surface
the operand structurally at the L1 level — `ast.substituteBinder`
substitutes by name and cannot target a `Member` subtree, so a
Member-operand projection that buries the operand inside a non-
Member sub-expression (e.g., a method call) falls through.

```ts
// (a) Positive ternary, with array wrapper or bare projection.
u == null ? [] : [u.name]
u == null ? null : u.name

// (b) Negated ternary.
u !== null ? u.age : null

// (c) Positive if-conversion (early return = empty side).
if (u === null) { return []; }
return [u.name];

// (d) Negated if-conversion (early return = present side).
if (u !== null) { return [u.name]; }
return [];
```

All four lower to `each n in u | name n` (or `age n`, etc.) — `n` is
the emitted comprehension binder (a fresh kebab-cased name allocated
through `cellRegisterName`, suffixed `n1`/`n2`/… on collision), not the
internal `$N` hygienic class. Fixtures:
`tests/fixtures/constructs/expressions-functor-lift.ts` (Var
operand, M4 P5) and
`tests/fixtures/constructs/expressions-functor-lift-property.ts`
(Member operand, M5 P4).

**Deliberate rejection of multi-element non-empty branches.** A shape
like `if (xs == null) return []; return [xs[0], xs[1]];` would require
the lift to multiply elements per input — `each x in xs | …` over a
length-≤1 list cannot produce two output elements. The recognizer
refuses, and these forms fall through to the standard L1 Cond build
(which then rejects at the no-list-literal wall — there is no
translatable target). This is intentional under the steering principle's
rule 1: the TS itself is fine, but ts2pant cannot translate
"sometimes-multi-element" pattern-matching without misrepresenting it.

**Operand-Substitution Rule (`body[e := n]`).** The lift's right-hand
side `each n in e | body'` is built by replacing every syntactic
occurrence of the operand subterm `e` inside `body` with the fresh
binder `n`. This is *first-order term rewriting* (Baader & Nipkow ch. 2):
match the operand `e` as a needle, replace each match with the binder,
return the rewritten haystack. The binder `n` here denotes the actual
emitted name (`n`, `n1`, `n2`, … allocated through `allocateLiftBinder`
→ `cellRegisterName`); it is *not* the `$N` internal-hygienic class,
which is reserved for binders that disappear before emission and would
not round-trip through Pantagruel's lexer.

For Var operands (M4 P5) the substitution primitive is Pant's
`ast.substituteBinder` post-lowering — the operand's Pant name is
known and the OpaqueExpr walker rewrites every reference. For Member
operands (M5 P4), `ast.substituteBinder` cannot target a Member
subtree (it substitutes only by Var name), so the substitution lives
at the L1 level via `substituteL1Subtree` in `ir1-build.ts`. The
operand and projection are both built into canonical L1 forms along
the entire chain so the structural matcher can compare them directly.

The four invariants the rewrite depends on, restated:

1. **Hygiene.** The comprehension binder is allocated via
   `cellRegisterName` (through `allocateLiftBinder`) against the
   document-wide `NameRegistry` — a parser-roundtrippable name (`n`,
   `n1`, `n2`, …), not the `$N` internal class. Barendregt convention
   applied at the emission layer.
2. **Structural reachability.** The operand must occur as a syntactic
   subterm of the projection. Var operands are checked via
   `expressionReferencesNames` at the TS-AST level; Member operands
   are gated on the explicit `changed` flag returned by
   `substituteL1Subtree` (a reference-equality check on the rewritten
   tree is unreliable because the walker rebuilds compound parents
   unconditionally).
3. **Referential transparency at the operand position.** The
   comprehension evaluates `e` once at its source position; each
   in-`body` occurrence is replaced by the binder. Sound iff `e` has
   no observable effects whose duplication or removal would change
   semantics — implicitly enforced by the recognizer matching only
   list-lifted nullable shapes that don't admit side effects.
4. **Closed form on Var/Member.** `structuralEqualL1` only compares
   `Var` and `Member` shapes; the pure-L1 builder
   `buildL1MemberOrVarForLift` produces only those shapes. Extending
   the eligible operand vocabulary (e.g., to `App`) requires
   extending both the matcher and the accept-set together.

Adding new operand shapes or comparable forms must re-verify all
four invariants. Update this section when the rewrite changes.

## Chain Fusion (.filter / .map / .reduce)

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

## Kleene Minimization (While-Loop μ-Search)

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

**Recognizer scope.** The TS-AST pair recognition lives in
`recognizeLetWhilePair` (`translate-body.ts`); validation that the
resulting L1 form matches the canonical Cytron-style μ-search shape
lives in `isCanonicalMuSearchForm` (`ir1-lower.ts`); the L1→L2
lowering to `comb-typed` is `lowerL1MuSearch` (`ir1-lower.ts`). The
TS-AST recognizer is purely structural — no Pantagruel-target
awareness — and only consumes the let+while pair when the L1
canonical-form check would accept the result. Conservative syntactic
match:
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
`const` counters, and bare `while` without a preceding `let` all fall through
to `extractReturnExpression`'s normal rejection path. The five `+1`
increment spellings (`i++`, `++i`, `i += 1`, `i = i + 1`, `i = 1 + i`) all
canonicalise to the same `Assign(target, BinOp(add, target, 1))` shape via
`buildL1IncrementStep` post-M2 of the imperative-IR workstream — the L1
normalizer is what allows the recognizer to fire on TS-surface variation
without per-spelling code paths. The `i--` / `--i` / `i -= 1` mirrors are
also canonicalised at L1 (to `Binop(sub, ...)`) but the μ-search
recognizer only accepts the `add` direction; decrement-direction support
for mutating bounded `let`+`while` loops landed in the general-loop
SSA workstream's L3 milestone (see
`intermediate-representation.md` § "Bounded while loop lowering (L3)")
and is symmetric to the L2 counter-loop path.

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
