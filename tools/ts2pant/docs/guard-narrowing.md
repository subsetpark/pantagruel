# Guard Narrowing

**Workstream:** ts2pant Flow-Sensitive Guard & Effect Analysis - Milestones 3a
and 3b (`du-discriminant-narrowing-layer`,
`nullish-and-predicate-narrowing`).

This note documents the shared assumption-environment narrowing model and the
M3b additions for nullish guards and user type-predicate guards.

The scope comes from the M2 survey in
[guard-narrowing-survey.md](./guard-narrowing-survey.md), which measured 165
nullish sites and a large but under-measured type-predicate tail. The governing
workstream decisions live in
[workstreams/ts2pant-guard-analysis.md](../../../workstreams/ts2pant-guard-analysis.md):
narrowing is intra-function only, uses the M3a assumption environment, and does
not special-case field names. The deferred type-predicate residual is tied to the
opaque value work described in
[workstreams/ts2pant-any-unknown-opaque.md](../../../workstreams/ts2pant-any-unknown-opaque.md).

## Assumption Environment

M3a introduced a function-scoped assumption environment. Lowering pushes a frame
when it enters a narrowed branch, records facts recognized from the branch test,
lets reads query those facts, and pops the frame on exit. This is occurrence
typing as a path-condition layer: the TypeScript test does not rewrite the whole
environment, but it contributes facts that discharge local Pantagruel
definedness obligations.

The environment currently carries three fact families:

- `discriminant`: a structural `<receiver>.<property> === <literal>` or matching
  switch-arm fact, used by discriminated-union field reads.
- `non-null`: a recognized nullish guard proving that an optional receiver is
  present.
- `predicate`: a Bool-valued predicate application, optionally annotated with
  user type-predicate metadata.

All facts are scoped to the translated function body. A fact learned inside a
caller is not transferred through a call boundary, and a helper predicate's body
is not followed to infer extra facts about the caller. This matches the
guard-analysis workstream's intra-function decision and the any/unknown
workstream's "cross-function-call narrowing not trusted by default" rule.

## Nullish Narrowing

Optionality is encoded as a Pantagruel list: `T | null`, `T | undefined`, and
similar nullable unions map to `[T]`. A value-level non-null assertion already
lowers a nullable read to singleton extraction:

```ts
x!
```

```pant
x 1
```

M3b applies the same extraction when the read is inside a recognized non-null
guard. For example, inside `if (x != null)`, a read of nullable `x` lowers as the
singleton element rather than as the list itself.

The nullish fact is recognized by reusing the existing nullish recognizer rather
than re-deriving syntactic cases. That keeps narrowing in lockstep with the
normalization that already handles:

- direct equality and inequality against `null` or `undefined`, including strict
  forms such as `x !== undefined`;
- `typeof x === "undefined"` and its negation;
- long-form nullish chains normalized by `nullish-recognizer.ts`;
- only operands whose TypeScript type is nullable, so scalar values do not get
  list-cardinality obligations.

When a nullable read is narrowed, ts2pant also records a definedness obligation.
For the direct matching fact, the obligation simplifies to `true`; otherwise it
is rendered from the in-scope non-null path conditions as a proof that `#x > 0`.
This is the same definedness story as the discriminant field checks: the emitted
`@pant` annotation has a solver-visible reason the partial read is safe.

The behavior is additive. Un-narrowed optional reads remain byte-identical to the
pre-M3b output: ts2pant does not add a new corpus-wide obligation to every
nullable read. Only reads that occur under a recognized non-null fact get
singleton extraction and a discharged cardinality obligation.

## Type-Predicate Narrowing

M3b also recognizes calls whose resolved signature has a TypeScript predicate of
the form:

```ts
function isNumber(x: number | null): x is number
```

A branch guarded by `if (isNumber(x))` records a predicate fact for the argument
`x` and the lowered predicate expression, such as `is-number x`. Reads of `x`
inside that branch can query the same assumption environment used for
discriminants and nullish facts.

The tractable subset is intentionally narrow. A type-predicate fact can discharge
when:

- the predicate is an identifier predicate (`x is T`) on a direct identifier
  argument;
- the predicate call itself can be translated as a pure Bool-valued expression;
- the refined target type can be represented by the currently landed type
  mapping; and
- the refined target is not a discriminated-union shape that would require
  learning field-specific variant implications from the predicate body.

For nullable values in that subset, the guarded read uses the same singleton
extraction shape as nullish narrowing and records a predicate-definedness
obligation. For non-null discriminated field reads in the tractable subset, the
field access is marked discharged by the in-scope predicate fact rather than by a
discriminant equality.

The important property is sound bail. If the predicate fact is absent, negated,
or marked intractable, the read keeps its existing lowering and the obligation is
not falsely discharged.

## Deferred Residual

Several useful `x is T` cases remain deliberately out of scope for M3b.

First, ts2pant does not follow the predicate function body to discover what the
predicate means. A helper such as `isCircle(s): s is Circle` may internally test
`s.kind === "circle"`, but M3b does not transfer that discriminant equality to
the caller by reading the helper's body. That would be cross-call narrowing, which
the guard-analysis workstream explicitly excludes.

Second, some predicate targets need a refined-type representation that is not
available until the any/unknown opaque work lands. The opaque workstream records
that TypeScript narrowing can recover precision at a use site, but that
cross-function narrowing is not trusted by default and that `any` / `unknown`
need an explicit `Opaque` floor before precision can be recovered soundly. M3b
therefore recognizes the predicate call but leaves such cases undischarged until a
future M3c can build on the opaque encoding.

Third, M3b does not add name-based exceptions. It does not treat `kind`, `type`,
`_tag`, or TypeScript compiler helper names specially. Discriminant facts remain
structural, nullish facts come from the nullish recognizer, and predicate facts
are scoped to the recognized predicate application.

The resulting contract is:

- nullish narrowing is complete for the recognized nullable test forms;
- tractable type-predicate facts can discharge the local read they justify;
- intractable type-predicate facts remain visible as facts but do not discharge;
- no predicate is treated as proved by body-following or by an opaque refined type
  that ts2pant cannot yet represent.

## Validation Expectations

The fixture contract for this layer is entailment, not broad snapshot churn.
Representative `@pant` annotations should entail under `pant --check` when a
recognized branch fact is in scope. Existing un-narrowed output should stay
unchanged, preserving the additive posture recommended by the M2 survey and
adopted in the M3b workstream plan.
