# Effect Error-Channel And Brand Preconditions

**Workstream:** ts2pant Flow-Sensitive Guard & Effect Analysis - Milestone 4
(`guard-effect-error-channel`).

This note documents the two M4 precondition sources added to ts2pant:
Effect error-channel recovery and branded/refined parameter recovery. Both follow
the workstream's conservative-bail rule: emit a sound precondition only when the
translator has enough static evidence, otherwise emit nothing.

See the M4 definition of done in
[`workstreams/ts2pant-guard-analysis.md`](../../../workstreams/ts2pant-guard-analysis.md#milestone-4-guard-effect-error-channel).
The Effect side is based on Effect's `Effect<A, E, R>` shape and generator
composition model in the Effect docs:
[`Effect.ts`](https://effect-ts.github.io/effect/effect/Effect.ts.html). The brand
side follows Effect's branded/refined type model and Schema filter naming:
[`Brand.ts`](https://effect-ts.github.io/effect/effect/Brand.ts.html) and
[`Schema.ts`](https://effect-ts.github.io/effect/effect/Schema.ts.html).

## Output Channel

These analyses do not create a new Pantagruel emission path. They feed the same
guard slot already used by if-throw guards, assertion guards, and followed guard
helpers:

- `PantRule.guard` for pure functions.
- `PantAction.guard` for actions.

When more than one source contributes a precondition, ts2pant AND-combines the
predicates. For example, an existing assertion guard, an Effect error-channel guard,
and a brand guard become one declaration guard:

```pant
f x: Int, assertion-guard x and effect-guard x and brand-guard x => Int.
```

## Effect Error-Channel Recovery

Effect's public type is `Effect<A, E, R>`: `A` is the success value, `E` is the
typed error channel, and `R` is the required environment. M4 only reads `E` for
precondition synthesis.

For an Effect-returning function, ts2pant:

1. Resolves that the return type is Effect's `Effect`.
2. Extracts the second type argument, `E`.
3. Enumerates the non-nullish members of `E` when it is a union.
4. Ignores the function when `E` is `never`, nullish-only, non-Effect, or not
   statically nameable.
5. Looks inside the returned `Effect.gen(function* () { ... })` body for guarded
   error yields of the form:

```ts
if (cond) {
  yield* new FooError()
}
```

For each enumerated error mode, the constructor name must match one recovered
`yield* new ErrorClass(...)`. If `FooError` is raised when `cond` holds, then the
success precondition includes `~cond`: callers get the success value only when that
failure condition is false.

For a return type with two modes:

```ts
Effect.Effect<number, FooError | BarError, never>
```

and a generator body that raises `FooError` under `x < 0` and `BarError` under
`x > 10`, ts2pant emits a precondition equivalent to:

```pant
~(x < 0) and ~(x > 10)
```

## Completeness Bail

The error-channel analysis is all-or-nothing per function. It emits the
Effect-derived precondition only when every enumerated `E` member is recovered and
every recovered condition is side-effect-free according to the M1 purity classifier.

ts2pant emits no Effect-derived precondition when:

- The return type is not Effect's `Effect<A, E, R>`.
- The `E` channel is `never`.
- An `E` member cannot be enumerated or named.
- A mode in `E` has no matching guarded `yield* new ErrorClass(...)`.
- The generator contains an unmatched error yield shape.
- A matching condition is effectful or cannot be translated as a Pantagruel guard.
- The same mode is recovered more than once.

This is a soundness rule. A partial conjunction would be a too-weak caller
precondition: it could claim the function succeeds in states where an unrecovered
error mode still fails.

## Branded Parameter Preconditions

M4 also recovers preconditions from recognized Effect brand/refinement information
on parameter types. Effect brands refine an underlying TypeScript type with an
extra type tag; Effect's `Brand.refined` API associates those brands with
validation predicates.

ts2pant keeps the parameter's base type mapping unchanged and contributes only an
additional declaration guard when the brand is in the curated allowlist:

| Brand/filter name | Pantagruel predicate |
| --- | --- |
| `positive`, `Positive` | `x > 0` |
| `negative`, `Negative` | `x < 0` |
| `nonNegative`, `NonNegative` | `x >= 0` |
| `nonPositive`, `NonPositive` | `x <= 0` |
| `int`, `Int`, `integer`, `Integer` | `integral x` |
| `nonEmpty`, `NonEmpty`, `NonEmptyString` | `#x > 0` |

Multiple recognized brands on the same parameter are AND-combined. For example:

```ts
type PositiveInt = number & Brand.Brand<"Positive"> & Brand.Brand<"Int">
```

contributes:

```pant
x > 0 and integral x
```

If a parameter carries an unrecognized brand, ts2pant emits no brand predicate for
that parameter rather than emitting only the recognized subset. Unbranded
parameters also contribute nothing.

## Out Of Scope

M4 is scoped to precondition synthesis only. It intentionally does not:

- Decompose or remap the Effect success type `A`.
- Change the branded parameter's base-type mapping.
- Infer arbitrary user-defined brand predicates by reading constructor bodies.
- Infer semantics for custom or nominal brand names outside the allowlist.
- Perform cross-function Effect generator analysis.
- Treat incomplete recovery as a weaker precondition.

The resulting behavior is additive: non-Effect functions, infallible Effect
functions, unbranded parameters, unknown brands, and unsupported Effect generator
shapes keep their prior emitted Pantagruel shape.
