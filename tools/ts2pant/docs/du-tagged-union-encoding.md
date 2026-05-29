# Tagged Discriminated-Union Encoding

This document describes the terminal-state `ts2pant` encoding for TypeScript
discriminated unions after the `du-cutover` milestone. It builds on the M2
entailment survey in
[du-entailment-narrowing-survey.md](./du-entailment-narrowing-survey.md) and
the milestone decisions in
[workstreams/ts2pant-discriminated-union.md](../../../workstreams/ts2pant-discriminated-union.md).

## Scope

A TypeScript union is a discriminated union only by structure. The translator
does not special-case field names such as `kind`, `type`, or `_tag`.

A union qualifies when:

- it has at least two non-nullish members;
- every member has at least one common field; and
- at least one common field has a distinct literal type on every member.

When more than one field qualifies, detection picks the first qualifying field
by sorted field name. The selected field is the discriminant. Any other
qualifying field is encoded as an ordinary guarded variant field under the
chosen discriminant.

Unions that contain `null`, `undefined`, or `void` do not qualify as
discriminated unions. Optionality stays on the existing list-lift path:
`T | null` maps to `[T]`, and `A | B | null` maps to `[A + B]`.

## Encoding

Each successfully registered discriminated-union shape maps to one synthesized
Pantagruel domain. Registration is shape-keyed, so the same TypeScript DU shape
uses one shared domain whether it appears as a top-level alias, a variant field,
an anonymous-record field, a `Map` value, or an array/tuple element.

For:

```ts
type Shape =
  | { kind: "circle"; r: number; shared: string }
  | { kind: "square"; s: number; shared: string };
```

the emitted shape is:

```pant
Shape.
shape--kind s: Shape => String.
shape--r s: Shape, shape--kind s = "circle" => Int.
shape--s s: Shape, shape--kind s = "square" => Int.
shape--shared s: Shape, shape--kind s = "circle" or shape--kind s = "square" => String.

all s: Shape | shape--kind s = "circle" or shape--kind s = "square".
```

The discriminant rule is total on the synthesized domain. Variant-only fields
are guarded by exactly the variants that declare them. Shared fields are also
guarded, with an `or` over every declaring variant. Field rule names follow the
same owner/field convention as record synthesis: `<domain>--<field>`.

The totality assertion is emitted once per DU domain. The M2 survey showed that
disjointness is already available from function-value semantics, but totality is
not free under EUF. The explicit invariant lets exhaustive narrowing and
fallback arms reason from the known TypeScript variant cover.

## Narrowing

Variant-field reads are sound without narrowing because guarded Pantagruel
rules are partial on their preconditions. The translator may emit
`shape--r x`, but the field rule carries the guard
`shape--kind x = "circle"`.

Narrowing is local and intra-function. The recognizer records facts from tests
such as:

- `x.kind === "circle"`;
- `"circle" === x.kind`;
- `x.kind !== "circle"`; and
- `switch (x.kind) { case "circle": ... }`.

The assumption environment stores those facts by receiver text, property name,
literal, and negation. When a guarded variant-field rule is applied, the
translator checks whether the matching discriminant fact is in scope. If it is,
the precondition is discharged and the generated `@pant` obligation entails
under z3. This is the target shape measured in the M2 survey: the guarded
encoding entails once the discriminant equality is available.

The narrowing layer is intentionally not cross-call. A helper predicate that
returns `x.kind === "circle"` does not currently transfer the discriminant fact
to the caller.

## Nested Discriminated Unions

Nested DUs use the same recursive synthesis path as other synthesized shapes.
When `mapTsType` is called with a synth cell, it recursively maps:

- array, tuple, iterator, and set elements;
- `Map<K, V>` keys and values;
- anonymous-record fields; and
- discriminated-union variant fields.

If a nested type is itself a discriminated union, it is registered through the
same `cellRegisterDiscriminatedUnion` path as a top-level DU. Because
registration is keyed by structural shape, the nested occurrence and any other
occurrence share one synthesized domain. A nested narrowed read therefore has
the same entailment story as a top-level narrowed read.

## Refusal Taxonomy

The cutover makes the tagged guarded-rule encoding the sole path for detected
discriminated unions:

- If detection succeeds and tagged registration succeeds, the union maps to the
  synthesized DU domain and is never `+`-encoded.
- If detection succeeds but tagged registration fails, the union is refused with
  `discriminated union could not be registered for tagged Pantagruel encoding`.
  It does not fall back to the legacy `+`-of-records encoding.
- If a non-discriminated union is used only as a value type, the existing
  `A + B` encoding remains available.
- If code accesses a field on a non-discriminated union, the access is refused
  with
  `field access on a non-discriminated union is not expressible in Pantagruel`.
  This removes the old unique-field unsoundness where a field declared by one
  member could be resolved to that member's accessor and applied to the whole
  union value.
- Intersection field access is unchanged. Intersections have AND semantics, so
  a field declared by one member is genuinely present on the intersection
  value; resolving that member's accessor remains sound.
- Optionality is unchanged: `T | null` still maps to `[T]`.

The remaining `A + B` support is therefore a value encoding for
non-discriminated unions, not a discriminated-union fallback and not a license
to access member-specific fields through a union receiver.
