# Opaque Narrowing

**Workstream:** ts2pant `any` / `unknown` handling via `Opaque` sort - Milestone 4
(`ts2pant-opaque-narrowing`).

This note documents the use-site narrowing rule for `any` and `unknown` values.
It is the M4 complement to [guard-narrowing.md](./guard-narrowing.md): guard
narrowing is about definedness facts, while this note is about choosing a more
precise sort for a dynamic value when the TypeScript checker has already
flow-narrowed it.

The governing workstream decisions live in
[workstreams/ts2pant-any-unknown-opaque.md](../../../workstreams/ts2pant-any-unknown-opaque.md):
narrowing is consulted at the use site with `checker.getTypeAtLocation`, the
recovered type must be something `mapTsType` already knows how to lower, and the
boundary remains conservative at function signatures.

## Use-Site Narrowing

M4 keeps the opaque floor, but it lets ts2pant recover precision where the
TypeScript compiler has already done the work. At a dynamic use site, lowering
consults `checker.getTypeAtLocation(useNode)` instead of relying only on the
declared type.

When the declared type is dynamic (`any` or `unknown`) and the use-site type is
concrete enough for the current `mapTsType` coverage, the occurrence lowers to a
synthesized constant of that concrete sort rather than to `Opaque`. The
occurrence remains a fresh synthesized value; only its sort changes.

If the narrowed type is still dynamic, or if it lands in a shape that the current
type mapping does not support, ts2pant keeps the existing opaque lowering. The
behavior is additive: un-narrowed or unmappable reads stay byte-identical to the
opaque baseline.

The narrowing is intentionally local to the use site. Two reads of the same
dynamic variable can lower differently if TypeScript has narrowed them
differently at those occurrences.

## Signature Boundary

M4 does not trust caller-side narrowing across a function boundary. A parameter
declared `any` or `unknown` keeps `Opaque` at the signature sort, even if the
caller had previously narrowed the argument. That is the conservative
cross-function boundary for this milestone.

The result is a simple split:

- use-site narrowing can recover a concrete sort when the checker already knows
  one;
- the function signature still records the declared dynamic type as opaque;
- cross-function narrowing is not inferred by following caller flow.

This preserves the workstream's soundness boundary while recovering the
common intra-function precision wins from `typeof`, `Array.isArray`,
`instanceof`, and related checker-driven refinements.

## Validation Expectations

The fixture contract for this layer is precision recovery, not broad rewriting.
Representative uses that flow-narrow to supported concrete sorts should lower to
those sorts, while un-narrowed or unsupported uses should stay opaque. A
parameter whose declared type is `any` or `unknown` should still appear as
`Opaque` in the signature.

Existing opaque output should remain stable outside the recognized narrowing
sites, preserving the additive posture established by the M2/M3 opaque work.
