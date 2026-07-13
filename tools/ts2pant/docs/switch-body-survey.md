# Switch Body Residual Survey

**Workstream:** ts2pant Body Completeness — Milestone 4
(`switch-body-completeness`). **Date:** 2026-07-13. **Status:** read-only
measurement; no translator behavior changed.

## Question

Milestone 3 left switch-shaped diagnostics in the self-translation corpus.
Milestone 4 was conditional on those residuals containing a meaningful family
of bounded, no-fall-through value switches with a clear `cond` target. This
survey classifies the residuals before committing to implementation.

## Post-M3 baseline

Run from `tools/ts2pant`:

```sh
NODE_OPTIONS=--max-old-space-size=6144 npx tsx scripts/corpus-diag.mts --summary-only
```

Result:

- 866 top-level functions
- 171 clean translations
- 690 functions with UNSUPPORTED output
- 5 build errors
- `for-of loop is not a recognized build-list comprehension`: 42, down from
  the post-M2 baseline of 52

The switch-related buckets contain 47 functions:

| Residual bucket | Count |
| --- | ---: |
| `switch default must end with return EXPR` | 19 |
| `switch case must end with return EXPR` | 14 |
| `switch case label must be a literal` | 7 |
| unified SSA rejects `SwitchStatement` | 4 |
| `switch block const has side effects` | 3 |

## Classification

Every residual was inspected against its TypeScript switch shape.

| Shape | Count | Classification |
| --- | ---: | --- |
| Exhaustive switch whose `default` throws | 19 | Intentionally unsupported by the M4 boundary: a throw-only value arm has no Pant value and cannot be used as a `cond` otherwise branch. |
| Empty/grouped case fall-through | 12 | Intentionally unsupported: the current switch-to-`cond` contract rejects fall-through, including grouped labels. |
| `ts.SyntaxKind.*` computed/property-access case labels | 7 | Intentionally unsupported: M4 keeps non-literal labels out of scope because label evaluation and constant resolution are separate concerns. |
| Statement-position visitor switch with local mutation and `break` | 4 | Not value-position switch lowering. These require statement/SSA effect modeling, not the existing switch-to-`cond` contract. |
| Block-local calls classified as effectful | 3 | Owned by purity/foreign-call analysis rather than structural switch lowering. |
| Complex block-return cases | 2 | `emit.ts:renderPropResult` and `ir1-lower.ts:lowerL1Expr` contain calls, nested conditionals, and collection operations; they are not the bounded pure block-return subset M4 proposed to admit. |

The 19 exhaustive-throw functions are:

- `brand-precondition.ts:compare`
- `builtins.ts:moduleForNamespace`
- `ir-emit.ts:{lowerBinop,lowerUnop,lowerCombiner,combinerToBinop}`
- `ir1-printer.ts:{formatIR1SsaLocation,readExpressionKey,formatUnopWithSsaReads,locationAsPrimedRule,formatIR1Literal,formatUnop,binopGlyph,unopGlyph}`
- `ir1-ssa-counter-loop.ts:{boundCmpToOpaque,combinerToOpaque}`
- `ir1-ssa-scalars.ts:{lowerScalarBinop,lowerScalarUnop}`
- `translate-body.ts:makeCombiner`

The 12 fall-through functions are:

- `index.ts:getStrategy`
- `ir1-ssa-collections.ts:{collectionSsaReadExpr,lowerCollectionSsaExprToOpaque,isCollectionSsaExpr}`
- `ir1-ssa-counter-loop.ts:{lastCounterValue,foldForBinop,exprReferencesVar,countTargetReads}`
- `ir1-ssa-scalars.ts:{scalarSsaReadExpr,lowerScalarSsaExprToOpaque,isScalarSsaExpr}`
- `ir1-substitute.ts:freeVarsIR1SsaLocation`

The seven non-literal-label functions all switch on TypeScript compiler enum
members:

- `ir-build.ts:binopToReduceInfo`
- `ir1-build.ts:{binaryOperatorToL1,compoundOpToBinop,explicitOpToBinop}`
- `narrowing-recognizer.ts:discriminantFactFromEquality`
- `translate-body.ts:binopToReduceInfo`
- `translate-signature.ts:translateOperator`

The four statement-position switches are
`ir1-printer.ts:consumeScalarReadsForExpr` and
`ir1-ssa-fixed-point.ts:{collectAllNames,collectFreeVars,collectIr1FreeVars}`.
The three effectful-block cases are
`ir1-printer.ts:{formatIR1Expr,formatIR1ExprWithSsaReads}` and
`ir1-ssa-foreach.ts:lowerShapeAExpr`.

## Verdict

Do not create an M4 behavior gameplan. The bounded, no-fall-through value
switches M4 proposed to add are already supported and covered by the existing
switch fixtures. None of the 47 self-translation residuals belongs to that
subset, so an implementation milestone would be vacuous and would not move a
corpus bucket.

Close M4 as measurement-only. Preserve the current precise refusals and route
future demand by semantic owner:

- exhaustive throw/default elimination needs an exhaustiveness-aware partial
  value encoding;
- grouped labels need explicit fall-through semantics;
- `SyntaxKind` labels need constant/enum resolution;
- visitor switches need statement-position SSA lowering;
- effectful block values need purity/foreign-call admission.

The next workstream action is M5's residual ownership pass, not switch lowering.
