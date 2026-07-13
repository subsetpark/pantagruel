# Body Completeness Residual Ownership Audit

**Workstream:** ts2pant Body Completeness — Milestone 5
(`residual-body-diagnostic-cleanup`). **Date:** 2026-07-13. **Status:**
ownership audit; no translator behavior changed.

## Outcome

The post-M3 self-translation corpus has no remaining high-volume family that
belongs to the bounded body transformations delivered by this workstream.
Every normalized UNSUPPORTED bucket above the M5 threshold is now assigned to
a semantic owner or an intentional refusal.

The audit threshold is **more than 10 normalized diagnostic occurrences**.
This resolves the workstream's suggested completion criterion and includes the
two 11-count buckets. Counts are diagnostic occurrences, not necessarily unique
functions: one function can emit the same reason more than once.

## Method

Run from `tools/ts2pant`:

```sh
NODE_OPTIONS=--max-old-space-size=6144 npx tsx scripts/corpus-diag.mts --summary-only
```

The audit then enumerated every function behind the high-volume structural
reasons and classified its statement shape, local writes, loop exits, returned
value, and downstream translation dependency. The owner table is encoded in
`scripts/corpus-diag.mts`; a future high-volume reason that has not been audited
is printed as `UNASSIGNED`.

## Baseline

- 866 top-level functions
- 171 clean translations
- 690 functions with UNSUPPORTED output
- 5 build errors (`Function not found: visit`)

The five errors are extraction/name-resolution failures, not body-lowering
diagnostics, and are outside this workstream.

## Owner table

| Count | Normalized reason | Owner / disposition |
| ---: | --- | --- |
| 82 | `alias IR1ForeachCondStmt: discriminated union could not be registered for tagged Pantagruel encoding` | DU/type-shape encoding |
| 60 | `early-return predicate has side effects` | guard/foreign-call purity |
| 46 | `if-with-return block must contain only const bindings followed by a return` | generalized nested sequencing follow-up; decomposed below |
| 42 | `for-of loop is not a recognized build-list comprehension` | generalized accumulator/search-loop follow-up; decomposed below |
| 41 | `expression statement before return (only const / μ-search / if-early-return allowed)` | local-effect/statement SSA follow-up; decomposed below |
| 34 | `early-return value has side effects` | guard/foreign-call purity |
| 34 | `Map builder construction is not supported in this milestone` | Map-builder semantics follow-up |
| 31 | `unsupported pure expression in const initializer` | expression/type lowering |
| 22 | `local bindings or multiple statements before return` | generalized local-sequencing follow-up; decomposed below |
| 19 | `Set builder from iterable is not supported` | collection-constructor semantics follow-up |
| 19 | `switch default must end with return EXPR` | intentional until exhaustiveness/throw semantics exist |
| 14 | `switch case must end with return EXPR` | intentional fall-through/break/throw refusal |
| 13 | `statement is not supported by unified SSA body lowering` | statement-position SSA follow-up |
| 11 | `branch call is not a recognized Map/Set effect` | branch-local collection-effect follow-up |
| 11 | `let captured by closure that reassigns it is not supported` | intentional until closure conversion is modeled |

The 19/14 switch rows are classified function-by-function in
`switch-body-survey.md`. The 82-count DU row, 60/34 purity rows, and 31-count
initializer row only surface at body translation; their missing semantics are
not structural body transformations.

## Structural bucket decomposition

### Nested if-with-return blocks: 46 functions

The label is a scanner boundary, not a single missing if-conversion rule.

| Source shape | Functions | Disposition |
| --- | ---: | --- |
| Calls, foreign accessors, or downstream value lowering inside the branch | 24 | guard/foreign-call or expression lowering |
| Loop/traversal inside the branch | 14 | generalized loop/local sequencing |
| Further nested value/type control | 4 | genuine nested structural tail, but below the threshold |
| Local mutation inside the branch | 2 | local-effect SSA |
| `throw`/`try` control | 2 | intentional until exceptional control is modeled |

Representative examples are
`translate-body.ts:recognizeEarlyReturnArm` (calls and downstream value
lowering), `translate-body.ts:translateBodyExpr` (loop traversal),
`translate-types.ts:literalFromType` (the four-function nested-value tail),
`index.ts:main` (mutation), and `pant-wasm.ts:ensureWasmLoaded` (`try`/`throw`).
There is no greater-than-10 family of the bounded pure
`const*; return value` shape M1 owns.

### Unrecognized for-of loops: 42 functions

| Source shape | Functions | Disposition |
| --- | ---: | --- |
| Writes one or more local list accumulators | 26 | generalized accumulator sequencing |
| Search/validation loop with early exit | 10 | search/general-loop control, not a builder |
| Writes a local Set | 5 | collection-constructor/effect semantics |
| Nested or multiple loops | 1 | generalized loop sequencing |

The 26 list-writing functions do not form another returned-list comprehension
family. They include multi-accumulator record construction
(`annotations.ts:extractAnnotations`), a builder consumed by `join`
(`translate-types.ts:tupleCtorBaseName`), conditional parsing with `continue`
(`emit.ts:parseCheckOutput`), and SSA program assembly returning a different
record (`ir1-ssa-foreach.ts:lowerForeachShapeBAsGeneralLoop`). Even the small
direct-result examples depend on foreign-call purity or richer conditional
push semantics. M3's single returned accumulator with a comprehension target
is already covered; these cases need a general accumulator-value pipeline.

### Expression statements before return: 41 functions

| Source shape | Functions | Disposition |
| --- | ---: | --- |
| Assignment or registration mutation | 15 | local-effect/statement SSA |
| Local list write | 8 | generalized accumulator sequencing |
| Other effectful expression | 7 | statement effects |
| Local Set write | 5 | collection effects |
| Frame or `try` effect | 4 | scoped/exceptional effects |
| Foreign/runtime call | 2 | foreign/runtime effect modeling |

Examples include SSA version allocation
(`ir1-ssa-scalars.ts:nextScalarSsaWrite`), registration pushes
(`ir1-build.ts:tryBuildL1PureSubExpression`), narrowing-frame management
(`ir1-build-body.ts:withNarrowingFrame`), and wasm loading
(`pant-wasm.ts:loadParser`). Admitting arbitrary expression statements in the
pure prelude would erase exactly the effects that these functions depend on.

### Local bindings or multiple statements: 22 functions

| Source shape | Functions | Disposition |
| --- | ---: | --- |
| Loop/while-based sequencing | 15 | generalized local/loop sequencing |
| Foreign-call sequencing | 2 | foreign-call effects |
| Local mutation | 2 | local-effect SSA |
| Multi-local value flow | 2 | generalized local sequencing |
| Branch sequencing | 1 | generalized statement lowering |

This is the fallback emitted after the bounded pure-prelude recognizers decline
the body. The dominant functions are source scanners such as
`translate-body.ts:extractReturnExpression`, `purity.ts:expressionIsPure`, and
`translate-types.ts:detectDiscriminatedUnion`; they are not missing simple
let-elimination.

### Unified SSA statements: 13 generic occurrences

The generic 13-count row and the four explicit `SwitchStatement` occurrences
come from nine unique functions because some functions emit the reason more
than once. Four are mutation-bearing visitor switches, three update local
Map/cache state, one allocates a local collection write, and one uses another
unsupported statement form. These require statement-position effect SSA. The
four switches are also accounted for by the M4 switch survey.

## Collection and closure boundaries

- **Map construction (34):** these functions build registries, caches, and SSA
  state. A faithful returned Map needs the guarded Stage-A membership / Stage-B
  value rule pair and alias/overwrite semantics. M2 intentionally deferred
  this to a Map-specific target; the count does not justify weakening that
  boundary.
- **Set-from-iterable construction (19):** these are seeded/cloned Sets rather
  than M2's empty finite Set plus straight-line `.add` form. Their semantics
  require iterating the seed and preserving membership, so they belong with
  collection constructors.
- **Branch-local Map/Set effects (11):** these are state changes in branches,
  not pure expression calls. They belong with collection location SSA and
  branch joins.
- **Closure-captured reassigned lets (11):** closure conversion changes
  environment and lifetime semantics. The existing dedicated diagnostic is an
  intentional, precise refusal.

## Verdict

Close M5 as an audit-only reconciliation milestone and close the Body
Completeness workstream. Do not create another behavior gameplan under this
workstream: no remaining greater-than-10 source-shape family maps to the
bounded if-conversion, let-elimination, record-return, local finite-builder, or
structured-iteration targets it owns.

Future work should start from the semantic owner rather than the coarse body
diagnostic:

- generalized accumulator and statement-position SSA;
- Map construction and seeded collection constructors;
- guard/foreign-call purity and expression lowering;
- DU/type-shape encoding;
- closure conversion or exceptional-control semantics only when demanded.

Re-open Body Completeness only if a future corpus run prints `UNASSIGNED` above
the threshold and inspection finds a bounded, unambiguous source family with a
clear existing Pant target.
