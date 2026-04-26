# M3 Cutover — Analysis & Blocker

## Summary

I branched off `zax--ts2pant-m3-iteration-mutation` to perform the
deferred cutover work but hit an architectural blocker in the very first
case I attempted (for-of Shape A, the simplest cutover candidate).
Documenting it here so the next session can pick up cleanly.

## What needed to land

Per the M3 PR (#134) description, the deferred work is:
1. L1 build pass: `buildL1ForOf`, `buildL1ForEachCall`,
   `buildL1ReduceCall`, `buildL1CondStmt` translating TS AST → L1
   statements.
2. Cutover: replace legacy `translateForOfLoop`,
   `translateForEachStmt`, `translateReduceCall` and the
   branched-mutation arm of `symbolicExecute` with the L1 path; delete
   `mergeOverrides` / `combineCond` / `classifyLoopStmt` / etc.
3. Shape B (accumulator fold) and Shape C (.reduce as expression)
   recognition at the L1 build site.

## The blocker: IR guard semantics for Shape A

Legacy emits Shape A as a quantified equation with **no typed
quantifier**, just a `gIn` (binder-introducing) guard:

  ```
  forall(
    params=[],
    guards=[gIn(iterName, arrExpr)],
    body=<equation>,
  )
  ```

This prints as `all x in arr | p' x = e.`

My Patch 5 `lowerL1Foreach` produces a `quantified-stmt([{x, T}],
[in(x, src)], …)` which lowers to **typed quantifier + gExpr guard**:

  ```
  forall(
    params=[param(x, T)],
    guards=[gExpr(in(x, src))],
    body=<equation>,
  )
  ```

This prints as `all x: T, x in src | p' x = e.` — a **superfluous type
annotation** vs the legacy form. The discrepancy is because:

1. `IREquation.guards` is `IRExpr[]`, lowered uniformly via `gExpr`.
2. There is no IR-level representation of `gIn` (the binder-introducing
   guard form Pantagruel uses for `x in src` quantification).
3. To match legacy output, the L1 → L2 lowering would need to either:
   - Add an IR guard ADT distinguishing `gExpr`-style from
     `gIn`-style guards, OR
   - Emit the equation by constructing OpaqueExpr quantifier+guard
     directly (bypassing IR for this specific case), OR
   - Accept the regression (typed quantifier appears in every iteration
     fixture's output)

## Why it matters

`pant --check` *probably* accepts both forms equivalently — Pantagruel
infers types from the iteration source. But the snapshot regression
would touch every iteration fixture (~10 files) and produce a noisier
output. Given snapshot policy says regressions are acceptable for
cleaner output, this regression is *uglier*, not cleaner.

The right architectural fix is option 1: extend the IR guard
representation. That's a structural change to `IREquation` and
`lowerEquation` that should land before the cutover, otherwise every
iteration use-site bakes in the suboptimal output shape.

## Proposed path forward

1. **Patch CO-0**: Extend IR guard types — add an `IRGuard` ADT (or
   tagged union of IRExpr + gIn-style) and update `IREquation.guards` /
   `IRAssertExit.guards` / `lowerEquation` / `lowerAssert`. Update
   Patch 5's `lowerL1Foreach` to emit `gIn`-style guards. Snapshot test
   verifies output matches legacy `all x in src | ...`.

2. **Patch CO-1**: Cutover `translateForOfLoop` Shape A bodies to
   `buildL1ForOfShapeA` → `lowerL1Stmt` → `emitStmt`. Delete
   Shape A logic from `classifyLoopStmt`/`translateForOfLoopBody`.

3. **Patch CO-2**: Cutover `translateForEachStmt` similarly.

4. **Patch CO-3**: Implement Shape B build pass + cutover. Shape B
   produces `write{property-field}` with a `comb`-RHS, not a quantified
   envelope, so the IR guard issue doesn't apply.

5. **Patch CO-4**: Cutover `translateReduceCall`. Shape C is
   expression-position — produces L2 `comb`, no IR guard issue.

6. **Patch CO-5**: Cutover branched-mutation arm of `symbolicExecute`.
   Requires `buildL1IfMutation` covering all mutation kinds (property,
   Map, Set) inside branches. The state-merge integration is the
   subtle piece — `emitStmt` finalizes equations immediately, which
   means a later if-statement on the same write-key would conflict.
   Solution: thread the merged WriteAccs back into outer
   `state.writes` rather than finalizing inside emitStmt. Requires a
   "merge-only, don't finalize" entry point on emitStmt.

7. **Patch CO-6**: Cleanup — delete `classifyLoopStmt`,
   `translateForOfLoopBody`, `translateForOfLoop`,
   `translateForEachStmt`, `translateReduceCall`, `mergeOverrides`,
   `combineCond`, `ShapeBLeaf`, `COMPOUND_ASSIGN_TO_FOLD`, etc.

## Recommendation

The cutover is structurally sound and the M3 plumbing PR (#134)
provides the foundation. The IR guard fix (Patch CO-0) is the
prerequisite — it's a small focused patch (~50 lines) that resolves
the output-shape mismatch.

I haven't attempted CO-0 in this session because the analysis was
already substantial; doing the architectural fix correctly deserves
its own focused effort with proper test coverage.
