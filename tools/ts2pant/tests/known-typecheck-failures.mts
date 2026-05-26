/**
 * Fixtures whose emitted Pantagruel does not currently parse / typecheck
 * through the wasm checker. Each entry is the `${file} > ${funcName}` key
 * the test reports, paired with a short tag identifying the cause.
 *
 * Wiring the wasm typechecker into snapshot tests surfaced these. Most
 * are real bugs in the emit pipeline (previously the snapshots
 * stabilized invalid output); a few are deliberate translation choices
 * where a single-function fixture is not standalone-checkable. New
 * fixtures must typecheck; entries here are an explicit work list, not
 * a permitted-failure mode.
 *
 * Shared between `constructs.test.mts` (snapshot suite) and
 * `ir-equivalence.test.mts` (legacy/IR string-equality suite) so a
 * failure surfaced in one place is muted in both. When a bug is fixed
 * (or a fixture is reframed to be standalone-checkable), drop the
 * matching entries here and the corresponding tests pick up the
 * typecheck automatically.
 *
 * Const-binding SSA deliberately moved several fixtures from prelude
 * rejection into emitted Pant that can fail later at the free-call-decl
 * boundary. Keep those entries only while the emitted document really
 * fails the wasm typecheck because a surviving call lacks a Pant
 * declaration; outputs that now typecheck, including parseable
 * `> UNSUPPORTED:` comments, should not stay on this list.
 *
 * Failure classes:
 *   - "$-binder-leak"     hygienic `$N` names reach the emitted text
 *                         (Pant identifiers cannot contain `$`); needs
 *                         synthCell/cellRegisterName plumbing through
 *                         the offending lowering path.
 *   - "requires-external-context"
 *                         class-method fixtures translate to a single-
 *                         method module and intentionally omit the
 *                         synthetic class domain + per-field accessor
 *                         rules — those belong to the surrounding
 *                         module context (only interfaces are processed
 *                         by `extractAllTypes`). Not a bug; these
 *                         fixtures are not standalone-checkable in the
 *                         wasm checker path.
 *   - "list-literal"      emits `[a, b]` for tuple/array initializers
 *                         where Pant has no list literal target.
 *   - "free-call-decl"    user calls a TS function that ts2pant doesn't
 *                         translate; the call survives but no Pant
 *                         declaration is synthesized for the callee.
 *   - "var-rejected"      local `var` binding fixture; `var` remains out of
 *                         scope and should keep a dedicated diagnostic.
 *   - "closure-captured-reassignment"
 *                         reassignment through a closure-captured `let`;
 *                         deliberately out of M2 scope.
 *   - "annotation-types"  the user's `@pant` annotation is itself
 *                         ill-typed against the emitted signature
 *                         (e.g. comparing a Bool result to an Int).
 */
export const KNOWN_TYPECHECK_FAILURES = new Map<string, string>([
  ["control-flow.ts > max", "$-binder-leak"],
  ["expressions-array.ts > nameLengths", "$-binder-leak"],
  ["expressions-boolean.ts > and", "$-binder-leak"],
  ["expressions-boolean.ts > or", "$-binder-leak"],
  ["expressions-calls.ts > methodCall", "free-call-decl"],
  ["expressions-const-chained-impure.ts > chainedArrayCount", "free-call-decl"],
  ["expressions-const-pure-calls.ts > constMathMax", "free-call-decl"],
  ["expressions-const-pure-calls.ts > constStringMethod", "free-call-decl"],
  ["expressions-const-pure-calls.ts > constChainedPure", "free-call-decl"],
  ["expressions-const-side-effectful.ts > constReplaceLiteral", "free-call-decl"],
  ["expressions-const-side-effectful.ts > constArrayFromMap", "free-call-decl"],
  ["expressions-const-side-effectful.ts > constMapEntries", "free-call-decl"],
  ["expressions-let-closure-captured.ts > letForEachCapturedTotal", "closure-captured-reassignment"],
  ["expressions-let-closure-captured.ts > letMapCapturedCount", "closure-captured-reassignment"],
  ["expressions-let-immutable.ts > letMathMax", "free-call-decl"],
  ["expressions-var-rejected.ts > varBinding", "var-rejected"],
  ["expressions-var-rejected.ts > varReassignment", "var-rejected"],
  ["functions-class.ts > Account.getBalance", "requires-external-context"],
  ["functions-class.ts > Account.deposit", "requires-external-context"],
  ["types-composite.ts > getPoint", "list-literal"],
]);
