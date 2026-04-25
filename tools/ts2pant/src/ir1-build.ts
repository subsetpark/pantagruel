/**
 * TS AST → Layer 1 builder.
 *
 * **M1 scope** (workstream M1: imperative-ir-conditionals): conditional
 * **value** forms only — ternary chains, if-with-returns, switch without
 * fall-through, `&&`/`||` when both operands are statically Bool-typed.
 * All collapse to one canonical L1 `cond([(g, v)], otherwise)` form.
 *
 * Sub-expressions inside conditionals (the guard, the value, the switch
 * discriminant) are translated through the legacy `translateBodyExpr`
 * pipeline and wrapped via the L1 `from-l2` adapter — see `ir1.ts`
 * for the lifetime contract on that adapter.
 *
 * **Conservative-refusal policy 3(b)**: when the build pass cannot prove
 * an equivalence (switch with possible fall-through, `==`/`===`-mixing
 * default-not-last, non-Bool short-circuit, non-literal switch case,
 * object-literal arms in a value-position cond), the function rejects
 * with an `unsupported` reason. Lifting any of these is a follow-up
 * within the workstream — see CLAUDE.md §"Imperative IR".
 *
 * **Hard rule per equivalence class** (workstream decision 4): every
 * conditional-value-shaped TS construct goes through this builder once
 * Patch 3 cuts over. During Patch 2, the path is gated by the
 * `TS2PANT_USE_L1=1` env var so legacy and L1 paths can be compared
 * byte-for-byte.
 */

import ts from "typescript";
import { irWrap } from "./ir.js";
import { lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  ir1Binop,
  ir1Cond,
  ir1FromL2,
  ir1LitBool,
  ir1LitNat,
  ir1LitString,
  ir1Unop,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { isStaticallyBoolTyped } from "./purity.js";
import type { SymbolicState, UniqueSupply } from "./translate-body.js";
import {
  expressionHasSideEffects,
  isBodyUnsupported,
  translateBodyExpr,
} from "./translate-body.js";
import type { NumericStrategy } from "./translate-types.js";

/**
 * Context threaded through L1 build. Mirrors the parameter set of
 * `translateBodyExpr` so sub-expression delegation stays trivial.
 */
export interface L1BuildContext {
  checker: ts.TypeChecker;
  strategy: NumericStrategy;
  paramNames: ReadonlyMap<string, string>;
  state: SymbolicState | undefined;
  supply: UniqueSupply;
}

export type L1BuildResult = IR1Expr | { unsupported: string };

export const isL1Unsupported = (
  r: L1BuildResult,
): r is { unsupported: string } =>
  typeof r === "object" && r !== null && "unsupported" in r;

// ---------------------------------------------------------------------------
// Recognizer entry — is this TS node a conditional shape M1 handles?
// ---------------------------------------------------------------------------

/**
 * True when `node` is a conditional **value** form M1 normalizes:
 * ternary, if-with-returns, switch w/o fall-through, or a Bool-typed
 * `&&`/`||`. The Bool-type check requires the checker, so the function
 * takes one as a parameter rather than being purely syntactic.
 *
 * `&&`/`||` with a non-Bool operand is *not* a conditional form for M1
 * — it stays on the legacy translateOperator path (or, post-cutover,
 * rejects as UNSUPPORTED at the call site).
 */
export function isL1ConditionalForm(
  node: ts.Node,
  checker: ts.TypeChecker,
): boolean {
  if (
    ts.isConditionalExpression(node) ||
    ts.isIfStatement(node) ||
    ts.isSwitchStatement(node)
  ) {
    return true;
  }
  if (
    ts.isBinaryExpression(node) &&
    (node.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
      node.operatorToken.kind === ts.SyntaxKind.BarBarToken)
  ) {
    return (
      isStaticallyBoolTyped(node.left, checker) &&
      isStaticallyBoolTyped(node.right, checker)
    );
  }
  return false;
}

// ---------------------------------------------------------------------------
// Sub-expression delegation: translate via legacy, wrap into L1 via from-l2
// ---------------------------------------------------------------------------

/**
 * Translate a non-conditional sub-expression via the legacy pipeline and
 * wrap as L1. Used for guards, values, and switch discriminants inside
 * an L1 conditional during M1 — those positions receive arbitrary TS
 * expressions whose normalization isn't M1's concern.
 *
 * Rejects collection-mutation sub-expressions (the legacy result type
 * `BodyResult` admits an "effect" variant that's only valid in
 * statement position; an L1 expression cannot host one).
 */
function buildSubExpr(expr: ts.Expression, ctx: L1BuildContext): L1BuildResult {
  // Object literals don't lower to a Pantagruel value — Pant has no
  // record-constructor expression syntax. Reject *before* translating
  // so the error message is precise (legacy translateBodyExpr would
  // produce garbage instead).
  if (ts.isObjectLiteralExpression(expr)) {
    return { unsupported: "object literal in conditional arm" };
  }
  const result = translateBodyExpr(
    expr,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(result)) {
    return { unsupported: result.unsupported };
  }
  if ("effect" in result) {
    return {
      unsupported: "collection mutation cannot appear in a conditional arm",
    };
  }
  return ir1FromL2(irWrap(result.expr));
}

// ---------------------------------------------------------------------------
// Top-level dispatch
// ---------------------------------------------------------------------------

/**
 * Build a Layer 1 value-position conditional from a TS conditional shape.
 * Dispatches on node kind; rejects with `unsupported` when the form
 * cannot be canonicalized (per conservative-refusal policy 3(b)).
 */
export function buildL1Conditional(
  expr: ts.Expression | ts.IfStatement | ts.SwitchStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  if (ts.isConditionalExpression(expr)) {
    return buildFromTernary(expr, ctx);
  }
  if (ts.isIfStatement(expr)) {
    return buildFromIfStatement(expr, ctx);
  }
  if (ts.isSwitchStatement(expr)) {
    return buildFromSwitchStatement(expr, ctx);
  }
  if (
    ts.isBinaryExpression(expr) &&
    (expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
      expr.operatorToken.kind === ts.SyntaxKind.BarBarToken)
  ) {
    return buildFromShortCircuit(expr, ctx);
  }
  return { unsupported: "not a recognized conditional form" };
}

/**
 * Compose an L1 cond from pre-built L1 arm pairs and a TS terminal.
 *
 * Caller responsibility: pre-built arms are typically wrapped via
 * `ir1FromL2(irWrap(opaqueExpr))` from `inlineConstBindings`'s
 * already-translated `arms: [OpaqueExpr, OpaqueExpr][]` list. Terminal
 * is translated by this function — either as an L1 conditional shape
 * (if it matches one) or as a plain expression delegated through the
 * `from-l2` adapter.
 *
 * The flattening rule: if the terminal is itself a conditional that
 * lowers to an L1 cond, its arms get appended to the prelude arms and
 * its otherwise becomes the combined cond's otherwise. This produces
 * one L1 cond node that the lowerer flattens to one L2 cond — matching
 * today's legacy `cond([...arms, [true, terminal]])` materialization.
 */
export function buildL1ConditionalFromArms(
  preludeArms: ReadonlyArray<readonly [IR1Expr, IR1Expr]>,
  terminal: ts.Expression | ts.IfStatement | ts.SwitchStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  const builtArms: Array<readonly [IR1Expr, IR1Expr]> = [...preludeArms];

  const terminalIsConditional =
    ts.isIfStatement(terminal) ||
    ts.isSwitchStatement(terminal) ||
    (ts.isExpression(terminal) && isL1ConditionalForm(terminal, ctx.checker));

  if (!terminalIsConditional) {
    if (builtArms.length === 0) {
      return { unsupported: "no conditional shape to build" };
    }
    if (!ts.isExpression(terminal)) {
      return { unsupported: "unsupported terminal form" };
    }
    const otherwise = buildSubExpr(terminal, ctx);
    if (isL1Unsupported(otherwise)) {
      return otherwise;
    }
    return ir1Cond(
      builtArms as [readonly [IR1Expr, IR1Expr], ...typeof builtArms],
      otherwise,
    );
  }

  // Terminal IS a conditional: build it as an L1 cond, then merge.
  const terminalL1 = buildL1Conditional(terminal, ctx);
  if (isL1Unsupported(terminalL1)) {
    return terminalL1;
  }
  if (terminalL1.kind === "cond") {
    for (const [g, v] of terminalL1.arms) {
      builtArms.push([g, v] as const);
    }
    if (builtArms.length === 0) {
      return { unsupported: "no conditional shape to build" };
    }
    return ir1Cond(
      builtArms as [readonly [IR1Expr, IR1Expr], ...typeof builtArms],
      terminalL1.otherwise,
    );
  }
  // Terminal lowered to a non-cond L1 (e.g., switch with only default
  // collapsed to its default value, or short-circuit returning a binop).
  // Treat the L1 result as the otherwise.
  if (builtArms.length === 0) {
    return terminalL1;
  }
  return ir1Cond(
    builtArms as [readonly [IR1Expr, IR1Expr], ...typeof builtArms],
    terminalL1,
  );
}

/**
 * Lower an L1 build result to an `OpaqueExpr` ready to drop into the
 * legacy pipeline. Combines `lowerL1Expr` and `lowerExpr` for callers
 * that want a one-liner.
 */
export function lowerL1ToOpaque(l1: IR1Expr): OpaqueExpr {
  return lowerExpr(lowerL1Expr(l1));
}

// ---------------------------------------------------------------------------
// Ternary chain flattening (right-associative)
// ---------------------------------------------------------------------------

function buildFromTernary(
  expr: ts.ConditionalExpression,
  ctx: L1BuildContext,
): L1BuildResult {
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  let current: ts.Expression = expr;
  // Right-leaning chain: a ? x : (b ? y : (c ? z : w)) → flatten.
  while (ts.isConditionalExpression(current)) {
    const guard = buildSubExpr(current.condition, ctx);
    if (isL1Unsupported(guard)) {
      return guard;
    }
    const value = buildSubExpr(current.whenTrue, ctx);
    if (isL1Unsupported(value)) {
      return value;
    }
    arms.push([guard, value] as const);
    current = current.whenFalse;
  }
  // Terminal else (non-ternary).
  const otherwise = buildSubExpr(current, ctx);
  if (isL1Unsupported(otherwise)) {
    return otherwise;
  }
  if (arms.length === 0) {
    // Cannot happen — we entered the loop because expr is a ternary.
    return { unsupported: "ternary with no arms (unreachable)" };
  }
  return ir1Cond(
    arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
    otherwise,
  );
}

// ---------------------------------------------------------------------------
// If-statement → cond (if-with-returns required for value position)
// ---------------------------------------------------------------------------

function buildFromIfStatement(
  stmt: ts.IfStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  let current: ts.IfStatement = stmt;
  while (true) {
    if (!current.elseStatement) {
      return {
        unsupported:
          "if-without-else cannot lower to value-position cond (need both branches)",
      };
    }
    const guard = buildSubExpr(current.expression, ctx);
    if (isL1Unsupported(guard)) {
      return guard;
    }
    const thenExpr = extractReturnFromStatement(current.thenStatement);
    if (!thenExpr) {
      return {
        unsupported: "if-then branch must contain a single return-with-value",
      };
    }
    const thenValue = buildSubExpr(thenExpr, ctx);
    if (isL1Unsupported(thenValue)) {
      return thenValue;
    }
    arms.push([guard, thenValue] as const);

    // Else: another IfStatement (chain) or a terminal block-with-return.
    if (ts.isIfStatement(current.elseStatement)) {
      current = current.elseStatement;
      continue;
    }
    const elseExpr = extractReturnFromStatement(current.elseStatement);
    if (!elseExpr) {
      return {
        unsupported: "if-else branch must contain a single return-with-value",
      };
    }
    const elseValue = buildSubExpr(elseExpr, ctx);
    if (isL1Unsupported(elseValue)) {
      return elseValue;
    }
    if (arms.length === 0) {
      return {
        unsupported: "if-statement walk produced no arms (unreachable)",
      };
    }
    return ir1Cond(
      arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
      elseValue,
    );
  }
}

/**
 * Extract `return EXPR` from a then/else branch — accepts either a
 * `ReturnStatement` directly or a `Block` containing exactly one
 * `ReturnStatement` (after filtering throws/asserts, which act as guards
 * but aren't represented in L1 expression position). Mirrors today's
 * `extractReturnFromBranch` in translate-body.ts:2841 but is kept local
 * to ir1-build.ts so the L1 builder doesn't depend on translate-body's
 * internal helpers staying exported.
 *
 * **Rejection of object-literal returns**: handled at `buildSubExpr`
 * (called downstream); not here.
 */
function extractReturnFromStatement(stmt: ts.Statement): ts.Expression | null {
  if (ts.isReturnStatement(stmt) && stmt.expression) {
    return stmt.expression;
  }
  if (ts.isBlock(stmt) && stmt.statements.length === 1) {
    const s = stmt.statements[0]!;
    if (ts.isReturnStatement(s) && s.expression) {
      return s.expression;
    }
  }
  return null;
}

// ---------------------------------------------------------------------------
// Switch → cond (cases as arms, default as otherwise)
// ---------------------------------------------------------------------------

function buildFromSwitchStatement(
  stmt: ts.SwitchStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  // Validate clause structure: every case ends with `return`/`break`/
  // `throw`; default is last (or the only clause); reject fall-through.
  const clauses = stmt.caseBlock.clauses;
  if (clauses.length === 0) {
    return { unsupported: "empty switch" };
  }

  let defaultIdx: number = -1;
  for (let i = 0; i < clauses.length; i++) {
    if (ts.isDefaultClause(clauses[i]!)) {
      if (i !== clauses.length - 1) {
        return { unsupported: "switch default must be the last clause" };
      }
      defaultIdx = i;
    }
  }
  if (defaultIdx === -1) {
    return {
      unsupported:
        "switch without default; literal-union exhaustiveness deferred (workstream open question)",
    };
  }

  // Discriminant must be side-effect-free — we structurally share it
  // across arms, so re-evaluation would diverge from TS's once-evaluated
  // semantics if it has effects.
  if (expressionHasSideEffects(stmt.expression, ctx.checker)) {
    return { unsupported: "switch discriminant has side effects" };
  }
  const disc = buildSubExpr(stmt.expression, ctx);
  if (isL1Unsupported(disc)) {
    return disc;
  }

  // Each non-default case: literal label, body ends in `return EXPR`.
  // M1 only supports return-cases; break/throw cases are rejected
  // (no coherent value-position lowering).
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  for (let i = 0; i < defaultIdx; i++) {
    const clause = clauses[i] as ts.CaseClause;
    const labelL1 = buildCaseLabel(clause.expression, ctx);
    if (isL1Unsupported(labelL1)) {
      return labelL1;
    }
    const ret = extractCaseReturn(clause);
    if (!ret) {
      return {
        unsupported:
          "switch case must end with `return EXPR` (no fall-through, no break/throw cases in M1)",
      };
    }
    const value = buildSubExpr(ret, ctx);
    if (isL1Unsupported(value)) {
      return value;
    }
    arms.push([ir1Binop("eq", disc, labelL1), value] as const);
  }

  // Default clause.
  const defaultClause = clauses[defaultIdx] as ts.DefaultClause;
  const defaultRet = extractCaseReturn(defaultClause);
  if (!defaultRet) {
    return {
      unsupported: "switch default must end with `return EXPR`",
    };
  }
  const otherwise = buildSubExpr(defaultRet, ctx);
  if (isL1Unsupported(otherwise)) {
    return otherwise;
  }

  if (arms.length === 0) {
    // Switch with only a default: collapse to just the default value.
    return otherwise;
  }
  return ir1Cond(
    arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
    otherwise,
  );
}

/**
 * Switch case labels must be literal — number, string, or boolean.
 * Non-literal labels (computed expressions) reject. Literal nat are
 * built directly to avoid re-translating through the legacy pipeline,
 * which is mechanical for literals but adds a `from-l2` wrap that
 * doesn't simplify under structural equality.
 */
function buildCaseLabel(
  label: ts.Expression,
  ctx: L1BuildContext,
): L1BuildResult {
  if (ts.isNumericLiteral(label)) {
    const n = Number(label.text);
    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
      return ir1LitNat(n);
    }
    if (Number.isFinite(n) && Number.isInteger(n) && n < 0) {
      return ir1Unop("neg", ir1LitNat(-n));
    }
    return { unsupported: "switch case label: non-integer numeric literal" };
  }
  if (ts.isStringLiteral(label)) {
    return ir1LitString(label.text);
  }
  if (label.kind === ts.SyntaxKind.TrueKeyword) {
    return ir1LitBool(true);
  }
  if (label.kind === ts.SyntaxKind.FalseKeyword) {
    return ir1LitBool(false);
  }
  // Could be a const-bound identifier, a property access (enum), etc.
  // Defer those to a follow-up — M1 keeps switch labels strictly literal
  // to avoid mis-treating non-literal labels as fall-through-eligible
  // when their evaluation order matters.
  void ctx;
  return {
    unsupported: "switch case label must be a literal (number/string/bool)",
  };
}

/**
 * Extract the `return EXPR` from a case clause. Accepts the canonical
 * shape: a single `return EXPR;` statement (possibly preceded by a
 * single `break;` that's structurally unreachable — but we reject
 * `break` since it implies fall-through-by-omission elsewhere). Reject
 * any case body containing more than one statement, or with a
 * non-return last statement.
 */
function extractCaseReturn(
  clause: ts.CaseClause | ts.DefaultClause,
): ts.Expression | null {
  // Filter to the structural body. Each case body is a list of
  // statements at the top level (TS doesn't wrap them in a Block by
  // default). M1 requires exactly one statement: `return EXPR;`.
  const stmts = clause.statements;
  if (stmts.length === 0) {
    // Empty case body = fall-through. Reject.
    return null;
  }
  // Walk past a trailing `break;` if present (no-op in a case body that
  // ends with return — but if the *only* stmt is break, that's a
  // statement-position effect M1 doesn't support).
  let lastIdx = stmts.length - 1;
  if (lastIdx > 0 && stmts[lastIdx]!.kind === ts.SyntaxKind.BreakStatement) {
    lastIdx--;
  }
  // Also accept `{ return EXPR; }` block.
  const last: ts.Statement = stmts[lastIdx]!;
  if (
    ts.isBlock(last) &&
    last.statements.length === 1 &&
    ts.isReturnStatement(last.statements[0]!) &&
    last.statements[0]!.expression
  ) {
    return last.statements[0]!.expression!;
  }
  if (lastIdx !== stmts.length - 1 && lastIdx < 0) {
    return null;
  }
  // Only one effective statement allowed (after trailing-break trim).
  if (lastIdx !== 0) {
    return null;
  }
  if (ts.isReturnStatement(last) && last.expression) {
    return last.expression;
  }
  return null;
}

// ---------------------------------------------------------------------------
// && / || → cond (Bool-typed only)
// ---------------------------------------------------------------------------

function buildFromShortCircuit(
  expr: ts.BinaryExpression,
  ctx: L1BuildContext,
): L1BuildResult {
  if (
    !isStaticallyBoolTyped(expr.left, ctx.checker) ||
    !isStaticallyBoolTyped(expr.right, ctx.checker)
  ) {
    return {
      unsupported:
        "&&/|| requires both operands to be statically Bool-typed (workstream conservative-refusal 3(b))",
    };
  }
  const left = buildSubExpr(expr.left, ctx);
  if (isL1Unsupported(left)) {
    return left;
  }
  const right = buildSubExpr(expr.right, ctx);
  if (isL1Unsupported(right)) {
    return right;
  }
  if (expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken) {
    return ir1Binop("and", left, right);
  }
  return ir1Binop("or", left, right);
}
