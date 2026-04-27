/**
 * M4 nullish recognizer (workstream M4: equality and nullish).
 *
 * Routes the equality/nullish equivalence class onto Layer 1's
 * canonical `is-nullish` form. Five surface forms collapse to one
 * shape:
 *
 *   - `x == null` / `x != null`            (loose-eq variant)
 *   - `x === null` / `x !== null`          (strict, null literal)
 *   - `x === undefined` / `x !== undefined`  (strict, undefined literal)
 *   - `x === null || x === undefined`       (long form, positive)
 *   - `x !== null && x !== undefined`       (long form, negated)
 *   - `typeof x === 'undefined'` / `!==`    (typeof-undefined family)
 *
 * Negated leaves wrap as `unop(not, IsNullish(x))`.
 *
 * **Nullability gate.** A leaf only folds when the matched operand's
 * TS type at the AST location actually includes `null`, `undefined`,
 * or `void`. This guards the list-lift encoding (`T | null → [T]`):
 * if `x: number` is non-nullable, `#x = 0` would be ill-typed
 * (cardinality applies to lists, not scalars). Non-nullable
 * occurrences fall through and the surrounding TS-AST handler
 * either rejects or emits the legacy shape. See
 * `tools/ts2pant/CLAUDE.md` § "Option-Type Elimination" for the
 * encoding rationale.
 *
 * **`undefined` shadowing.** `undefined` is not a reserved word in
 * JavaScript — a parameter, local, or import named `undefined` is
 * a legal binding that shadows the global. Identifier-only matching
 * would fold `x === undefined` even when `undefined` resolves to a
 * shadowed local of unrelated type. The leaf recognizer guards by
 * asking the checker for the resolved type at the identifier's
 * location and only matching when that type's flags include
 * `Undefined`.
 *
 * **Long-form recognizer is flat-aware.** For a chain
 * `a || b || c || …`, all `||` operands at the same level are
 * collected into a flat list. The recognizer scans for two operands
 * that are nullish-shaped against the *same operand expression*
 * (structurally — see `ast-equal.ts`), folds the pair into one
 * `IsNullish` node, and rebuilds the disjunction with the remaining
 * clauses around the fold. Same for `&&` chains using the negated
 * form. This means
 * `(a === null) || (a === undefined) || other`
 * recognizes as `IsNullish(a) || other` rather than forcing the
 * programmer to rewrite their TS to fit a strict pattern.
 *
 * **Operand mismatch falls through.** If no two leaves in a
 * chain share an operand structurally, the long-form recognizer
 * returns `null` — the surrounding `||`/`&&` flows through the
 * normal Bool-typed short-circuit path. Each leaf, on its own
 * recursive translation, is still a leaf-level nullish form and
 * recognized individually.
 */

import ts from "typescript";
import { structurallyEqualExpression } from "./ast-equal.js";
import { type IR1Expr, ir1Binop, ir1IsNullish, ir1Unop } from "./ir1.js";

/**
 * True when the TS type at this AST location includes `null`,
 * `undefined`, or `void` — i.e., when the operand is list-lifted to
 * `[T]` on the Pant side and `#operand = 0` is a well-typed nullish
 * test. Mirrors `isNullableTsType` in `translate-body.ts`; defined
 * locally to keep this module dependency-light.
 */
function isNullableType(type: ts.Type): boolean {
  const mask = ts.TypeFlags.Null | ts.TypeFlags.Undefined | ts.TypeFlags.Void;
  if (type.isUnion()) {
    return type.types.some((t) => (t.flags & mask) !== 0);
  }
  return (type.flags & mask) !== 0;
}

function isOperandNullable(
  operand: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  return isNullableType(checker.getTypeAtLocation(operand));
}

/**
 * A nullish leaf match — one of the short-form binary expressions
 * (`x == null`, `x === undefined`, `typeof x === 'undefined'`, etc.).
 * `negated` is `true` for `!=` / `!==` forms.
 */
export interface NullishLeafMatch {
  operand: ts.Expression;
  negated: boolean;
}

function unwrapParens(e: ts.Expression): ts.Expression {
  let cur = e;
  while (ts.isParenthesizedExpression(cur)) {
    cur = cur.expression;
  }
  return cur;
}

function isNullKeyword(e: ts.Expression): boolean {
  return unwrapParens(e).kind === ts.SyntaxKind.NullKeyword;
}

/**
 * True when `e` is a reference to the global `undefined` value (not a
 * shadowed local with that name). Verifies via the checker because
 * `undefined` is not a reserved word — a parameter, local variable,
 * or import named `undefined` is a legal binding that hides the
 * global. The resolved type's flags must include `Undefined`; a
 * shadowed `undefined: number` resolves to `number` (no Undefined
 * flag) and is rejected.
 */
function isUndefinedIdentifier(
  e: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  const u = unwrapParens(e);
  if (!ts.isIdentifier(u) || u.text !== "undefined") {
    return false;
  }
  const type = checker.getTypeAtLocation(u);
  return (type.flags & ts.TypeFlags.Undefined) !== 0;
}

function isUndefinedStringLiteral(e: ts.Expression): boolean {
  const u = unwrapParens(e);
  return ts.isStringLiteral(u) && u.text === "undefined";
}

function isTypeofExpression(e: ts.Expression): ts.Expression | null {
  const u = unwrapParens(e);
  if (ts.isTypeOfExpression(u)) {
    return u.expression;
  }
  return null;
}

/**
 * Recognize the leaf nullish-equality forms:
 *   - `x == null` / `x != null`
 *   - `x === null` / `x !== null`
 *   - `x === undefined` / `x !== undefined`
 *
 * The `null`/`undefined` literal can appear on either side; the
 * other side is `operand`. Loose equality (`==`/`!=`) accepts only
 * the `null` literal — `x == undefined` is unusual idiomatically and
 * is not part of the recognized leaf shapes.
 *
 * The match also requires that `operand` has a nullable TS type at
 * the AST location (includes `null`, `undefined`, or `void`). A
 * non-nullable operand like `x: number` doesn't list-lift to `[T]`,
 * so `#x = 0` would be ill-typed in Pant. Mismatched cases fall
 * through (return `null`) and the surrounding handler decides what
 * to do — typically reject as unsupported.
 *
 * Returns `null` if the binary expression is not a nullish-equality
 * leaf (e.g., `x === 0`, `x + null`, `==` against a non-null literal,
 * or operand whose TS type isn't nullable).
 */
export function recognizeNullishLeaf(
  expr: ts.BinaryExpression,
  checker: ts.TypeChecker,
): NullishLeafMatch | null {
  let negated: boolean;
  let strict: boolean;
  switch (expr.operatorToken.kind) {
    case ts.SyntaxKind.EqualsEqualsToken:
      negated = false;
      strict = false;
      break;
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
      negated = false;
      strict = true;
      break;
    case ts.SyntaxKind.ExclamationEqualsToken:
      negated = true;
      strict = false;
      break;
    case ts.SyntaxKind.ExclamationEqualsEqualsToken:
      negated = true;
      strict = true;
      break;
    default:
      return null;
  }

  const matchIfNullable = (operand: ts.Expression): NullishLeafMatch | null =>
    isOperandNullable(operand, checker) ? { operand, negated } : null;

  const leftIsNull = isNullKeyword(expr.left);
  const rightIsNull = isNullKeyword(expr.right);
  if (rightIsNull) {
    return matchIfNullable(expr.left);
  }
  if (leftIsNull) {
    return matchIfNullable(expr.right);
  }
  if (strict) {
    const leftIsUndef = isUndefinedIdentifier(expr.left, checker);
    const rightIsUndef = isUndefinedIdentifier(expr.right, checker);
    if (rightIsUndef) {
      return matchIfNullable(expr.left);
    }
    if (leftIsUndef) {
      return matchIfNullable(expr.right);
    }
  }
  return null;
}

/**
 * Recognize the `typeof x === 'undefined'` / `typeof x !== 'undefined'`
 * family. The `typeof` operand can appear on either side; the other
 * side is the string literal `'undefined'`. Loose equality (`==`/`!=`)
 * is also accepted because the legal TS coercion semantics for
 * `typeof x` are well-defined.
 *
 * Like `recognizeNullishLeaf`, requires the operand inside `typeof`
 * to have a nullable TS type — otherwise `#x = 0` would be ill-typed
 * even though the surface syntax `typeof x === 'undefined'` is
 * always-false-but-legal in TypeScript.
 *
 * Returns `null` if the binary expression is not a typeof-undefined
 * leaf or the operand isn't nullable.
 */
export function recognizeTypeofUndefined(
  expr: ts.BinaryExpression,
  checker: ts.TypeChecker,
): NullishLeafMatch | null {
  let negated: boolean;
  switch (expr.operatorToken.kind) {
    case ts.SyntaxKind.EqualsEqualsToken:
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
      negated = false;
      break;
    case ts.SyntaxKind.ExclamationEqualsToken:
    case ts.SyntaxKind.ExclamationEqualsEqualsToken:
      negated = true;
      break;
    default:
      return null;
  }

  const leftTypeofOperand = isTypeofExpression(expr.left);
  const rightTypeofOperand = isTypeofExpression(expr.right);

  const matchIfNullable = (operand: ts.Expression): NullishLeafMatch | null =>
    isOperandNullable(operand, checker) ? { operand, negated } : null;

  if (leftTypeofOperand !== null && isUndefinedStringLiteral(expr.right)) {
    return matchIfNullable(leftTypeofOperand);
  }
  if (rightTypeofOperand !== null && isUndefinedStringLiteral(expr.left)) {
    return matchIfNullable(rightTypeofOperand);
  }
  return null;
}

/**
 * Combined leaf matcher: tries `recognizeNullishLeaf` first, then
 * `recognizeTypeofUndefined`. Returns `null` if neither matches.
 */
function recognizeAnyLeaf(
  expr: ts.BinaryExpression,
  checker: ts.TypeChecker,
): NullishLeafMatch | null {
  return (
    recognizeNullishLeaf(expr, checker) ??
    recognizeTypeofUndefined(expr, checker)
  );
}

/**
 * Translation callback shape expected by the nullish recognizer. The
 * recognizer drives translation of operands (the `x` in `x == null`)
 * and any non-nullish residue from a long-form chain. The caller
 * threads the appropriate `translateBodyExpr` / `translateExpr` into
 * this callback.
 *
 * Returning `{ unsupported }` propagates the rejection up through the
 * recognizer's result, so the caller can surface the same reason as
 * if the recognizer had not fired.
 */
export type NullishTranslate = (
  expr: ts.Expression,
) => IR1Expr | { unsupported: string };

const isUnsupported = (
  r: IR1Expr | { unsupported: string },
): r is { unsupported: string } => "unsupported" in r;

/**
 * Build an L1 expression for a single recognized leaf — `IsNullish(x)`,
 * wrapped in `unop(not, …)` if negated.
 */
function buildLeafL1(
  leaf: NullishLeafMatch,
  translate: NullishTranslate,
): IR1Expr | { unsupported: string } {
  const operandL1 = translate(leaf.operand);
  if (isUnsupported(operandL1)) {
    return operandL1;
  }
  const isNull = ir1IsNullish(operandL1);
  return leaf.negated ? ir1Unop("not", isNull) : isNull;
}

/**
 * Flatten a chain of binary expressions joined by `targetOp` into the
 * list of leaf operands. Parenthesized wrappers around a chain
 * connector are unwrapped so nesting structure doesn't hide a
 * fold-eligible pair.
 */
function flattenChain(
  expr: ts.Expression,
  targetOp: ts.SyntaxKind,
  out: ts.Expression[],
): void {
  const u = unwrapParens(expr);
  if (ts.isBinaryExpression(u) && u.operatorToken.kind === targetOp) {
    flattenChain(u.left, targetOp, out);
    flattenChain(u.right, targetOp, out);
    return;
  }
  out.push(expr);
}

/**
 * Recognize a long-form chain of `||` (positive) or `&&` (negated).
 * Looks for two operands that are nullish-shaped against the same
 * operand expression and folds them into one `IsNullish` node.
 *
 * Returns `null` if no fold-eligible pair was found — the caller
 * falls through to the normal short-circuit translation path. If a
 * pair is found, the result is the chain rebuilt with the fold in
 * place of the two consumed operands and any remaining clauses
 * translated through the callback.
 */
function recognizeLongForm(
  expr: ts.BinaryExpression,
  combinator: "or" | "and",
  checker: ts.TypeChecker,
  translate: NullishTranslate,
): IR1Expr | { unsupported: string } | null {
  const targetOp =
    combinator === "or"
      ? ts.SyntaxKind.BarBarToken
      : ts.SyntaxKind.AmpersandAmpersandToken;
  const targetNegated = combinator === "and";

  const operands: ts.Expression[] = [];
  flattenChain(expr.left, targetOp, operands);
  flattenChain(expr.right, targetOp, operands);

  // Classify each operand: is it a nullish leaf with negated matching
  // the chain's polarity? If so, remember its operand expression for
  // pair-matching against the same operand.
  const leafOperands: Array<ts.Expression | null> = operands.map((op) => {
    const u = unwrapParens(op);
    if (!ts.isBinaryExpression(u)) {
      return null;
    }
    const leaf = recognizeAnyLeaf(u, checker);
    if (leaf === null || leaf.negated !== targetNegated) {
      return null;
    }
    return leaf.operand;
  });

  // Greedy pairing: walk left-to-right, pair each unconsumed nullish
  // leaf with the next unconsumed nullish leaf that shares an operand.
  const consumed = new Set<number>();
  type Fold = {
    firstIndex: number;
    secondIndex: number;
    operand: ts.Expression;
  };
  const folds: Fold[] = [];
  for (let i = 0; i < leafOperands.length; i++) {
    if (consumed.has(i)) {
      continue;
    }
    const opA = leafOperands[i];
    if (opA === null || opA === undefined) {
      continue;
    }
    for (let j = i + 1; j < leafOperands.length; j++) {
      if (consumed.has(j)) {
        continue;
      }
      const opB = leafOperands[j];
      if (opB === null || opB === undefined) {
        continue;
      }
      if (structurallyEqualExpression(opA, opB)) {
        consumed.add(i);
        consumed.add(j);
        folds.push({ firstIndex: i, secondIndex: j, operand: opA });
        break;
      }
    }
  }

  if (folds.length === 0) {
    return null;
  }

  const skipIndices = new Set<number>(folds.map((f) => f.secondIndex));
  const foldByFirst = new Map<number, ts.Expression>(
    folds.map((f) => [f.firstIndex, f.operand]),
  );

  const items: IR1Expr[] = [];
  for (let i = 0; i < operands.length; i++) {
    if (skipIndices.has(i)) {
      continue;
    }
    const foldedOperand = foldByFirst.get(i);
    if (foldedOperand !== undefined) {
      const operandL1 = translate(foldedOperand);
      if (isUnsupported(operandL1)) {
        return operandL1;
      }
      const isNull = ir1IsNullish(operandL1);
      items.push(targetNegated ? ir1Unop("not", isNull) : isNull);
    } else {
      const otherL1 = translate(operands[i] as ts.Expression);
      if (isUnsupported(otherL1)) {
        return otherL1;
      }
      items.push(otherL1);
    }
  }

  if (items.length === 0) {
    // Should be unreachable — a fold consumed two leaves but produced
    // no items, meaning the chain had length < 2. Defense in depth.
    return null;
  }
  let result = items[0] as IR1Expr;
  for (let i = 1; i < items.length; i++) {
    result = ir1Binop(combinator, result, items[i] as IR1Expr);
  }
  return result;
}

/**
 * Top-level recognizer. Tries leaf forms first (`x == null`,
 * `typeof x === 'undefined'`, etc.), then long-form chains
 * (`x === null || x === undefined`).
 *
 * The checker is required for the nullability gate (see module
 * doc) and for verifying that an `undefined` identifier resolves
 * to the global value rather than a shadowed local.
 *
 * Returns:
 * - An `IR1Expr` if recognized — the caller should lower it and use
 *   the result instead of the legacy translation path.
 * - `{ unsupported }` if recognized but the operand failed to
 *   translate — the caller should surface the same rejection.
 * - `null` if not recognized — the caller should fall through to its
 *   normal binary-expression handling.
 */
export function recognizeNullishForm(
  expr: ts.BinaryExpression,
  checker: ts.TypeChecker,
  translate: NullishTranslate,
): IR1Expr | { unsupported: string } | null {
  const leaf = recognizeAnyLeaf(expr, checker);
  if (leaf !== null) {
    return buildLeafL1(leaf, translate);
  }

  if (expr.operatorToken.kind === ts.SyntaxKind.BarBarToken) {
    return recognizeLongForm(expr, "or", checker, translate);
  }
  if (expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken) {
    return recognizeLongForm(expr, "and", checker, translate);
  }
  return null;
}
