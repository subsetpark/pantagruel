import ts from "typescript";
import type { Fact } from "./assumption-env.js";
import { getAst } from "./pant-wasm.js";
import { isStaticallyBoolTyped } from "./purity.js";
import {
  isTranslateExprUnsupported,
  translateExpr,
} from "./translate-signature.js";
import { IntStrategy } from "./translate-types.js";

type LiteralValue = string;

function unwrapParens(expr: ts.Expression): ts.Expression {
  let cur = expr;
  while (ts.isParenthesizedExpression(cur)) {
    cur = cur.expression;
  }
  return cur;
}

function literalValue(expr: ts.Expression): LiteralValue | null {
  const unwrapped = unwrapParens(expr);
  if (
    ts.isStringLiteral(unwrapped) ||
    ts.isNoSubstitutionTemplateLiteral(unwrapped)
  ) {
    return unwrapped.text;
  }
  if (ts.isNumericLiteral(unwrapped)) {
    return unwrapped.text;
  }
  if (unwrapped.kind === ts.SyntaxKind.TrueKeyword) {
    return "true";
  }
  if (unwrapped.kind === ts.SyntaxKind.FalseKeyword) {
    return "false";
  }
  return null;
}

function propertyAccessParts(expr: ts.Expression): {
  receiver: string;
  property: string;
} | null {
  const unwrapped = unwrapParens(expr);
  if (ts.isPropertyAccessExpression(unwrapped)) {
    return {
      receiver: unwrapped.expression.getText(),
      property: unwrapped.name.text,
    };
  }
  return null;
}

function discriminantFactFromParts(
  access: ts.Expression,
  literal: ts.Expression,
): Fact | null {
  const parts = propertyAccessParts(access);
  const value = literalValue(literal);
  if (!parts || value === null) {
    return null;
  }
  return {
    kind: "discriminant",
    receiver: parts.receiver,
    property: parts.property,
    literal: value,
  };
}

function discriminantFactFromEqualityOperands(
  expr: ts.BinaryExpression,
): Fact | null {
  return (
    discriminantFactFromParts(expr.left, expr.right) ??
    discriminantFactFromParts(expr.right, expr.left)
  );
}

function discriminantFactFromEquality(expr: ts.BinaryExpression): Fact | null {
  switch (expr.operatorToken.kind) {
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
      return discriminantFactFromEqualityOperands(expr);
    case ts.SyntaxKind.ExclamationEqualsEqualsToken: {
      const fact = discriminantFactFromEqualityOperands(expr);
      return fact === null ? null : negateFact(fact);
    }
    case ts.SyntaxKind.ExclamationEqualsToken: {
      const fact = discriminantFactFromEqualityOperands(expr);
      return fact === null ? null : negateFact(fact);
    }
    default:
      return null;
  }
}

/** Recognize a TypeScript boolean test as a narrowing fact. */
export function recognizeNarrowingPredicate(
  test: ts.Expression,
  checker: ts.TypeChecker,
): Fact | null {
  if (!isStaticallyBoolTyped(test, checker)) {
    return null;
  }

  const unwrapped = unwrapParens(test);
  if (ts.isBinaryExpression(unwrapped)) {
    const fact = discriminantFactFromEquality(unwrapped);
    if (fact !== null) {
      return fact;
    }
  }

  const translated = translateExpr(
    test,
    checker,
    IntStrategy,
    new Map<string, string>(),
  );
  if (isTranslateExprUnsupported(translated)) {
    return null;
  }
  return {
    kind: "predicate",
    testExpr: translated,
  };
}

export function recognizeNarrowingFromSwitchCase(
  switchTest: ts.Expression,
  caseLabel: ts.Expression,
): Fact | null {
  return discriminantFactFromParts(switchTest, caseLabel);
}

export function negateFact(fact: Fact): Fact {
  if (fact.kind === "discriminant") {
    const negated = /^!\((.*)\)$/u.exec(fact.literal);
    return {
      ...fact,
      literal: negated === null ? `!(${fact.literal})` : negated[1]!,
    };
  }
  const ast = getAst();
  return {
    kind: "predicate",
    testExpr: ast.unop(ast.opNot(), fact.testExpr),
  };
}
