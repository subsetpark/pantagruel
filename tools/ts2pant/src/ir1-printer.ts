import type { IR1Binop, IR1Expr, IR1Literal, IR1Unop } from "./ir1.js";

export function formatIR1Expr(expr: IR1Expr): string {
  switch (expr.kind) {
    case "var":
      return `${expr.name}${expr.primed === true ? "'" : ""}`;
    case "lit":
      return formatIR1Literal(expr.value);
    case "binop":
      return `(${formatIR1Expr(expr.lhs)} ${binopGlyph(expr.op)} ${formatIR1Expr(expr.rhs)})`;
    case "unop":
      return formatUnop(expr.op, expr.arg);
    case "app": {
      const callee = formatAsAppCallee(expr.callee);
      const args = expr.args.map(formatAsAppArg);
      return [callee, ...args].join(" ");
    }
    case "member":
      return `${expr.name} ${formatAsMemberReceiver(expr.receiver)}`;
    case "cond": {
      const arms = expr.arms.map(
        ([guard, value]) =>
          `${formatIR1Expr(guard)} => ${formatIR1Expr(value)}`,
      );
      arms.push(`true => ${formatIR1Expr(expr.otherwise)}`);
      return `cond ${arms.join(", ")}`;
    }
    case "is-nullish":
      return `nullish? ${formatAsAppArg(expr.operand)}`;
    case "each": {
      const guards = expr.guards.map(
        (guard) => `, when ${formatIR1Expr(guard)}`,
      );
      return `each ${expr.binder} in ${formatIR1Expr(expr.src)} | ${formatIR1Expr(expr.proj)}${guards.join("")}`;
    }
    case "map-read": {
      const ruleName = expr.op === "get" ? expr.ruleName : expr.keyPredName;
      return `${ruleName} ${formatAsAppArg(expr.receiver)} ${formatAsAppArg(expr.key)}`;
    }
    case "set-read":
      return `${formatIR1Expr(expr.elem)} in ${formatIR1Expr(expr.receiver)}`;
    default: {
      const _exhaustive: never = expr;
      throw new Error(
        `Unhandled IR1 expression kind: ${JSON.stringify(_exhaustive)}`,
      );
    }
  }
}

function formatIR1Literal(literal: IR1Literal): string {
  switch (literal.kind) {
    case "nat":
      return String(literal.value);
    case "bool":
      return literal.value ? "true" : "false";
    case "string":
      return JSON.stringify(literal.value);
    default: {
      const _exhaustive: never = literal;
      throw new Error(
        `Unhandled IR1 literal kind: ${JSON.stringify(_exhaustive)}`,
      );
    }
  }
}

function formatUnop(op: IR1Unop, arg: IR1Expr): string {
  switch (op) {
    case "not":
      return `${unopGlyph(op)}${formatAsTightPrefixArg(arg)}`;
    case "neg":
      return `(${unopGlyph(op)}${formatIR1Expr(arg)})`;
    case "card":
      return `${unopGlyph(op)}${formatAsTightPrefixArg(arg)}`;
    default: {
      const _exhaustive: never = op;
      throw new Error(
        `Unhandled IR1 unary operator: ${JSON.stringify(_exhaustive)}`,
      );
    }
  }
}

function binopGlyph(op: IR1Binop): string {
  switch (op) {
    case "and":
      return "and";
    case "or":
      return "or";
    case "impl":
      return "->";
    case "iff":
      return "<->";
    case "eq":
      return "=";
    case "neq":
      return "~=";
    case "lt":
      return "<";
    case "gt":
      return ">";
    case "le":
      return "<=";
    case "ge":
      return ">=";
    case "in":
      return "in";
    case "subset":
      return "subset";
    case "add":
      return "+";
    case "sub":
      return "-";
    case "mul":
      return "*";
    case "div":
      return "/";
    default: {
      const _exhaustive: never = op;
      throw new Error(
        `Unhandled IR1 binary operator: ${JSON.stringify(_exhaustive)}`,
      );
    }
  }
}

function unopGlyph(op: IR1Unop): string {
  switch (op) {
    case "not":
      return "~";
    case "neg":
      return "-";
    case "card":
      return "#";
    default: {
      const _exhaustive: never = op;
      throw new Error(
        `Unhandled IR1 unary operator: ${JSON.stringify(_exhaustive)}`,
      );
    }
  }
}

function formatAsAppCallee(expr: IR1Expr): string {
  return needsCalleeParens(expr)
    ? `(${formatIR1Expr(expr)})`
    : formatIR1Expr(expr);
}

function formatAsAppArg(expr: IR1Expr): string {
  return needsAppArgParens(expr)
    ? `(${formatIR1Expr(expr)})`
    : formatIR1Expr(expr);
}

function formatAsMemberReceiver(expr: IR1Expr): string {
  return needsMemberReceiverParens(expr)
    ? `(${formatIR1Expr(expr)})`
    : formatIR1Expr(expr);
}

function formatAsTightPrefixArg(expr: IR1Expr): string {
  return expr.kind === "binop" || expr.kind === "cond"
    ? formatIR1Expr(expr)
    : formatAsAppArg(expr);
}

function needsCalleeParens(expr: IR1Expr): boolean {
  return expr.kind === "binop" || expr.kind === "cond";
}

function needsAppArgParens(expr: IR1Expr): boolean {
  return expr.kind === "app" || expr.kind === "cond";
}

function needsMemberReceiverParens(expr: IR1Expr): boolean {
  return expr.kind === "binop" || expr.kind === "member";
}
