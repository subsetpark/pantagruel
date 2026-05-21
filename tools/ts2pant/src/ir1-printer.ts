import type {
  IR1Binop,
  IR1Expr,
  IR1Literal,
  IR1SsaLocation,
  IR1SsaProgram,
  IR1SsaValue,
  IR1SsaVersion,
  IR1Unop,
} from "./ir1.js";

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

export function formatIR1SsaLocation(loc: IR1SsaLocation): string {
  switch (loc.kind) {
    case "property":
      return `${loc.ruleName} ${formatAsAppArg(loc.receiver)}`;
    case "map-value":
      return `${loc.keyPredName} ${formatAsAppArg(loc.receiver)} ${formatAsAppArg(loc.key)}`;
    case "map-membership":
      return `${formatIR1Expr(loc.key)} in ${formatIR1Expr(loc.receiver)}`;
    case "set-membership":
      return formatIR1Expr(loc.receiver);
    default: {
      const _exhaustive: never = loc;
      throw new Error(
        `Unhandled IR1 SSA location kind: ${String(_exhaustive)}`,
      );
    }
  }
}

export function formatIR1SsaProgram(program: IR1SsaProgram): string {
  const labeller = new VersionLabeller();
  const lines: string[] = [
    `> rules: declared = ${formatRuleList(program.declaredRules)}; modified = ${formatRuleList(program.modifiedRules)}; framed = ${formatRuleList(program.framedRules)}`,
    "",
  ];
  const emittedInitials = new Set<symbol>();
  const currentVersions = new Map<string, IR1SsaVersion>();

  for (const read of program.reads) {
    if (read.version.origin !== "initial") {
      continue;
    }
    currentVersions.set(locationKey(read.location), read.version);
    if (emittedInitials.has(read.version.id)) {
      continue;
    }
    emittedInitials.add(read.version.id);
    lines.push(
      `${labeller.labelOf(read.version)} = ${formatIR1SsaLocation(read.location)}.    > initial`,
    );
  }

  for (const write of program.writes) {
    const key = locationKey(write.location);
    const priorVersion = currentVersions.get(key) ?? null;
    const readLabels = readExpressionLabels(currentVersions.values(), labeller);
    lines.push(
      `${labeller.labelOf(write.version)} = ${formatSsaWriteRhs(
        write.value,
        write.location,
        priorVersion === null ? null : labeller.labelOf(priorVersion),
        readLabels,
      )}.    > ${locationAsPrimedRule(write.location)}`,
    );
    currentVersions.set(key, write.version);
  }

  for (const join of program.joins) {
    lines.push(
      `${labeller.labelOf(join.joinVersion)} = phi ${labeller.labelOf(
        join.thenVersion,
      )} ${labeller.labelOf(join.elseVersion)}.    > join ${formatIR1SsaLocation(join.location)}`,
    );
    currentVersions.set(locationKey(join.location), join.joinVersion);
  }

  for (const summary of program.loopSummaries) {
    lines.push(
      `${labeller.labelOf(summary.summaryVersion)} = summarize-${summary.shape} ${formatIR1SsaLocation(summary.location)}.    > loop`,
    );
    currentVersions.set(locationKey(summary.location), summary.summaryVersion);
  }

  return lines.join("\n");
}

class VersionLabeller {
  private readonly labels = new Map<symbol, number>();
  private next = 1;

  labelOf(version: IR1SsaVersion): string {
    const existing = this.labels.get(version.id);
    if (existing !== undefined) {
      return `v${existing}`;
    }
    const label = this.next;
    this.next += 1;
    this.labels.set(version.id, label);
    return `v${label}`;
  }
}

function formatRuleList(rules: readonly string[]): string {
  return `[${rules.join(", ")}]`;
}

function locationKey(loc: IR1SsaLocation): string {
  return `${loc.kind}:${formatIR1SsaLocation(loc)}`;
}

function formatSsaWriteRhs(
  value: IR1SsaValue,
  location: IR1SsaLocation,
  priorVersionLabel: string | null,
  readLabels: ReadonlyMap<string, string>,
): string {
  switch (value.kind) {
    case "property":
      return formatIR1ExprWithSsaReads(value.value, readLabels);
    case "map-value":
      return formatIR1ExprWithSsaReads(value.value, readLabels);
    case "map-membership":
      if (value.op === "set") {
        return "true";
      }
      return "false";
    case "set-membership":
      switch (value.op) {
        case "add":
          return `${formatPriorSetVersion(location, priorVersionLabel)} with ${formatIR1ExprWithSsaReads(value.elem, readLabels)}`;
        case "delete":
          return `${formatPriorSetVersion(location, priorVersionLabel)} without ${formatIR1ExprWithSsaReads(value.elem, readLabels)}`;
        case "clear":
          return "empty";
        default: {
          const _exhaustive: never = value;
          throw new Error(
            `Unhandled IR1 SSA set membership value: ${String(_exhaustive)}`,
          );
        }
      }
    default: {
      const _exhaustive: never = value;
      throw new Error(`Unhandled IR1 SSA value kind: ${String(_exhaustive)}`);
    }
  }
}

function readExpressionLabels(
  versions: Iterable<IR1SsaVersion>,
  labeller: VersionLabeller,
): Map<string, string> {
  const labels = new Map<string, string>();
  for (const version of versions) {
    labels.set(readExpressionKey(version.location), labeller.labelOf(version));
  }
  return labels;
}

function readExpressionKey(loc: IR1SsaLocation): string {
  switch (loc.kind) {
    case "property":
      return `property:${loc.ruleName} ${formatAsAppArg(loc.receiver)}`;
    case "map-value":
      return `map-value:${loc.ruleName} ${formatAsAppArg(loc.receiver)} ${formatAsAppArg(loc.key)}`;
    case "map-membership":
      return `map-membership:${loc.keyPredName} ${formatAsAppArg(loc.receiver)} ${formatAsAppArg(loc.key)}`;
    case "set-membership":
      return `set-membership:${formatIR1Expr(loc.receiver)}`;
    default: {
      const _exhaustive: never = loc;
      throw new Error(
        `Unhandled IR1 SSA location kind: ${String(_exhaustive)}`,
      );
    }
  }
}

function formatIR1ExprWithSsaReads(
  expr: IR1Expr,
  readLabels: ReadonlyMap<string, string>,
): string {
  switch (expr.kind) {
    case "var":
      return `${expr.name}${expr.primed === true ? "'" : ""}`;
    case "lit":
      return formatIR1Literal(expr.value);
    case "binop":
      return `(${formatIR1ExprWithSsaReads(expr.lhs, readLabels)} ${binopGlyph(expr.op)} ${formatIR1ExprWithSsaReads(expr.rhs, readLabels)})`;
    case "unop":
      return formatUnopWithSsaReads(expr.op, expr.arg, readLabels);
    case "app": {
      const callee = formatVersionedAsAppCallee(expr.callee, readLabels);
      const args = expr.args.map((arg) =>
        formatVersionedAsAppArg(arg, readLabels),
      );
      return [callee, ...args].join(" ");
    }
    case "member": {
      const label = readLabels.get(
        `property:${expr.name} ${formatAsAppArg(expr.receiver)}`,
      );
      if (label !== undefined) {
        return label;
      }
      return `${expr.name} ${formatVersionedAsMemberReceiver(
        expr.receiver,
        readLabels,
      )}`;
    }
    case "cond": {
      const arms = expr.arms.map(
        ([guard, value]) =>
          `${formatIR1ExprWithSsaReads(guard, readLabels)} => ${formatIR1ExprWithSsaReads(value, readLabels)}`,
      );
      arms.push(
        `true => ${formatIR1ExprWithSsaReads(expr.otherwise, readLabels)}`,
      );
      return `cond ${arms.join(", ")}`;
    }
    case "is-nullish":
      return `nullish? ${formatVersionedAsAppArg(expr.operand, readLabels)}`;
    case "each": {
      const guards = expr.guards.map(
        (guard) => `, when ${formatIR1ExprWithSsaReads(guard, readLabels)}`,
      );
      return `each ${expr.binder} in ${formatIR1ExprWithSsaReads(
        expr.src,
        readLabels,
      )} | ${formatIR1ExprWithSsaReads(expr.proj, readLabels)}${guards.join("")}`;
    }
    case "map-read": {
      const keyPrefix =
        expr.op === "get"
          ? `map-value:${expr.ruleName}`
          : `map-membership:${expr.keyPredName}`;
      const label = readLabels.get(
        `${keyPrefix} ${formatAsAppArg(expr.receiver)} ${formatAsAppArg(expr.key)}`,
      );
      if (label !== undefined) {
        return label;
      }
      const ruleName = expr.op === "get" ? expr.ruleName : expr.keyPredName;
      return `${ruleName} ${formatVersionedAsAppArg(
        expr.receiver,
        readLabels,
      )} ${formatVersionedAsAppArg(expr.key, readLabels)}`;
    }
    case "set-read": {
      const receiverLabel = readLabels.get(
        `set-membership:${formatIR1Expr(expr.receiver)}`,
      );
      return `${formatIR1ExprWithSsaReads(expr.elem, readLabels)} in ${
        receiverLabel ?? formatIR1ExprWithSsaReads(expr.receiver, readLabels)
      }`;
    }
    default: {
      const _exhaustive: never = expr;
      throw new Error(`Unhandled IR1 expression kind: ${String(_exhaustive)}`);
    }
  }
}

function formatUnopWithSsaReads(
  op: IR1Unop,
  arg: IR1Expr,
  readLabels: ReadonlyMap<string, string>,
): string {
  switch (op) {
    case "not":
      return `${unopGlyph(op)}${formatVersionedAsTightPrefixArg(arg, readLabels)}`;
    case "neg":
      return `(${unopGlyph(op)}${formatIR1ExprWithSsaReads(arg, readLabels)})`;
    case "card":
      return `${unopGlyph(op)}${formatVersionedAsTightPrefixArg(arg, readLabels)}`;
    default: {
      const _exhaustive: never = op;
      throw new Error(`Unhandled IR1 unary operator: ${String(_exhaustive)}`);
    }
  }
}

function formatPriorSetVersion(
  location: IR1SsaLocation,
  priorVersionLabel: string | null,
): string {
  return priorVersionLabel ?? formatIR1SsaLocation(location);
}

function locationAsPrimedRule(loc: IR1SsaLocation): string {
  switch (loc.kind) {
    case "property":
      return `${loc.ruleName}' ${formatAsAppArg(loc.receiver)}`;
    case "map-value":
      return `${loc.keyPredName}' ${formatAsAppArg(loc.receiver)} ${formatAsAppArg(loc.key)}`;
    case "map-membership":
      return `(${formatIR1Expr(loc.key)} in ${formatIR1Expr(loc.receiver)})'`;
    case "set-membership":
      return `${formatIR1Expr(loc.receiver)}'`;
    default: {
      const _exhaustive: never = loc;
      throw new Error(
        `Unhandled IR1 SSA location kind: ${String(_exhaustive)}`,
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

function formatVersionedAsAppCallee(
  expr: IR1Expr,
  readLabels: ReadonlyMap<string, string>,
): string {
  return needsCalleeParens(expr)
    ? `(${formatIR1ExprWithSsaReads(expr, readLabels)})`
    : formatIR1ExprWithSsaReads(expr, readLabels);
}

function formatVersionedAsAppArg(
  expr: IR1Expr,
  readLabels: ReadonlyMap<string, string>,
): string {
  return needsAppArgParens(expr)
    ? `(${formatIR1ExprWithSsaReads(expr, readLabels)})`
    : formatIR1ExprWithSsaReads(expr, readLabels);
}

function formatVersionedAsMemberReceiver(
  expr: IR1Expr,
  readLabels: ReadonlyMap<string, string>,
): string {
  return needsMemberReceiverParens(expr)
    ? `(${formatIR1ExprWithSsaReads(expr, readLabels)})`
    : formatIR1ExprWithSsaReads(expr, readLabels);
}

function formatVersionedAsTightPrefixArg(
  expr: IR1Expr,
  readLabels: ReadonlyMap<string, string>,
): string {
  return expr.kind === "binop" || expr.kind === "cond"
    ? formatIR1ExprWithSsaReads(expr, readLabels)
    : formatVersionedAsAppArg(expr, readLabels);
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
