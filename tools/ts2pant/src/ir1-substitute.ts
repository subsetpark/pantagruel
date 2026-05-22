import {
  type IR1Expr,
  type IR1FoldLeaf,
  type IR1SsaLocation,
  type IR1SsaLoopBody,
  type IR1SsaValue,
  type IR1Stmt,
  ir1App,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1CombTyped,
  ir1Cond,
  ir1CondStmt,
  ir1Each,
  ir1Exists,
  ir1ExprStmt,
  ir1For,
  ir1Forall,
  ir1Foreach,
  ir1IsNullish,
  ir1Member,
  ir1Return,
  ir1Throw,
  ir1Unop,
  ir1While,
} from "./ir1.js";

type SupportedNeedle = Extract<IR1Expr, { kind: "var" | "member" }>;

export class CaptureRiskError extends Error {
  binderName: string;
  capturedVar: string;

  constructor(binderName: string, capturedVar: string) {
    super(
      `IR1 substitution would capture free var '${capturedVar}' under binder '${binderName}'`,
    );
    this.name = "CaptureRiskError";
    this.binderName = binderName;
    this.capturedVar = capturedVar;
  }
}

export function freeVarsIR1Expr(expr: IR1Expr): Set<string> {
  switch (expr.kind) {
    case "var":
      return new Set([expr.name]);
    case "lit":
      return new Set();
    case "binop":
      return union(freeVarsIR1Expr(expr.lhs), freeVarsIR1Expr(expr.rhs));
    case "unop":
      return freeVarsIR1Expr(expr.arg);
    case "app":
      return union(
        freeVarsIR1Expr(expr.callee),
        ...expr.args.map(freeVarsIR1Expr),
      );
    case "member":
      return freeVarsIR1Expr(expr.receiver);
    case "cond":
      return union(
        ...expr.arms.flatMap(([guard, value]) => [
          freeVarsIR1Expr(guard),
          freeVarsIR1Expr(value),
        ]),
        freeVarsIR1Expr(expr.otherwise),
      );
    case "is-nullish":
      return freeVarsIR1Expr(expr.operand);
    case "each": {
      const scoped = union(
        ...expr.guards.map(freeVarsIR1Expr),
        freeVarsIR1Expr(expr.proj),
      );
      scoped.delete(expr.binder);
      return union(freeVarsIR1Expr(expr.src), scoped);
    }
    case "comb-typed": {
      const scoped = union(
        ...expr.guards.map(freeVarsIR1Expr),
        freeVarsIR1Expr(expr.proj),
      );
      scoped.delete(expr.binder);
      return scoped;
    }
    case "forall":
    case "exists": {
      const scoped = union(
        ...(expr.guard === undefined ? [] : [freeVarsIR1Expr(expr.guard)]),
        freeVarsIR1Expr(expr.body),
      );
      scoped.delete(expr.binder);
      return scoped;
    }
    case "map-read":
      return union(freeVarsIR1Expr(expr.receiver), freeVarsIR1Expr(expr.key));
    case "set-read":
      return union(freeVarsIR1Expr(expr.receiver), freeVarsIR1Expr(expr.elem));
    default: {
      const _exhaustive: never = expr;
      return _exhaustive;
    }
  }
}

export function freeVarsIR1Stmt(stmt: IR1Stmt): Set<string> {
  return freeVarsStmtWithScope(stmt, new Set());
}

export function freeVarsIR1SsaLocation(location: IR1SsaLocation): Set<string> {
  switch (location.kind) {
    case "property":
      return freeVarsIR1Expr(location.receiver);
    case "map-value":
    case "map-membership":
      return union(
        freeVarsIR1Expr(location.receiver),
        freeVarsIR1Expr(location.key),
      );
    case "set-membership":
      return freeVarsIR1Expr(location.receiver);
    case "return-value":
    case "local-binding":
      return new Set();
    default: {
      const _exhaustive: never = location;
      return _exhaustive;
    }
  }
}

export function freeVarsIR1SsaLoopBody(body: IR1SsaLoopBody): Set<string> {
  return union(
    ...body.headerJoins.flatMap((header) => [
      freeVarsIR1SsaLocation(header.location),
      freeVarsIR1SsaLocation(header.preheaderVersion.location),
      ...(header.loopBackVersion === null
        ? []
        : [freeVarsIR1SsaLocation(header.loopBackVersion.location)]),
    ]),
    ...body.writes.flatMap((write) => [
      freeVarsIR1SsaLocation(write.location),
      freeVarsIR1SsaValue(write.value),
    ]),
    ...body.joins.flatMap((join) => [
      freeVarsIR1SsaLocation(join.location),
      freeVarsIR1SsaLocation(join.thenVersion.location),
      freeVarsIR1SsaLocation(join.elseVersion.location),
    ]),
    ...body.breakHandles.flatMap((handle) => [
      freeVarsIR1SsaLocation(handle.location),
      freeVarsIR1SsaLocation(handle.version.location),
    ]),
    ...body.continueHandles.flatMap((handle) => [
      freeVarsIR1SsaLocation(handle.location),
      freeVarsIR1SsaLocation(handle.version.location),
    ]),
    ...body.returnHandles.flatMap((handle) => [
      freeVarsIR1SsaLocation(handle.location),
      freeVarsIR1SsaLocation(handle.version.location),
    ]),
    ...body.throwHandles.flatMap((handle) => [
      freeVarsIR1SsaLocation(handle.location),
      freeVarsIR1SsaLocation(handle.version.location),
      freeVarsIR1Expr(handle.guard),
    ]),
    ...(body.terminationMetric === null
      ? []
      : body.terminationMetric.kind === "ssa-termination-metric"
        ? [
            freeVarsIR1Expr(body.terminationMetric.expr),
            ...(body.terminationMetric.lowerBound === null
              ? []
              : [freeVarsIR1Expr(body.terminationMetric.lowerBound)]),
          ]
        : [freeVarsIR1Expr(body.terminationMetric.source)]),
  );
}

export function substituteIR1ExprSubtree(
  haystack: IR1Expr,
  needle: IR1Expr,
  replacement: IR1Expr,
): IR1Expr {
  const supportedNeedle = assertSupportedNeedle(needle);
  const replacementFreeVars = freeVarsIR1Expr(replacement);
  return substituteExpr(
    haystack,
    supportedNeedle,
    replacement,
    replacementFreeVars,
  );
}

export function substituteIR1StmtSubtree(
  haystack: IR1Stmt,
  needle: IR1Expr,
  replacement: IR1Expr,
): IR1Stmt {
  const supportedNeedle = assertSupportedNeedle(needle);
  const replacementFreeVars = freeVarsIR1Expr(replacement);
  return substituteStmt(
    haystack,
    supportedNeedle,
    replacement,
    replacementFreeVars,
    new Set(),
  );
}

function freeVarsStmtWithScope(stmt: IR1Stmt, bound: Set<string>): Set<string> {
  switch (stmt.kind) {
    case "block": {
      let inScope = new Set(bound);
      const free = new Set<string>();
      for (const child of stmt.stmts) {
        addAll(free, freeVarsStmtWithScope(child, inScope));
        if (child.kind === "let") {
          inScope = addName(inScope, child.name);
        }
      }
      return free;
    }
    case "let":
      return without(freeVarsIR1Expr(stmt.value), bound);
    case "assign":
      return without(
        union(freeVarsIR1Expr(stmt.target), freeVarsIR1Expr(stmt.value)),
        bound,
      );
    case "cond-stmt":
      return union(
        ...stmt.arms.flatMap(([guard, body]) => [
          without(freeVarsIR1Expr(guard), bound),
          freeVarsStmtWithScope(body, bound),
        ]),
        ...(stmt.otherwise === null
          ? []
          : [freeVarsStmtWithScope(stmt.otherwise, bound)]),
      );
    case "foreach": {
      const scoped = addName(bound, stmt.binder);
      return union(
        without(freeVarsIR1Expr(stmt.source), bound),
        ...(stmt.body === null
          ? []
          : [freeVarsStmtWithScope(stmt.body, scoped)]),
        ...stmt.foldLeaves.map((leaf) => freeVarsFoldLeaf(leaf, scoped)),
      );
    }
    case "for": {
      const free = new Set<string>();
      let inScope = new Set(bound);
      if (stmt.init !== null) {
        addAll(free, freeVarsStmtWithScope(stmt.init, inScope));
        inScope = extendWithForInitBinders(inScope, stmt.init);
      }
      if (stmt.cond !== null) {
        addAll(free, without(freeVarsIR1Expr(stmt.cond), inScope));
      }
      if (stmt.step !== null) {
        addAll(free, freeVarsStmtWithScope(stmt.step, inScope));
      }
      addAll(free, freeVarsStmtWithScope(stmt.body, inScope));
      return free;
    }
    case "while":
      return union(
        without(freeVarsIR1Expr(stmt.cond), bound),
        freeVarsStmtWithScope(stmt.body, bound),
      );
    case "return":
      return stmt.expr === null
        ? new Set()
        : without(freeVarsIR1Expr(stmt.expr), bound);
    case "break":
    case "continue":
      return new Set();
    case "throw":
      return without(freeVarsIR1Expr(stmt.expr), bound);
    case "expr-stmt":
      return without(freeVarsIR1Expr(stmt.expr), bound);
    case "map-effect":
      return union(
        without(freeVarsIR1Expr(stmt.objExpr), bound),
        without(freeVarsIR1Expr(stmt.keyExpr), bound),
        ...(stmt.valueExpr === null
          ? []
          : [without(freeVarsIR1Expr(stmt.valueExpr), bound)]),
      );
    case "set-effect":
      return union(
        without(freeVarsIR1Expr(stmt.objExpr), bound),
        ...(stmt.elemExpr === null
          ? []
          : [without(freeVarsIR1Expr(stmt.elemExpr), bound)]),
      );
    default: {
      const _exhaustive: never = stmt;
      return _exhaustive;
    }
  }
}

function freeVarsFoldLeaf(leaf: IR1FoldLeaf, bound: Set<string>): Set<string> {
  return union(
    without(freeVarsIR1Expr(leaf.target), bound),
    without(freeVarsIR1Expr(leaf.rhs), bound),
    ...(leaf.guard === null
      ? []
      : [without(freeVarsIR1Expr(leaf.guard), bound)]),
  );
}

function freeVarsIR1SsaValue(value: IR1SsaValue): Set<string> {
  switch (value.kind) {
    case "property":
    case "local-binding":
    case "map-value":
      return freeVarsIR1Expr(value.value);
    case "map-membership":
      return new Set();
    case "set-membership":
      return value.elem === null ? new Set() : freeVarsIR1Expr(value.elem);
    default: {
      const _exhaustive: never = value;
      return _exhaustive;
    }
  }
}

function substituteExpr(
  expr: IR1Expr,
  needle: SupportedNeedle,
  replacement: IR1Expr,
  replacementFreeVars: Set<string>,
): IR1Expr {
  if (ir1ExprStructurallyEqual(expr, needle)) {
    return replacement;
  }
  switch (expr.kind) {
    case "var":
    case "lit":
      return expr;
    case "binop": {
      const lhs = substituteExpr(
        expr.lhs,
        needle,
        replacement,
        replacementFreeVars,
      );
      const rhs = substituteExpr(
        expr.rhs,
        needle,
        replacement,
        replacementFreeVars,
      );
      return lhs === expr.lhs && rhs === expr.rhs
        ? expr
        : ir1Binop(expr.op, lhs, rhs);
    }
    case "unop": {
      const arg = substituteExpr(
        expr.arg,
        needle,
        replacement,
        replacementFreeVars,
      );
      return arg === expr.arg ? expr : ir1Unop(expr.op, arg);
    }
    case "app": {
      const callee = substituteExpr(
        expr.callee,
        needle,
        replacement,
        replacementFreeVars,
      );
      const args = expr.args.map((arg) =>
        substituteExpr(arg, needle, replacement, replacementFreeVars),
      );
      return callee === expr.callee && arrayRefEqual(args, expr.args)
        ? expr
        : ir1App(callee, args);
    }
    case "member": {
      const receiver = substituteExpr(
        expr.receiver,
        needle,
        replacement,
        replacementFreeVars,
      );
      return receiver === expr.receiver ? expr : ir1Member(receiver, expr.name);
    }
    case "cond": {
      const arms = expr.arms.map(([guard, value]) => [
        substituteExpr(guard, needle, replacement, replacementFreeVars),
        substituteExpr(value, needle, replacement, replacementFreeVars),
      ]) as unknown as [
        readonly [IR1Expr, IR1Expr],
        ...ReadonlyArray<readonly [IR1Expr, IR1Expr]>,
      ];
      const otherwise = substituteExpr(
        expr.otherwise,
        needle,
        replacement,
        replacementFreeVars,
      );
      const armsUnchanged = arms.every(
        ([guard, value], index) =>
          guard === expr.arms[index]![0] && value === expr.arms[index]![1],
      );
      return armsUnchanged && otherwise === expr.otherwise
        ? expr
        : ir1Cond(arms, otherwise);
    }
    case "is-nullish": {
      const operand = substituteExpr(
        expr.operand,
        needle,
        replacement,
        replacementFreeVars,
      );
      return operand === expr.operand ? expr : ir1IsNullish(operand);
    }
    case "each": {
      throwIfCaptureRisk(expr.binder, replacementFreeVars);
      const src = substituteExpr(
        expr.src,
        needle,
        replacement,
        replacementFreeVars,
      );
      if (shadowsNeedle(expr.binder, needle)) {
        return src === expr.src
          ? expr
          : ir1Each(expr.binder, src, expr.guards, expr.proj);
      }
      const guards = expr.guards.map((guard) =>
        substituteExpr(guard, needle, replacement, replacementFreeVars),
      );
      const proj = substituteExpr(
        expr.proj,
        needle,
        replacement,
        replacementFreeVars,
      );
      return src === expr.src &&
        arrayRefEqual(guards, expr.guards) &&
        proj === expr.proj
        ? expr
        : ir1Each(expr.binder, src, guards, proj);
    }
    case "comb-typed": {
      throwIfCaptureRisk(expr.binder, replacementFreeVars);
      if (shadowsNeedle(expr.binder, needle)) {
        return expr;
      }
      const guards = expr.guards.map((guard) =>
        substituteExpr(guard, needle, replacement, replacementFreeVars),
      );
      const proj = substituteExpr(
        expr.proj,
        needle,
        replacement,
        replacementFreeVars,
      );
      return arrayRefEqual(guards, expr.guards) && proj === expr.proj
        ? expr
        : ir1CombTyped(
            expr.combiner,
            expr.binder,
            expr.binderType,
            guards,
            proj,
          );
    }
    case "forall": {
      throwIfCaptureRisk(expr.binder, replacementFreeVars);
      if (shadowsNeedle(expr.binder, needle)) {
        return expr;
      }
      const guard =
        expr.guard === undefined
          ? undefined
          : substituteExpr(
              expr.guard,
              needle,
              replacement,
              replacementFreeVars,
            );
      const body = substituteExpr(
        expr.body,
        needle,
        replacement,
        replacementFreeVars,
      );
      return guard === expr.guard && body === expr.body
        ? expr
        : ir1Forall(expr.binder, expr.binderType, body, guard);
    }
    case "exists": {
      throwIfCaptureRisk(expr.binder, replacementFreeVars);
      if (shadowsNeedle(expr.binder, needle)) {
        return expr;
      }
      const guard =
        expr.guard === undefined
          ? undefined
          : substituteExpr(
              expr.guard,
              needle,
              replacement,
              replacementFreeVars,
            );
      const body = substituteExpr(
        expr.body,
        needle,
        replacement,
        replacementFreeVars,
      );
      return guard === expr.guard && body === expr.body
        ? expr
        : ir1Exists(expr.binder, expr.binderType, body, guard);
    }
    case "map-read": {
      const receiver = substituteExpr(
        expr.receiver,
        needle,
        replacement,
        replacementFreeVars,
      );
      const key = substituteExpr(
        expr.key,
        needle,
        replacement,
        replacementFreeVars,
      );
      return receiver === expr.receiver && key === expr.key
        ? expr
        : {
            ...expr,
            receiver,
            key,
          };
    }
    case "set-read": {
      const receiver = substituteExpr(
        expr.receiver,
        needle,
        replacement,
        replacementFreeVars,
      );
      const elem = substituteExpr(
        expr.elem,
        needle,
        replacement,
        replacementFreeVars,
      );
      return receiver === expr.receiver && elem === expr.elem
        ? expr
        : {
            ...expr,
            receiver,
            elem,
          };
    }
    default: {
      const _exhaustive: never = expr;
      return _exhaustive;
    }
  }
}

function substituteStmt(
  stmt: IR1Stmt,
  needle: SupportedNeedle,
  replacement: IR1Expr,
  replacementFreeVars: Set<string>,
  scopedBinders: Set<string>,
): IR1Stmt {
  switch (stmt.kind) {
    case "block": {
      let inScope = new Set(scopedBinders);
      const stmts = stmt.stmts.map((child) => {
        const rewritten = substituteStmt(
          child,
          needle,
          replacement,
          replacementFreeVars,
          inScope,
        );
        if (child.kind === "let") {
          throwIfCaptureRisk(child.name, replacementFreeVars);
          inScope = addName(inScope, child.name);
        }
        return rewritten;
      });
      return ir1Block(stmts as [IR1Stmt, ...IR1Stmt[]]);
    }
    case "let":
      return {
        ...stmt,
        value: substituteExprRespectingScope(
          stmt.value,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      };
    case "assign":
      return ir1Assign(
        substituteExprRespectingScope(
          stmt.target,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
        substituteExprRespectingScope(
          stmt.value,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      );
    case "cond-stmt":
      return ir1CondStmt(
        stmt.arms.map(([guard, body]) => [
          substituteExprRespectingScope(
            guard,
            needle,
            replacement,
            replacementFreeVars,
            scopedBinders,
          ),
          substituteStmt(
            body,
            needle,
            replacement,
            replacementFreeVars,
            scopedBinders,
          ),
        ]) as unknown as [
          readonly [IR1Expr, IR1Stmt],
          ...ReadonlyArray<readonly [IR1Expr, IR1Stmt]>,
        ],
        stmt.otherwise === null
          ? null
          : substituteStmt(
              stmt.otherwise,
              needle,
              replacement,
              replacementFreeVars,
              scopedBinders,
            ),
      );
    case "foreach": {
      const source = substituteExprRespectingScope(
        stmt.source,
        needle,
        replacement,
        replacementFreeVars,
        scopedBinders,
      );
      throwIfCaptureRisk(stmt.binder, replacementFreeVars);
      if (shadowsNeedle(stmt.binder, needle)) {
        return ir1Foreach(stmt.binder, source, stmt.body, stmt.foldLeaves);
      }
      const innerScope = addName(scopedBinders, stmt.binder);
      return ir1Foreach(
        stmt.binder,
        source,
        stmt.body === null
          ? null
          : (substituteStmt(
              stmt.body,
              needle,
              replacement,
              replacementFreeVars,
              innerScope,
            ) as typeof stmt.body),
        stmt.foldLeaves.map((leaf) =>
          substituteFoldLeaf(
            leaf,
            needle,
            replacement,
            replacementFreeVars,
            innerScope,
          ),
        ),
      );
    }
    case "for": {
      const init =
        stmt.init === null
          ? null
          : substituteStmt(
              stmt.init,
              needle,
              replacement,
              replacementFreeVars,
              scopedBinders,
            );
      const innerScope =
        stmt.init === null
          ? scopedBinders
          : extendWithForInitBinders(scopedBinders, stmt.init);
      return ir1For(
        init,
        stmt.cond === null
          ? null
          : substituteExprRespectingScope(
              stmt.cond,
              needle,
              replacement,
              replacementFreeVars,
              innerScope,
            ),
        stmt.step === null
          ? null
          : substituteStmt(
              stmt.step,
              needle,
              replacement,
              replacementFreeVars,
              innerScope,
            ),
        substituteStmt(
          stmt.body,
          needle,
          replacement,
          replacementFreeVars,
          innerScope,
        ),
      );
    }
    case "while":
      return ir1While(
        substituteExprRespectingScope(
          stmt.cond,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
        substituteStmt(
          stmt.body,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      );
    case "return":
      return ir1Return(
        stmt.expr === null
          ? null
          : substituteExprRespectingScope(
              stmt.expr,
              needle,
              replacement,
              replacementFreeVars,
              scopedBinders,
            ),
      );
    case "break":
    case "continue":
      return stmt;
    case "throw":
      return ir1Throw(
        substituteExprRespectingScope(
          stmt.expr,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      );
    case "expr-stmt":
      return ir1ExprStmt(
        substituteExprRespectingScope(
          stmt.expr,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      );
    case "map-effect": {
      const objExpr = substituteExprRespectingScope(
        stmt.objExpr,
        needle,
        replacement,
        replacementFreeVars,
        scopedBinders,
      );
      const keyExpr = substituteExprRespectingScope(
        stmt.keyExpr,
        needle,
        replacement,
        replacementFreeVars,
        scopedBinders,
      );
      if (stmt.op === "delete") {
        return { ...stmt, objExpr, keyExpr, valueExpr: null };
      }
      return {
        ...stmt,
        objExpr,
        keyExpr,
        valueExpr: substituteExprRespectingScope(
          stmt.valueExpr,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      };
    }
    case "set-effect": {
      const objExpr = substituteExprRespectingScope(
        stmt.objExpr,
        needle,
        replacement,
        replacementFreeVars,
        scopedBinders,
      );
      if (stmt.op === "clear") {
        return { ...stmt, objExpr, elemExpr: null };
      }
      return {
        ...stmt,
        objExpr,
        elemExpr: substituteExprRespectingScope(
          stmt.elemExpr,
          needle,
          replacement,
          replacementFreeVars,
          scopedBinders,
        ),
      };
    }
    default: {
      const _exhaustive: never = stmt;
      return _exhaustive;
    }
  }
}

function substituteExprRespectingScope(
  expr: IR1Expr,
  needle: SupportedNeedle,
  replacement: IR1Expr,
  replacementFreeVars: Set<string>,
  scopedBinders: Set<string>,
): IR1Expr {
  for (const binder of scopedBinders) {
    throwIfCaptureRisk(binder, replacementFreeVars);
    if (shadowsNeedle(binder, needle)) {
      return expr;
    }
  }
  return substituteExpr(expr, needle, replacement, replacementFreeVars);
}

function substituteFoldLeaf(
  leaf: IR1FoldLeaf,
  needle: SupportedNeedle,
  replacement: IR1Expr,
  replacementFreeVars: Set<string>,
  scopedBinders: Set<string>,
): IR1FoldLeaf {
  return {
    ...leaf,
    target: substituteExprRespectingScope(
      leaf.target,
      needle,
      replacement,
      replacementFreeVars,
      scopedBinders,
    ),
    rhs: substituteExprRespectingScope(
      leaf.rhs,
      needle,
      replacement,
      replacementFreeVars,
      scopedBinders,
    ),
    guard:
      leaf.guard === null
        ? null
        : substituteExprRespectingScope(
            leaf.guard,
            needle,
            replacement,
            replacementFreeVars,
            scopedBinders,
          ),
  };
}

function extendWithForInitBinders(
  scopedBinders: Set<string>,
  init: IR1Stmt,
): Set<string> {
  if (init.kind !== "let") {
    return scopedBinders;
  }
  return addName(scopedBinders, init.name);
}

function assertSupportedNeedle(needle: IR1Expr): SupportedNeedle {
  if (needle.kind === "var" || needle.kind === "member") {
    return needle;
  }
  throw new Error(`unsupported needle kind: ${needle.kind}`);
}

function ir1ExprStructurallyEqual(a: IR1Expr, b: SupportedNeedle): boolean {
  if (a.kind !== b.kind) {
    return false;
  }
  if (a.kind === "var" && b.kind === "var") {
    return a.name === b.name && (a.primed ?? false) === (b.primed ?? false);
  }
  if (a.kind === "member" && b.kind === "member") {
    return (
      a.name === b.name &&
      ir1ExprStructurallyEqualReceiver(a.receiver, b.receiver)
    );
  }
  return false;
}

function ir1ExprStructurallyEqualReceiver(a: IR1Expr, b: IR1Expr): boolean {
  if (b.kind === "var" || b.kind === "member") {
    return ir1ExprStructurallyEqual(a, b);
  }
  return false;
}

function shadowsNeedle(binder: string, needle: SupportedNeedle): boolean {
  return needle.kind === "var" && !needle.primed && needle.name === binder;
}

function throwIfCaptureRisk(
  binderName: string,
  replacementFreeVars: Set<string>,
): void {
  if (replacementFreeVars.has(binderName)) {
    throw new CaptureRiskError(binderName, binderName);
  }
}

function union(...sets: Set<string>[]): Set<string> {
  const out = new Set<string>();
  for (const set of sets) {
    addAll(out, set);
  }
  return out;
}

function addAll(target: Set<string>, source: Set<string>): void {
  for (const value of source) {
    target.add(value);
  }
}

function without(source: Set<string>, names: Set<string>): Set<string> {
  const out = new Set(source);
  for (const name of names) {
    out.delete(name);
  }
  return out;
}

function addName(source: Set<string>, name: string): Set<string> {
  const out = new Set(source);
  out.add(name);
  return out;
}

function arrayRefEqual<T>(a: readonly T[], b: readonly T[]): boolean {
  return a.length === b.length && a.every((value, index) => value === b[index]);
}
