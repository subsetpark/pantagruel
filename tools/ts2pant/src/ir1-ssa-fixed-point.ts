import { lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1SsaLocation,
  type IR1SsaLoopBody,
  type IR1SsaProgram,
  type IR1SsaVersion,
  type IR1SsaWrite,
  type IR1Stmt,
  ir1SsaCloseLoopHeader,
  ir1SsaInitialVersion,
  ir1SsaLoopBody,
  ir1SsaOpenLoopHeader,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRuleOfLocation,
  ir1SsaWrite,
  ir1Var,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { LoopSsaBuildOptions } from "./ir1-ssa-loops.js";
import {
  type IR1SsaBodyLowerResult,
  ir1SsaBodyLowerSuccess,
  ir1SsaBodyLowerUnsupported,
} from "./ir1-ssa-lower.js";
import { registerName } from "./name-registry.js";
import type { OpaqueExpr, OpaqueTypeExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { SynthCell } from "./translate-types.js";
import type { PropResult } from "./types.js";

export interface FixedPointLoopLowerOptions extends LoopSsaBuildOptions {
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
  synthCell?: SynthCell;
  locationType?: OpaqueTypeExpr | string;
  invariantTypes?: ReadonlyMap<string, OpaqueTypeExpr | string>;
}

export interface FixedPointLoopShape {
  guardExpr: IR1Expr;
  bodyStmt: IR1Stmt;
  mutatedLocation: Extract<IR1SsaLocation, { kind: "property" }>;
}

interface TargetInfo {
  target: Extract<IR1Expr, { kind: "member" }>;
  location: Extract<IR1SsaLocation, { kind: "property" }>;
  objExpr: OpaqueExpr;
  lhs: OpaqueExpr;
  prior: OpaqueExpr;
  key: string;
}

export function recognizeFixedPointLoopShape(
  stmt: Extract<IR1Stmt, { kind: "while" }>,
): FixedPointLoopShape | { unsupported: string } {
  if (
    stmt.cond.kind === "lit" &&
    stmt.cond.value.kind === "bool" &&
    stmt.cond.value.value === true
  ) {
    return {
      unsupported:
        "while loop has a literal-true guard; cannot define a fixed point",
    };
  }

  const locations = collectMutatedPropertyLocations(stmt.body);
  if (locations.size > 1) {
    return {
      unsupported: `fixed-point while lowering supports single-location bodies only; this loop modifies ${locations.size} rules`,
    };
  }
  const mutatedLocation = [...locations.values()][0];
  if (mutatedLocation === undefined) {
    return {
      unsupported: "fixed-point while lowering requires one property mutation",
    };
  }
  return {
    guardExpr: stmt.cond,
    bodyStmt: stmt.body,
    mutatedLocation,
  };
}

export function lowerFixedPointLoopL1Body(
  stmt: Extract<IR1Stmt, { kind: "while" }>,
  options: FixedPointLoopLowerOptions = {},
): IR1SsaBodyLowerResult {
  const shape = recognizeFixedPointLoopShape(stmt);
  if ("unsupported" in shape) {
    return ir1SsaBodyLowerUnsupported(shape.unsupported);
  }

  const writeBody = findSinglePropertyWrite(shape.bodyStmt);
  if ("unsupported" in writeBody) {
    return ir1SsaBodyLowerUnsupported(writeBody.unsupported);
  }
  if (!sameLocation(shape.mutatedLocation, writeBody.location)) {
    return ir1SsaBodyLowerUnsupported(
      "fixed-point while lowering could not match the mutated location to the loop body write",
    );
  }

  const lowerOpaque = options.lowerOpaque ?? ((e: OpaqueExpr) => e);
  const ast = getAst();
  const targetInfo = buildTargetInfo(writeBody.target, lowerOpaque, options);
  const preheaderVersion: IR1SsaVersion = ir1SsaInitialVersion(
    targetInfo.location,
  );
  const header = ir1SsaOpenLoopHeader(targetInfo.location, preheaderVersion);
  const stateVar = ir1Var("s");
  const effectExpr = substituteMemberReads(
    writeBody.value,
    writeBody.target,
    stateVar,
  );
  const write: IR1SsaWrite = ir1SsaWrite(
    targetInfo.location,
    ir1SsaPropertyValue(effectExpr),
  );
  ir1SsaCloseLoopHeader(header, write.version);

  const loopBody: IR1SsaLoopBody = ir1SsaLoopBody({
    headerJoins: [header],
    writes: [write],
    joins: [],
    breakHandles: [],
    continueHandles: [],
    terminationMetric: null,
  });

  const ruleName = allocateLoopRuleName(options.synthCell);
  const locationType = typeExpr(options.locationType);
  const invariantNames = collectLoopInvariantNames(
    [shape.guardExpr, writeBody.value],
    writeBody.target,
  );
  const invariantParams = invariantNames.map((name) => ({
    name,
    type: typeExpr(options.invariantTypes?.get(name)),
  }));
  const effect = lowerOpaque(lowerExpr(lowerL1Expr(effectExpr)));
  const loweredGuard = lowerOpaque(
    lowerExpr(
      lowerL1Expr(
        substituteMemberReads(shape.guardExpr, writeBody.target, stateVar),
      ),
    ),
  );
  const helperCallOnEffect = ast.app(ast.var(ruleName), [
    effect,
    ...invariantNames.map((name) => ast.var(name)),
  ]);
  const helperBody = ast.cond([
    [loweredGuard, helperCallOnEffect],
    [ast.litBool(true), ast.var("s")],
  ]);
  const ruleDecl: PropResult = {
    kind: "rule-decl",
    ruleName,
    params: [{ name: "s", type: locationType }, ...invariantParams],
    returnType: locationType,
    body: helperBody,
  };
  const callerEquation: PropResult = {
    kind: "equation",
    quantifiers: [],
    lhs: targetInfo.lhs,
    rhs: ast.app(ast.var(ruleName), [
      targetInfo.prior,
      ...invariantNames.map((name) => ast.var(name)),
    ]),
  };

  const modifiedRules = [ir1SsaRuleOfLocation(targetInfo.location)];
  const declaredRules = new Set([
    ...(options.declaredRules ?? []),
    ...modifiedRules,
  ]);
  const program: IR1SsaProgram = {
    reads: [],
    writes: [write],
    joins: [],
    loopSummaries: [],
    loopHeaderJoins: [header],
    loopBodies: [loopBody],
    declaredRules: [...declaredRules],
    modifiedRules,
    framedRules: [...declaredRules].filter(
      (rule) => !modifiedRules.includes(rule),
    ),
  };

  return ir1SsaBodyLowerSuccess({
    programs: [program],
    propositions: [ruleDecl, callerEquation],
    modifiedRules,
    finalProperties: [
      {
        location: targetInfo.location,
        version: write.version,
        objExpr: targetInfo.objExpr,
        lhs: targetInfo.lhs,
        rhs: callerEquation.rhs,
      },
    ],
  });
}

function buildTargetInfo(
  target: Extract<IR1Expr, { kind: "member" }>,
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr,
  options: FixedPointLoopLowerOptions,
): TargetInfo {
  const ast = getAst();
  const objExpr = lowerOpaque(lowerExpr(lowerL1Expr(target.receiver)));
  const location = ir1SsaPropertyLocation(
    target.name,
    target.receiver,
    target.name,
  ) as Extract<IR1SsaLocation, { kind: "property" }>;
  const lhs = ast.app(ast.primed(target.name), [objExpr]);
  const key = `${target.name}::${ast.strExpr(objExpr)}`;
  const prior =
    options.initialPropertyValues?.get(key) ??
    ast.app(ast.var(target.name), [objExpr]);
  return { target, location, objExpr, lhs, prior, key };
}

function typeExpr(type: OpaqueTypeExpr | string | undefined): OpaqueTypeExpr {
  const ast = getAst();
  return typeof type === "string"
    ? ast.tName(type)
    : (type ?? ast.tName("Nat0"));
}

function allocateLoopRuleName(synthCell: SynthCell | undefined): string {
  if (synthCell === undefined) {
    return "fn--loop";
  }
  const registered = registerName(synthCell.registry, "fn--loop");
  synthCell.registry = registered.registry;
  return registered.name;
}

function collectMutatedPropertyLocations(
  stmt: IR1Stmt,
): Map<string, Extract<IR1SsaLocation, { kind: "property" }>> {
  const out = new Map<string, Extract<IR1SsaLocation, { kind: "property" }>>();
  walkStmt(stmt, (s) => {
    if (s.kind === "assign" && s.target.kind === "member") {
      const location = ir1SsaPropertyLocation(
        s.target.name,
        s.target.receiver,
        s.target.name,
      ) as Extract<IR1SsaLocation, { kind: "property" }>;
      out.set(locationKey(location), location);
    }
  });
  return out;
}

function findSinglePropertyWrite(stmt: IR1Stmt):
  | {
      target: Extract<IR1Expr, { kind: "member" }>;
      value: IR1Expr;
      location: Extract<IR1SsaLocation, { kind: "property" }>;
    }
  | { unsupported: string } {
  const writes: Array<{
    target: Extract<IR1Expr, { kind: "member" }>;
    value: IR1Expr;
    location: Extract<IR1SsaLocation, { kind: "property" }>;
  }> = [];
  walkStmt(stmt, (s) => {
    if (s.kind === "assign" && s.target.kind === "member") {
      const location = ir1SsaPropertyLocation(
        s.target.name,
        s.target.receiver,
        s.target.name,
      ) as Extract<IR1SsaLocation, { kind: "property" }>;
      writes.push({ target: s.target, value: s.value, location });
    }
  });
  if (writes.length !== 1) {
    return {
      unsupported:
        "fixed-point while lowering requires exactly one property write in the loop body",
    };
  }
  return writes[0]!;
}

function walkStmt(stmt: IR1Stmt, visit: (stmt: IR1Stmt) => void): void {
  visit(stmt);
  switch (stmt.kind) {
    case "block":
      for (const child of stmt.stmts) {
        walkStmt(child, visit);
      }
      break;
    case "cond-stmt":
      for (const [, body] of stmt.arms) {
        walkStmt(body, visit);
      }
      if (stmt.otherwise !== null) {
        walkStmt(stmt.otherwise, visit);
      }
      break;
    case "for":
      if (stmt.init !== null) {
        walkStmt(stmt.init, visit);
      }
      if (stmt.step !== null) {
        walkStmt(stmt.step, visit);
      }
      walkStmt(stmt.body, visit);
      break;
    case "while":
      walkStmt(stmt.body, visit);
      break;
    case "let":
    case "assign":
    case "foreach":
    case "return":
    case "throw":
    case "expr-stmt":
    case "map-effect":
    case "set-effect":
      break;
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
    }
  }
}

function substituteMemberReads(
  expr: IR1Expr,
  member: Extract<IR1Expr, { kind: "member" }>,
  replacement: IR1Expr,
): IR1Expr {
  if (sameMemberExpr(expr, member)) {
    return replacement;
  }
  switch (expr.kind) {
    case "var":
    case "lit":
      return expr;
    case "binop":
      return {
        ...expr,
        lhs: substituteMemberReads(expr.lhs, member, replacement),
        rhs: substituteMemberReads(expr.rhs, member, replacement),
      };
    case "unop":
      return {
        ...expr,
        arg: substituteMemberReads(expr.arg, member, replacement),
      };
    case "app":
      return {
        ...expr,
        callee: substituteMemberReads(expr.callee, member, replacement),
        args: expr.args.map((arg) =>
          substituteMemberReads(arg, member, replacement),
        ),
      };
    case "member":
      return {
        ...expr,
        receiver: substituteMemberReads(expr.receiver, member, replacement),
      };
    case "cond":
      return {
        ...expr,
        arms: expr.arms.map(([guard, value]) => [
          substituteMemberReads(guard, member, replacement),
          substituteMemberReads(value, member, replacement),
        ]),
        otherwise: substituteMemberReads(expr.otherwise, member, replacement),
      };
    case "is-nullish":
      return {
        ...expr,
        operand: substituteMemberReads(expr.operand, member, replacement),
      };
    case "each":
      return {
        ...expr,
        src: substituteMemberReads(expr.src, member, replacement),
        guards: expr.guards.map((guard) =>
          substituteMemberReads(guard, member, replacement),
        ),
        proj: substituteMemberReads(expr.proj, member, replacement),
      };
    case "map-read":
      return {
        ...expr,
        receiver: substituteMemberReads(expr.receiver, member, replacement),
        key: substituteMemberReads(expr.key, member, replacement),
      };
    case "set-read":
      return {
        ...expr,
        receiver: substituteMemberReads(expr.receiver, member, replacement),
        elem: substituteMemberReads(expr.elem, member, replacement),
      };
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return expr;
    }
  }
}

function collectLoopInvariantNames(
  exprs: readonly IR1Expr[],
  target: Extract<IR1Expr, { kind: "member" }>,
): string[] {
  const names = new Set<string>();
  const excluded = new Set<string>(["s"]);
  const receiverRoot = rootName(target.receiver);
  if (receiverRoot !== null) {
    excluded.add(receiverRoot);
  }
  for (const expr of exprs) {
    collectFreeVars(expr, names, excluded);
  }
  return [...names].sort();
}

function collectFreeVars(
  expr: IR1Expr,
  out: Set<string>,
  excluded: ReadonlySet<string>,
  bound: ReadonlySet<string> = new Set(),
): void {
  switch (expr.kind) {
    case "var":
      if (!expr.primed && !excluded.has(expr.name) && !bound.has(expr.name)) {
        out.add(expr.name);
      }
      break;
    case "lit":
      break;
    case "binop":
      collectFreeVars(expr.lhs, out, excluded, bound);
      collectFreeVars(expr.rhs, out, excluded, bound);
      break;
    case "unop":
    case "is-nullish":
      collectFreeVars(
        expr.kind === "unop" ? expr.arg : expr.operand,
        out,
        excluded,
        bound,
      );
      break;
    case "app":
      collectFreeVars(expr.callee, out, excluded, bound);
      for (const arg of expr.args) {
        collectFreeVars(arg, out, excluded, bound);
      }
      break;
    case "member":
      collectFreeVars(expr.receiver, out, excluded, bound);
      break;
    case "cond":
      for (const [guard, value] of expr.arms) {
        collectFreeVars(guard, out, excluded, bound);
        collectFreeVars(value, out, excluded, bound);
      }
      collectFreeVars(expr.otherwise, out, excluded, bound);
      break;
    case "each": {
      collectFreeVars(expr.src, out, excluded, bound);
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      for (const guard of expr.guards) {
        collectFreeVars(guard, out, excluded, nextBound);
      }
      collectFreeVars(expr.proj, out, excluded, nextBound);
      break;
    }
    case "map-read":
      collectFreeVars(expr.receiver, out, excluded, bound);
      collectFreeVars(expr.key, out, excluded, bound);
      break;
    case "set-read":
      collectFreeVars(expr.receiver, out, excluded, bound);
      collectFreeVars(expr.elem, out, excluded, bound);
      break;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
    }
  }
}

function sameLocation(
  a: Extract<IR1SsaLocation, { kind: "property" }>,
  b: Extract<IR1SsaLocation, { kind: "property" }>,
): boolean {
  return a.ruleName === b.ruleName && ir1ExprEqual(a.receiver, b.receiver);
}

function locationKey(location: Extract<IR1SsaLocation, { kind: "property" }>) {
  return `${location.ruleName}::${exprKey(location.receiver)}`;
}

function sameMemberExpr(
  expr: IR1Expr,
  member: Extract<IR1Expr, { kind: "member" }>,
): boolean {
  return (
    expr.kind === "member" &&
    expr.name === member.name &&
    ir1ExprEqual(expr.receiver, member.receiver)
  );
}

function ir1ExprEqual(a: IR1Expr, b: IR1Expr): boolean {
  return exprKey(a) === exprKey(b);
}

function exprKey(expr: IR1Expr): string {
  return JSON.stringify(expr);
}

function rootName(expr: IR1Expr): string | null {
  if (expr.kind === "var") {
    return expr.name;
  }
  if (expr.kind === "member") {
    return rootName(expr.receiver);
  }
  return null;
}
