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
import { substituteIR1ExprSubtree } from "./ir1-substitute.js";
import { registerName } from "./name-registry.js";
import type { OpaqueExpr, OpaqueTypeExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import {
  IntStrategy,
  type NumericStrategy,
  type SynthCell,
} from "./translate-types.js";
import type { PantDeclaration, PropResult } from "./types.js";

export interface FixedPointLoopLowerOptions extends LoopSsaBuildOptions {
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
  synthCell?: SynthCell;
  locationType?: OpaqueTypeExpr | string;
  invariantTypes?: ReadonlyMap<string, OpaqueTypeExpr | string>;
  declarations?: readonly PantDeclaration[];
  strategy?: NumericStrategy;
  returnRuleName?: string;
}

export interface FixedPointLoopShape {
  guardExpr: IR1Expr;
  bodyStmt: IR1Stmt;
  localLets: ReadonlyMap<string, IR1Expr>;
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
  stmt:
    | Extract<IR1Stmt, { kind: "while" }>
    | Extract<IR1Stmt, { kind: "block" }>,
): FixedPointLoopShape | { unsupported: string } {
  if (stmt.kind === "block") {
    const children = stmt.stmts;
    const last = children.at(-1);
    if (last?.kind !== "while") {
      return { unsupported: "fixed-point block must end in a while loop" };
    }
    const localLets = new Map<string, IR1Expr>();
    for (const child of children.slice(0, -1)) {
      if (child.kind !== "let") {
        return {
          unsupported:
            "fixed-point while prelude must contain let bindings only",
        };
      }
      localLets.set(child.name, child.value);
    }
    const recognized = recognizeFixedPointLoopShape(last);
    if ("unsupported" in recognized) {
      return recognized;
    }
    return { ...recognized, localLets };
  }

  if (
    stmt.cond.kind === "lit" &&
    stmt.cond.value.kind === "bool" &&
    stmt.cond.value.value === true &&
    !hasReachableBreakOrReturn(stmt.body)
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
    localLets: new Map(),
    mutatedLocation,
  };
}

export function lowerFixedPointLoopL1Body(
  stmt:
    | Extract<IR1Stmt, { kind: "while" }>
    | Extract<IR1Stmt, { kind: "block" }>,
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
  const guardExpr = substituteLocalLets(shape.guardExpr, shape.localLets);
  const valueExpr = substituteLocalLets(writeBody.value, shape.localLets);
  const loopExits = collectLoopExits(shape.bodyStmt);
  if (
    containsNonTargetMemberRead(guardExpr, writeBody.target) ||
    containsNonTargetMemberRead(valueExpr, writeBody.target)
  ) {
    return ir1SsaBodyLowerUnsupported(
      "fixed-point while lowering supports guards and updates over the mutated property only",
    );
  }
  if (
    !containsTargetMemberRead(guardExpr, writeBody.target) &&
    loopExits.breaks.length === 0 &&
    loopExits.returns.length === 0
  ) {
    return ir1SsaBodyLowerUnsupported(
      "fixed-point while guard must depend on the mutated property",
    );
  }

  const lowerOpaque = options.lowerOpaque ?? ((e: OpaqueExpr) => e);
  const ast = getAst();
  const targetInfo = buildTargetInfo(writeBody.target, lowerOpaque, options);
  const preheaderVersion: IR1SsaVersion = ir1SsaInitialVersion(
    targetInfo.location,
  );
  const header = ir1SsaOpenLoopHeader(targetInfo.location, preheaderVersion);
  const exitExprs = [
    ...loopExits.breaks.map((exit) => exit.guard),
    ...loopExits.continues.map((exit) => exit.guard),
    ...loopExits.returns.flatMap((exit) =>
      exit.expr === null ? [exit.guard] : [exit.guard, exit.expr],
    ),
    ...loopExits.throws.map((exit) => exit.guard),
  ];
  const invariantNames = collectLoopInvariantNames(
    [guardExpr, valueExpr, ...exitExprs],
    writeBody.target,
  );
  const stateName = allocateFreshStateName(
    stateAvoidanceNames([guardExpr, valueExpr], writeBody.target),
  );
  const stateVar = ir1Var(stateName);
  const effectExpr = substituteIR1ExprSubtree(
    valueExpr,
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
    breakHandles: loopExits.breaks.map(() => ({
      kind: "ssa-break-handle",
      location: targetInfo.location,
      version: write.version,
    })),
    continueHandles: loopExits.continues.map(() => ({
      kind: "ssa-continue-handle",
      location: targetInfo.location,
      version: write.version,
    })),
    returnHandles: loopExits.returns.map(() => ({
      kind: "ssa-return-handle",
      location: targetInfo.location,
      version: write.version,
    })),
    throwHandles: loopExits.throws.map((exit) => ({
      kind: "ssa-throw-handle",
      location: targetInfo.location,
      version: write.version,
      guard: exit.guard,
    })),
    terminationMetric: null,
  });

  const ruleName = allocateLoopRuleName(options.synthCell);
  const inferredInvariantTypes = inferInvariantTypes(
    invariantNames,
    options.declarations,
  );
  const locationType = typeExpr(
    options.locationType ??
      inferLocationType(targetInfo.location.ruleName, options.declarations),
    options.strategy,
  );
  const invariantParams = invariantNames.map((name) => ({
    name,
    type: typeExpr(
      options.invariantTypes?.get(name) ?? inferredInvariantTypes.get(name),
      options.strategy,
    ),
  }));
  const effect = lowerOpaque(lowerExpr(lowerL1Expr(effectExpr)));
  const loweredGuard = lowerOpaque(
    lowerExpr(
      lowerL1Expr(
        substituteIR1ExprSubtree(guardExpr, writeBody.target, stateVar),
      ),
    ),
  );
  const lowerExitGuard = (guard: IR1Expr): OpaqueExpr =>
    lowerOpaque(
      lowerExpr(
        lowerL1Expr(
          substituteIR1ExprSubtree(guard, writeBody.target, stateVar),
        ),
      ),
    );
  const orOpaque = (guards: OpaqueExpr[]): OpaqueExpr | null =>
    guards.length === 0
      ? null
      : guards.reduce((lhs, rhs) => ast.binop(ast.opOr(), lhs, rhs));
  const andOpaque = (lhs: OpaqueExpr, rhs: OpaqueExpr): OpaqueExpr =>
    ast.binop(ast.opAnd(), lhs, rhs);
  const continueGuard = orOpaque(
    loopExits.continues.map((exit) => lowerExitGuard(exit.guard)),
  );
  const loopBack =
    continueGuard === null
      ? effect
      : ast.cond([
          [continueGuard, ast.var(stateName)],
          [ast.litBool(true), effect],
        ]);
  const helperCallOnLoopBack = ast.app(ast.var(ruleName), [
    loopBack,
    ...invariantNames.map((name) => ast.var(name)),
  ]);
  const guardedLoopCondition = loopExits.throws
    .map((exit) => ast.unop(ast.opNot(), lowerExitGuard(exit.guard)))
    .reduce((acc, guard) => andOpaque(acc, guard), loweredGuard);
  const helperArms: [OpaqueExpr, OpaqueExpr][] = [];
  for (const exit of loopExits.breaks) {
    helperArms.push([
      andOpaque(guardedLoopCondition, lowerExitGuard(exit.guard)),
      effect,
    ]);
  }
  for (const exit of loopExits.returns) {
    helperArms.push([
      andOpaque(guardedLoopCondition, lowerExitGuard(exit.guard)),
      effect,
    ]);
  }
  helperArms.push([guardedLoopCondition, helperCallOnLoopBack]);
  helperArms.push([ast.litBool(true), ast.var(stateName)]);
  const helperBody = ast.cond(helperArms);
  const ruleDecl: PropResult = {
    kind: "rule-decl",
    ruleName,
    params: [{ name: stateName, type: locationType }, ...invariantParams],
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
    ...(options.returnRuleName === undefined ||
    !loopExits.returns.some((exit) => exit.expr !== null)
      ? {}
      : {
          returnValue: {
            ruleName: options.returnRuleName,
            expression: ast.cond([
              ...loopExits.returns
                .filter((exit) => exit.expr !== null)
                .map((exit): [OpaqueExpr, OpaqueExpr] => [
                  lowerOpaque(lowerExpr(lowerL1Expr(exit.guard))),
                  lowerOpaque(lowerExpr(lowerL1Expr(exit.expr!))),
                ]),
              [ast.litBool(true), callerEquation.rhs],
            ]),
          },
        }),
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

interface LoopExitInfo {
  breaks: Array<{ guard: IR1Expr }>;
  continues: Array<{ guard: IR1Expr }>;
  returns: Array<{ guard: IR1Expr; expr: IR1Expr | null }>;
  throws: Array<{ guard: IR1Expr }>;
}

function collectLoopExits(stmt: IR1Stmt): LoopExitInfo {
  const exits: LoopExitInfo = {
    breaks: [],
    continues: [],
    returns: [],
    throws: [],
  };
  const trueExpr: IR1Expr = {
    kind: "lit",
    value: { kind: "bool", value: true },
  };
  const combineGuard = (outer: IR1Expr, inner: IR1Expr): IR1Expr =>
    isTrueExpr(outer)
      ? inner
      : isTrueExpr(inner)
        ? outer
        : {
            kind: "binop",
            op: "and",
            lhs: outer,
            rhs: inner,
          };
  const visit = (node: IR1Stmt, guard: IR1Expr): void => {
    switch (node.kind) {
      case "block":
        for (const child of node.stmts) {
          visit(child, guard);
        }
        return;
      case "cond-stmt":
        for (const [armGuard, body] of node.arms) {
          visit(body, combineGuard(guard, armGuard));
        }
        if (node.otherwise !== null) {
          visit(node.otherwise, guard);
        }
        return;
      case "break":
        exits.breaks.push({ guard });
        return;
      case "continue":
        exits.continues.push({ guard });
        return;
      case "return":
        exits.returns.push({ guard, expr: node.expr });
        return;
      case "throw":
        exits.throws.push({ guard });
        return;
      case "while":
      case "for":
      case "foreach":
        return;
      case "let":
      case "assign":
      case "expr-stmt":
      case "map-effect":
      case "set-effect":
        return;
      default: {
        const _exhaustive: never = node;
        void _exhaustive;
      }
    }
  };
  visit(stmt, trueExpr);
  return exits;
}

function hasReachableBreakOrReturn(stmt: IR1Stmt): boolean {
  const exits = collectLoopExits(stmt);
  return exits.breaks.length > 0 || exits.returns.length > 0;
}

function isTrueExpr(expr: IR1Expr): boolean {
  return (
    expr.kind === "lit" &&
    expr.value.kind === "bool" &&
    expr.value.value === true
  );
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

function typeExpr(
  type: OpaqueTypeExpr | string | undefined,
  strategy: NumericStrategy = IntStrategy,
): OpaqueTypeExpr {
  const ast = getAst();
  return typeof type === "string"
    ? ast.tName(type)
    : (type ?? ast.tName(strategy.mapNumber()));
}

function inferLocationType(
  ruleName: string,
  declarations: readonly PantDeclaration[] | undefined,
): string | undefined {
  return declarations?.find(
    (decl): decl is Extract<PantDeclaration, { kind: "rule" }> =>
      decl.kind === "rule" && decl.name === ruleName,
  )?.returnType;
}

function inferInvariantTypes(
  names: readonly string[],
  declarations: readonly PantDeclaration[] | undefined,
): ReadonlyMap<string, string> {
  const wanted = new Set(names);
  const out = new Map<string, string>();
  if (wanted.size === 0 || declarations === undefined) {
    return out;
  }
  for (const decl of declarations) {
    if (decl.kind !== "action") {
      continue;
    }
    for (const param of decl.params) {
      if (wanted.has(param.name) && !out.has(param.name)) {
        out.set(param.name, param.type);
      }
    }
  }
  return out;
}

function allocateLoopRuleName(synthCell: SynthCell | undefined): string {
  if (synthCell === undefined) {
    return "fn--loop";
  }
  const registered = registerName(synthCell.registry, "fn--loop");
  synthCell.registry = registered.registry;
  return registered.name;
}

function allocateFreshStateName(forbidden: ReadonlySet<string>): string {
  const registered = registerName({ used: new Set(forbidden) }, "s");
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
    case "break":
    case "continue":
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

function substituteLocalLets(
  expr: IR1Expr,
  localLets: ReadonlyMap<string, IR1Expr>,
  bound: ReadonlySet<string> = new Set(),
  resolving: ReadonlySet<string> = new Set(),
): IR1Expr {
  if (localLets.size === 0) {
    return expr;
  }
  if (
    expr.kind === "var" &&
    !expr.primed &&
    !bound.has(expr.name) &&
    localLets.has(expr.name) &&
    !resolving.has(expr.name)
  ) {
    return substituteLocalLets(
      localLets.get(expr.name)!,
      localLets,
      bound,
      new Set([...resolving, expr.name]),
    );
  }
  switch (expr.kind) {
    case "var":
    case "lit":
      return expr;
    case "binop":
      return {
        ...expr,
        lhs: substituteLocalLets(expr.lhs, localLets, bound, resolving),
        rhs: substituteLocalLets(expr.rhs, localLets, bound, resolving),
      };
    case "unop":
      return {
        ...expr,
        arg: substituteLocalLets(expr.arg, localLets, bound, resolving),
      };
    case "app":
      return {
        ...expr,
        callee: substituteLocalLets(expr.callee, localLets, bound, resolving),
        args: expr.args.map((arg) =>
          substituteLocalLets(arg, localLets, bound, resolving),
        ),
      };
    case "member":
      return {
        ...expr,
        receiver: substituteLocalLets(
          expr.receiver,
          localLets,
          bound,
          resolving,
        ),
      };
    case "cond":
      return {
        ...expr,
        arms: expr.arms.map(([guard, value]) => [
          substituteLocalLets(guard, localLets, bound, resolving),
          substituteLocalLets(value, localLets, bound, resolving),
        ]),
        otherwise: substituteLocalLets(
          expr.otherwise,
          localLets,
          bound,
          resolving,
        ),
      };
    case "is-nullish":
      return {
        ...expr,
        operand: substituteLocalLets(expr.operand, localLets, bound, resolving),
      };
    case "each": {
      const localFreeVars = localLetValueFreeVars(localLets);
      const binder =
        localFreeVars.has(expr.binder) &&
        (expr.guards.length > 0 || expr.proj !== undefined)
          ? freshIr1Name(expr.binder, [
              expr,
              ...localLets.values(),
              ...[...bound].map((name) => ir1Var(name)),
            ])
          : expr.binder;
      const guards =
        binder === expr.binder
          ? expr.guards
          : expr.guards.map((guard) =>
              renameBoundVarRefs(guard, expr.binder, binder),
            );
      const proj =
        binder === expr.binder
          ? expr.proj
          : renameBoundVarRefs(expr.proj, expr.binder, binder);
      const nextBound = new Set(bound);
      nextBound.add(binder);
      return {
        ...expr,
        binder,
        src: substituteLocalLets(expr.src, localLets, bound, resolving),
        guards: guards.map((guard) =>
          substituteLocalLets(guard, localLets, nextBound, resolving),
        ),
        proj: substituteLocalLets(proj, localLets, nextBound, resolving),
      };
    }
    case "comb-typed": {
      const localFreeVars = localLetValueFreeVars(localLets);
      const binder = localFreeVars.has(expr.binder)
        ? freshIr1Name(expr.binder, [
            expr,
            ...localLets.values(),
            ...[...bound].map((name) => ir1Var(name)),
          ])
        : expr.binder;
      const guards =
        binder === expr.binder
          ? expr.guards
          : expr.guards.map((guard) =>
              renameBoundVarRefs(guard, expr.binder, binder),
            );
      const proj =
        binder === expr.binder
          ? expr.proj
          : renameBoundVarRefs(expr.proj, expr.binder, binder);
      const nextBound = new Set(bound);
      nextBound.add(binder);
      return {
        ...expr,
        binder,
        guards: guards.map((guard) =>
          substituteLocalLets(guard, localLets, nextBound, resolving),
        ),
        proj: substituteLocalLets(proj, localLets, nextBound, resolving),
      };
    }
    case "forall":
    case "exists": {
      const localFreeVars = localLetValueFreeVars(localLets);
      const binder = localFreeVars.has(expr.binder)
        ? freshIr1Name(expr.binder, [
            expr,
            ...localLets.values(),
            ...[...bound].map((name) => ir1Var(name)),
          ])
        : expr.binder;
      const guard =
        expr.guard === undefined || binder === expr.binder
          ? expr.guard
          : renameBoundVarRefs(expr.guard, expr.binder, binder);
      const body =
        binder === expr.binder
          ? expr.body
          : renameBoundVarRefs(expr.body, expr.binder, binder);
      const nextBound = new Set(bound);
      nextBound.add(binder);
      return {
        ...expr,
        binder,
        ...(guard === undefined
          ? {}
          : {
              guard: substituteLocalLets(
                guard,
                localLets,
                nextBound,
                resolving,
              ),
            }),
        body: substituteLocalLets(body, localLets, nextBound, resolving),
      };
    }
    case "map-read":
      return {
        ...expr,
        receiver: substituteLocalLets(
          expr.receiver,
          localLets,
          bound,
          resolving,
        ),
        key: substituteLocalLets(expr.key, localLets, bound, resolving),
      };
    case "set-read":
      return {
        ...expr,
        receiver: substituteLocalLets(
          expr.receiver,
          localLets,
          bound,
          resolving,
        ),
        elem: substituteLocalLets(expr.elem, localLets, bound, resolving),
      };
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return expr;
    }
  }
}

function localLetValueFreeVars(
  localLets: ReadonlyMap<string, IR1Expr>,
): ReadonlySet<string> {
  const out = new Set<string>();
  for (const value of localLets.values()) {
    collectIr1FreeVars(value, out);
  }
  return out;
}

function collectIr1FreeVars(
  expr: IR1Expr,
  out: Set<string>,
  bound: ReadonlySet<string> = new Set(),
): void {
  switch (expr.kind) {
    case "var":
      if (!expr.primed && !bound.has(expr.name)) {
        out.add(expr.name);
      }
      break;
    case "lit":
      break;
    case "binop":
      collectIr1FreeVars(expr.lhs, out, bound);
      collectIr1FreeVars(expr.rhs, out, bound);
      break;
    case "unop":
      collectIr1FreeVars(expr.arg, out, bound);
      break;
    case "app":
      collectIr1FreeVars(expr.callee, out, bound);
      for (const arg of expr.args) {
        collectIr1FreeVars(arg, out, bound);
      }
      break;
    case "member":
      collectIr1FreeVars(expr.receiver, out, bound);
      break;
    case "cond":
      for (const [guard, value] of expr.arms) {
        collectIr1FreeVars(guard, out, bound);
        collectIr1FreeVars(value, out, bound);
      }
      collectIr1FreeVars(expr.otherwise, out, bound);
      break;
    case "is-nullish":
      collectIr1FreeVars(expr.operand, out, bound);
      break;
    case "each": {
      collectIr1FreeVars(expr.src, out, bound);
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      for (const guard of expr.guards) {
        collectIr1FreeVars(guard, out, nextBound);
      }
      collectIr1FreeVars(expr.proj, out, nextBound);
      break;
    }
    case "comb-typed": {
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      for (const guard of expr.guards) {
        collectIr1FreeVars(guard, out, nextBound);
      }
      collectIr1FreeVars(expr.proj, out, nextBound);
      break;
    }
    case "forall":
    case "exists": {
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      if (expr.guard !== undefined) {
        collectIr1FreeVars(expr.guard, out, nextBound);
      }
      collectIr1FreeVars(expr.body, out, nextBound);
      break;
    }
    case "map-read":
      collectIr1FreeVars(expr.receiver, out, bound);
      collectIr1FreeVars(expr.key, out, bound);
      break;
    case "set-read":
      collectIr1FreeVars(expr.receiver, out, bound);
      collectIr1FreeVars(expr.elem, out, bound);
      break;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
    }
  }
}

function freshIr1Name(base: string, exprs: Iterable<IR1Expr>): string {
  const used = new Set<string>();
  for (const expr of exprs) {
    collectAllNames(expr, used);
  }
  let candidate = `${base}1`;
  for (let i = 2; used.has(candidate); i++) {
    candidate = `${base}${i}`;
  }
  return candidate;
}

function renameBoundVarRefs(expr: IR1Expr, from: string, to: string): IR1Expr {
  switch (expr.kind) {
    case "var":
      return !expr.primed && expr.name === from ? { ...expr, name: to } : expr;
    case "lit":
      return expr;
    case "binop":
      return {
        ...expr,
        lhs: renameBoundVarRefs(expr.lhs, from, to),
        rhs: renameBoundVarRefs(expr.rhs, from, to),
      };
    case "unop":
      return { ...expr, arg: renameBoundVarRefs(expr.arg, from, to) };
    case "app":
      return {
        ...expr,
        callee: renameBoundVarRefs(expr.callee, from, to),
        args: expr.args.map((arg) => renameBoundVarRefs(arg, from, to)),
      };
    case "member":
      return { ...expr, receiver: renameBoundVarRefs(expr.receiver, from, to) };
    case "cond":
      return {
        ...expr,
        arms: expr.arms.map(([guard, value]) => [
          renameBoundVarRefs(guard, from, to),
          renameBoundVarRefs(value, from, to),
        ]),
        otherwise: renameBoundVarRefs(expr.otherwise, from, to),
      };
    case "is-nullish":
      return { ...expr, operand: renameBoundVarRefs(expr.operand, from, to) };
    case "each":
      if (expr.binder === from) {
        return {
          ...expr,
          src: renameBoundVarRefs(expr.src, from, to),
        };
      }
      return {
        ...expr,
        src: renameBoundVarRefs(expr.src, from, to),
        guards: expr.guards.map((guard) => renameBoundVarRefs(guard, from, to)),
        proj: renameBoundVarRefs(expr.proj, from, to),
      };
    case "comb-typed":
      if (expr.binder === from) {
        return expr;
      }
      return {
        ...expr,
        guards: expr.guards.map((guard) => renameBoundVarRefs(guard, from, to)),
        proj: renameBoundVarRefs(expr.proj, from, to),
      };
    case "forall":
    case "exists":
      if (expr.binder === from) {
        return expr;
      }
      return {
        ...expr,
        ...(expr.guard === undefined
          ? {}
          : { guard: renameBoundVarRefs(expr.guard, from, to) }),
        body: renameBoundVarRefs(expr.body, from, to),
      };
    case "map-read":
      return {
        ...expr,
        receiver: renameBoundVarRefs(expr.receiver, from, to),
        key: renameBoundVarRefs(expr.key, from, to),
      };
    case "set-read":
      return {
        ...expr,
        receiver: renameBoundVarRefs(expr.receiver, from, to),
        elem: renameBoundVarRefs(expr.elem, from, to),
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
  const excluded = new Set<string>();
  const receiverRoot = rootName(target.receiver);
  if (receiverRoot !== null) {
    excluded.add(receiverRoot);
  }
  for (const expr of exprs) {
    collectFreeVars(expr, names, excluded);
  }
  return [...names].sort();
}

function stateAvoidanceNames(
  exprs: readonly IR1Expr[],
  target: Extract<IR1Expr, { kind: "member" }>,
): ReadonlySet<string> {
  const names = new Set<string>();
  const receiverRoot = rootName(target.receiver);
  if (receiverRoot !== null) {
    names.add(receiverRoot);
  }
  for (const expr of exprs) {
    collectAllNames(expr, names);
  }
  return names;
}

function collectAllNames(expr: IR1Expr, out: Set<string>): void {
  switch (expr.kind) {
    case "var":
      out.add(expr.name);
      break;
    case "lit":
      break;
    case "binop":
      collectAllNames(expr.lhs, out);
      collectAllNames(expr.rhs, out);
      break;
    case "unop":
      collectAllNames(expr.arg, out);
      break;
    case "app":
      collectAllNames(expr.callee, out);
      for (const arg of expr.args) {
        collectAllNames(arg, out);
      }
      break;
    case "member":
      collectAllNames(expr.receiver, out);
      break;
    case "cond":
      for (const [guard, value] of expr.arms) {
        collectAllNames(guard, out);
        collectAllNames(value, out);
      }
      collectAllNames(expr.otherwise, out);
      break;
    case "is-nullish":
      collectAllNames(expr.operand, out);
      break;
    case "each":
      out.add(expr.binder);
      collectAllNames(expr.src, out);
      for (const guard of expr.guards) {
        collectAllNames(guard, out);
      }
      collectAllNames(expr.proj, out);
      break;
    case "comb-typed":
      out.add(expr.binder);
      for (const guard of expr.guards) {
        collectAllNames(guard, out);
      }
      collectAllNames(expr.proj, out);
      break;
    case "forall":
    case "exists":
      out.add(expr.binder);
      if (expr.guard !== undefined) {
        collectAllNames(expr.guard, out);
      }
      collectAllNames(expr.body, out);
      break;
    case "map-read":
      collectAllNames(expr.receiver, out);
      collectAllNames(expr.key, out);
      break;
    case "set-read":
      collectAllNames(expr.receiver, out);
      collectAllNames(expr.elem, out);
      break;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
    }
  }
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
    case "comb-typed": {
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      for (const guard of expr.guards) {
        collectFreeVars(guard, out, excluded, nextBound);
      }
      collectFreeVars(expr.proj, out, excluded, nextBound);
      break;
    }
    case "forall":
    case "exists": {
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      if (expr.guard !== undefined) {
        collectFreeVars(expr.guard, out, excluded, nextBound);
      }
      collectFreeVars(expr.body, out, excluded, nextBound);
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

function containsNonTargetMemberRead(
  expr: IR1Expr,
  target: Extract<IR1Expr, { kind: "member" }>,
): boolean {
  let found = false;
  walkExpr(expr, (node, bound) => {
    if (
      node.kind === "member" &&
      !sameUnshadowedMemberExpr(node, target, bound)
    ) {
      found = true;
    }
  });
  return found;
}

function containsTargetMemberRead(
  expr: IR1Expr,
  target: Extract<IR1Expr, { kind: "member" }>,
): boolean {
  let found = false;
  walkExpr(expr, (node, bound) => {
    if (sameUnshadowedMemberExpr(node, target, bound)) {
      found = true;
    }
  });
  return found;
}

function walkExpr(
  expr: IR1Expr,
  visit: (expr: IR1Expr, bound: ReadonlySet<string>) => void,
  bound: ReadonlySet<string> = new Set(),
): void {
  visit(expr, bound);
  switch (expr.kind) {
    case "var":
    case "lit":
      break;
    case "binop":
      walkExpr(expr.lhs, visit, bound);
      walkExpr(expr.rhs, visit, bound);
      break;
    case "unop":
      walkExpr(expr.arg, visit, bound);
      break;
    case "app":
      walkExpr(expr.callee, visit, bound);
      for (const arg of expr.args) {
        walkExpr(arg, visit, bound);
      }
      break;
    case "member":
      walkExpr(expr.receiver, visit, bound);
      break;
    case "cond":
      for (const [guard, value] of expr.arms) {
        walkExpr(guard, visit, bound);
        walkExpr(value, visit, bound);
      }
      walkExpr(expr.otherwise, visit, bound);
      break;
    case "is-nullish":
      walkExpr(expr.operand, visit, bound);
      break;
    case "each": {
      walkExpr(expr.src, visit, bound);
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      for (const guard of expr.guards) {
        walkExpr(guard, visit, nextBound);
      }
      walkExpr(expr.proj, visit, nextBound);
      break;
    }
    case "comb-typed": {
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      for (const guard of expr.guards) {
        walkExpr(guard, visit, nextBound);
      }
      walkExpr(expr.proj, visit, nextBound);
      break;
    }
    case "forall":
    case "exists": {
      const nextBound = new Set(bound);
      nextBound.add(expr.binder);
      if (expr.guard !== undefined) {
        walkExpr(expr.guard, visit, nextBound);
      }
      walkExpr(expr.body, visit, nextBound);
      break;
    }
    case "map-read":
      walkExpr(expr.receiver, visit, bound);
      walkExpr(expr.key, visit, bound);
      break;
    case "set-read":
      walkExpr(expr.receiver, visit, bound);
      walkExpr(expr.elem, visit, bound);
      break;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
    }
  }
}

function sameUnshadowedMemberExpr(
  expr: IR1Expr,
  member: Extract<IR1Expr, { kind: "member" }>,
  bound: ReadonlySet<string>,
): boolean {
  return (
    expr.kind === "member" &&
    !memberReceiverRootIsBound(expr, bound) &&
    sameMemberExpr(expr, member)
  );
}

function memberReceiverRootIsBound(
  expr: Extract<IR1Expr, { kind: "member" }>,
  bound: ReadonlySet<string>,
): boolean {
  const root = rootName(expr.receiver);
  return root !== null && bound.has(root);
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
