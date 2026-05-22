import { lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1SsaJoin,
  type IR1SsaLocation,
  type IR1SsaProgram,
  type IR1SsaRead,
  type IR1SsaRuleName,
  type IR1SsaVersion,
  type IR1SsaWrite,
  type IR1Stmt,
  ir1SsaInitialVersion,
  ir1SsaJoin,
  ir1SsaLocalBindingLocation,
  ir1SsaLocalBindingValue,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRead,
  ir1SsaRuleOfLocation,
  ir1SsaWrite,
  ir1Var,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PropResult } from "./types.js";

export interface ScalarSsaState {
  currentVersions: Map<string, IR1SsaVersion>;
  initialVersions: Map<string, IR1SsaVersion>;
  locations: Map<string, IR1SsaLocation>;
  reads: IR1SsaRead[];
  writes: IR1SsaWrite[];
  joins: IR1SsaJoin[];
  writtenKeys: Set<string>;
  declaredRules: Set<string>;
  modifiedRules: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
}

export interface ScalarSsaBuildOptions {
  declaredRules?: Iterable<string>;
  canonicalize?: (e: IR1Expr) => IR1Expr;
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface ScalarSsaFinalPropertyEntry {
  location: Extract<IR1SsaLocation, { kind: "property" }>;
  version: IR1SsaVersion;
  objExpr: OpaqueExpr;
  lhs: OpaqueExpr;
  rhs: OpaqueExpr;
}

export interface ScalarSsaLowerResult {
  program: IR1SsaProgram;
  finalProperties: ScalarSsaFinalPropertyEntry[];
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: Array<Extract<PropResult, { kind: "unsupported" }>>;
}

export interface ScalarSsaEarlyExitPropertyInput {
  key: string;
  prop: string;
  objExpr: OpaqueExpr;
  priorValue: OpaqueExpr | undefined;
  continuationValue: OpaqueExpr;
}

export interface ScalarSsaEarlyExitMergeOptions {
  guardExpr: OpaqueExpr;
  earlyExitWhenTrue: boolean;
  canonicalize?: (e: OpaqueExpr) => OpaqueExpr;
}

interface ScalarSsaLocationState {
  locations: Map<string, IR1SsaLocation>;
  declaredRules?: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
}

interface ScalarSsaInitialVersionState {
  initialVersions: Map<string, IR1SsaVersion>;
}

export function makeScalarSsaState(
  options: ScalarSsaBuildOptions = {},
): ScalarSsaState {
  return {
    currentVersions: new Map(),
    initialVersions: new Map(),
    locations: new Map(),
    reads: [],
    writes: [],
    joins: [],
    writtenKeys: new Set(),
    declaredRules: new Set(options.declaredRules ?? []),
    modifiedRules: new Set(),
    canonicalize: options.canonicalize ?? ((e) => e),
  };
}

export function buildScalarSsaProgram(
  stmt: IR1Stmt,
  options: ScalarSsaBuildOptions = {},
): IR1SsaProgram {
  const state = makeScalarSsaState(options);
  lowerScalarSsaL1Body(stmt, state);
  const declaredRules = new Set(state.declaredRules);
  for (const rule of state.modifiedRules) {
    declaredRules.add(rule);
  }
  const framedRules = [...declaredRules].filter(
    (rule) => !state.modifiedRules.has(rule),
  );
  return {
    reads: state.reads,
    writes: state.writes,
    joins: state.joins,
    loopHeaderJoins: [],
    loopBodies: [],
    declaredRules: [...declaredRules],
    modifiedRules: [...state.modifiedRules],
    framedRules,
  };
}

export function lowerScalarSsaToProps(
  stmt: IR1Stmt,
  options: ScalarSsaBuildOptions = {},
): ScalarSsaLowerResult {
  if (!isScalarSsaL1Body(stmt)) {
    const reason = "statement is not supported by scalar SSA lowering";
    return {
      program: {
        reads: [],
        writes: [],
        joins: [],
        loopHeaderJoins: [],
        loopBodies: [],
        declaredRules: [...new Set(options.declaredRules ?? [])],
        modifiedRules: [],
        framedRules: [...new Set(options.declaredRules ?? [])],
      },
      finalProperties: [],
      propositions: [],
      modifiedRules: [],
      diagnostics: [{ kind: "unsupported", reason }],
    };
  }

  const program = buildScalarSsaProgram(stmt, options);
  const lowerState = makeScalarSsaLowerState(
    program,
    options.canonicalize,
    options.lowerOpaque,
    options.initialPropertyValues,
  );
  lowerScalarSsaStmtToVersions(stmt, lowerState);

  const ast = getAst();
  const finalProperties: ScalarSsaFinalPropertyEntry[] = [];
  const propositions: PropResult[] = [];
  for (const write of program.writes) {
    if (write.location.kind !== "local-binding") {
      continue;
    }
    const rhs = lowerState.versionExprs.get(write.version);
    if (rhs === undefined) {
      throw new Error(
        "missing lowered expression for local-binding SSA version",
      );
    }
    propositions.push({
      kind: "equation",
      quantifiers: [],
      lhs: ast.var(write.location.name),
      rhs,
    });
  }
  for (const [key, version] of lowerState.currentVersions) {
    const location = lowerState.locations.get(key);
    if (location === undefined || location.kind !== "property") {
      continue;
    }
    const rhs = lowerState.versionExprs.get(version);
    if (rhs === undefined) {
      throw new Error(
        "missing lowered expression for final scalar SSA version",
      );
    }
    const objExpr = lowerState.lowerOpaque(
      lowerScalarOpaqueExpr(location.receiver, lowerState.canonicalize),
    );
    const lhs = ast.app(ast.primed(location.ruleName), [objExpr]);
    finalProperties.push({ location, version, objExpr, lhs, rhs });
    propositions.push({
      kind: "equation",
      quantifiers: [],
      lhs,
      rhs,
    });
  }
  return {
    program,
    finalProperties,
    propositions,
    modifiedRules: [...program.modifiedRules],
    diagnostics: [],
  };
}

export function lowerScalarSsaEarlyExitMerge(
  inputs: readonly ScalarSsaEarlyExitPropertyInput[],
  options: ScalarSsaEarlyExitMergeOptions,
): ScalarSsaLowerResult {
  const ast = getAst();
  const canonicalize = options.canonicalize ?? ((e: OpaqueExpr) => e);
  const reads: IR1SsaRead[] = [];
  const writes: IR1SsaWrite[] = [];
  const joins: IR1SsaJoin[] = [];
  const finalProperties: ScalarSsaFinalPropertyEntry[] = [];
  const propositions: PropResult[] = [];
  const modifiedRules = new Set<IR1SsaRuleName>();

  for (const [index, input] of inputs.entries()) {
    const location = ir1SsaPropertyLocation(
      input.prop,
      ir1Var(`early_exit_${index}`),
      input.prop,
    ) as Extract<IR1SsaLocation, { kind: "property" }>;
    const initialVersion = ir1SsaInitialVersion(location);
    const initialValue = canonicalize(
      ast.app(ast.var(input.prop), [input.objExpr]),
    );

    let priorVersion = initialVersion;
    if (input.priorValue !== undefined) {
      const priorWrite = ir1SsaWrite(
        location,
        ir1SsaPropertyValue(ir1Var(`early_exit_prior_${index}`)),
      );
      writes.push(priorWrite);
      priorVersion = priorWrite.version;
    }

    const continuationWrite = ir1SsaWrite(
      location,
      ir1SsaPropertyValue(ir1Var(`early_exit_continuation_${index}`)),
    );
    writes.push(continuationWrite);

    const thenVersion = options.earlyExitWhenTrue
      ? priorVersion
      : continuationWrite.version;
    const elseVersion = options.earlyExitWhenTrue
      ? continuationWrite.version
      : priorVersion;
    const joined = ir1SsaJoin(location, thenVersion, elseVersion);
    const finalVersion =
      joined.kind === "ssa-join" ? joined.joinVersion : joined;
    if (joined.kind === "ssa-join") {
      joins.push(joined);
    }

    const priorValue = input.priorValue ?? initialValue;
    const armGuard = options.earlyExitWhenTrue
      ? ast.unop(ast.opNot(), options.guardExpr)
      : options.guardExpr;
    const rhs = canonicalize(
      ast.cond([
        [armGuard, input.continuationValue],
        [ast.litBool(true), priorValue],
      ]),
    );
    const lhs = ast.app(ast.primed(input.prop), [input.objExpr]);
    finalProperties.push({
      location,
      version: finalVersion,
      objExpr: input.objExpr,
      lhs,
      rhs,
    });
    propositions.push({
      kind: "equation",
      quantifiers: [],
      lhs,
      rhs,
    });
    modifiedRules.add(input.prop);
  }

  const program: IR1SsaProgram = {
    reads,
    writes,
    joins,
    loopHeaderJoins: [],
    loopBodies: [],
    declaredRules: [...modifiedRules],
    modifiedRules: [...modifiedRules],
    framedRules: [],
  };

  return {
    program,
    finalProperties,
    propositions,
    modifiedRules: [...modifiedRules],
    diagnostics: [],
  };
}

export function isScalarSsaL1Body(stmt: IR1Stmt): boolean {
  switch (stmt.kind) {
    case "assign":
      return (
        stmt.target.kind === "member" &&
        isScalarSsaExpr(stmt.target.receiver) &&
        isScalarSsaExpr(stmt.value)
      );
    case "block":
      return stmt.stmts.every(isScalarSsaL1Body);
    case "cond-stmt":
      return (
        stmt.arms.length === 1 &&
        stmt.arms.every(
          ([guard, body]) => isScalarSsaExpr(guard) && isScalarSsaL1Body(body),
        ) &&
        (stmt.otherwise === null || isScalarSsaL1Body(stmt.otherwise))
      );
    case "let":
      return isScalarSsaExpr(stmt.value);
    case "map-effect":
    case "set-effect":
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "break":
    case "continue":
    case "throw":
    case "expr-stmt":
      return false;
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      return false;
    }
  }
}

export function lowerScalarSsaL1Body(
  stmt: IR1Stmt,
  state: ScalarSsaState,
): void {
  switch (stmt.kind) {
    case "assign":
      lowerScalarAssign(stmt, state);
      return;
    case "block":
      for (const child of stmt.stmts) {
        lowerScalarSsaL1Body(child, state);
      }
      return;
    case "cond-stmt":
      lowerScalarCondStmt(stmt, state);
      return;
    case "let":
      lowerScalarLet(stmt, state);
      return;
    case "map-effect":
    case "set-effect":
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "break":
    case "continue":
    case "throw":
    case "expr-stmt":
      throw new Error(`${stmt.kind} is not supported by scalar SSA lowering`);
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      throw new Error("unsupported IR1 statement in scalar SSA lowering");
    }
  }
}

export function scalarSsaReadExpr(
  expr: IR1Expr,
  state: ScalarSsaState,
): IR1Expr {
  switch (expr.kind) {
    case "member":
      scalarSsaReadMember(expr, state);
      scalarSsaReadExpr(expr.receiver, state);
      return expr;
    case "binop":
      scalarSsaReadExpr(expr.lhs, state);
      scalarSsaReadExpr(expr.rhs, state);
      return expr;
    case "unop":
      scalarSsaReadExpr(expr.arg, state);
      return expr;
    case "app":
      scalarSsaReadExpr(expr.callee, state);
      for (const arg of expr.args) {
        scalarSsaReadExpr(arg, state);
      }
      return expr;
    case "cond":
      for (const [guard, value] of expr.arms) {
        scalarSsaReadExpr(guard, state);
        scalarSsaReadExpr(value, state);
      }
      scalarSsaReadExpr(expr.otherwise, state);
      return expr;
    case "is-nullish":
      scalarSsaReadExpr(expr.operand, state);
      return expr;
    case "each":
      scalarSsaReadExpr(expr.src, state);
      for (const guard of expr.guards) {
        scalarSsaReadExpr(guard, state);
      }
      scalarSsaReadExpr(expr.proj, state);
      return expr;
    case "comb-typed":
      for (const guard of expr.guards) {
        scalarSsaReadExpr(guard, state);
      }
      scalarSsaReadExpr(expr.proj, state);
      return expr;
    case "forall":
    case "exists":
      if (expr.guard !== undefined) {
        scalarSsaReadExpr(expr.guard, state);
      }
      scalarSsaReadExpr(expr.body, state);
      return expr;
    case "var":
      scalarSsaReadLocalBinding(expr.name, state);
      return expr;
    case "lit":
      return expr;
    case "map-read":
    case "set-read":
      throw new Error(`${expr.kind} is not a scalar SSA expression`);
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return expr;
    }
  }
}

function lowerScalarAssign(
  stmt: Extract<IR1Stmt, { kind: "assign" }>,
  state: ScalarSsaState,
): void {
  if (stmt.target.kind !== "member") {
    throw new Error("scalar SSA assignment target must be a property member");
  }
  const value = scalarSsaReadExpr(stmt.value, state);
  const { key, location } = scalarLocationForMember(stmt.target, state);
  const write = ir1SsaWrite(location, ir1SsaPropertyValue(value));
  state.writes.push(write);
  state.currentVersions.set(key, write.version);
  state.writtenKeys.add(key);
  state.modifiedRules.add(ir1SsaRuleOfLocation(location));
}

function lowerScalarLet(
  stmt: Extract<IR1Stmt, { kind: "let" }>,
  state: ScalarSsaState,
): void {
  const value = scalarSsaReadExpr(stmt.value, state);
  const location = ir1SsaLocalBindingLocation(stmt.name);
  const key = scalarLocalBindingKey(stmt.name);
  state.locations.set(key, location);
  initialVersionFor(key, location, state);
  const write = ir1SsaWrite(location, ir1SsaLocalBindingValue(value));
  state.writes.push(write);
  state.currentVersions.set(key, write.version);
}

interface ScalarSsaLowerState {
  writes: readonly IR1SsaWrite[];
  joins: readonly IR1SsaJoin[];
  writeIndex: number;
  joinIndex: number;
  currentVersions: Map<string, IR1SsaVersion>;
  initialVersions: Map<string, IR1SsaVersion>;
  locations: Map<string, IR1SsaLocation>;
  versionExprs: Map<IR1SsaVersion, OpaqueExpr>;
  writtenKeys: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues: ReadonlyMap<string, OpaqueExpr>;
}

function makeScalarSsaLowerState(
  program: IR1SsaProgram,
  canonicalize?: (e: IR1Expr) => IR1Expr,
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr,
  initialPropertyValues: ReadonlyMap<string, OpaqueExpr> = new Map(),
): ScalarSsaLowerState {
  return {
    writes: program.writes,
    joins: program.joins,
    writeIndex: 0,
    joinIndex: 0,
    currentVersions: new Map(),
    initialVersions: new Map(),
    locations: new Map(),
    versionExprs: new Map(),
    writtenKeys: new Set(),
    canonicalize: canonicalize ?? ((e) => e),
    lowerOpaque: lowerOpaque ?? ((e) => e),
    initialPropertyValues,
  };
}

function lowerScalarSsaStmtToVersions(
  stmt: IR1Stmt,
  state: ScalarSsaLowerState,
): void {
  switch (stmt.kind) {
    case "assign":
      lowerScalarSsaAssignToVersions(stmt, state);
      return;
    case "block":
      for (const child of stmt.stmts) {
        lowerScalarSsaStmtToVersions(child, state);
      }
      return;
    case "cond-stmt":
      lowerScalarSsaCondToVersions(stmt, state);
      return;
    case "let":
      lowerScalarSsaLetToVersions(stmt, state);
      return;
    case "map-effect":
    case "set-effect":
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "break":
    case "continue":
    case "throw":
    case "expr-stmt":
      throw new Error(`${stmt.kind} is not supported by scalar SSA lowering`);
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      throw new Error("unsupported IR1 statement in scalar SSA lowering");
    }
  }
}

function lowerScalarSsaAssignToVersions(
  stmt: Extract<IR1Stmt, { kind: "assign" }>,
  state: ScalarSsaLowerState,
): void {
  if (stmt.target.kind !== "member") {
    throw new Error("scalar SSA assignment target must be a property member");
  }
  const write = nextScalarSsaWrite(state, stmt.target.name);
  if (write.location.kind !== "property") {
    throw new Error("scalar SSA write lowered to a non-property location");
  }
  const { key, location } = scalarLocationForMember(stmt.target, state);
  if (!sameScalarSsaLocation(write.location, location, state.canonicalize)) {
    throw new Error(
      "scalar SSA write order did not match its property location",
    );
  }
  const rhs = lowerScalarSsaExprToOpaque(stmt.value, state);
  state.currentVersions.set(key, write.version);
  state.locations.set(key, location);
  state.versionExprs.set(write.version, rhs);
  state.writtenKeys.add(key);
}

function lowerScalarSsaLetToVersions(
  stmt: Extract<IR1Stmt, { kind: "let" }>,
  state: ScalarSsaLowerState,
): void {
  const write = nextScalarSsaWrite(state);
  const location = ir1SsaLocalBindingLocation(stmt.name);
  if (!sameScalarSsaLocation(write.location, location, state.canonicalize)) {
    throw new Error(
      "scalar SSA write order did not match its local-binding location",
    );
  }
  const rhs = lowerScalarSsaExprToOpaque(stmt.value, state);
  const key = scalarLocalBindingKey(stmt.name);
  state.locations.set(key, location);
  initialVersionFor(key, location, state);
  state.currentVersions.set(key, write.version);
  state.versionExprs.set(write.version, rhs);
}

function lowerScalarSsaCondToVersions(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: ScalarSsaLowerState,
): void {
  if (stmt.arms.length !== 1) {
    throw new Error("multi-armed cond-stmt is not supported by scalar SSA");
  }
  const ast = getAst();
  const [guard, thenBody] = stmt.arms[0]!;
  const guardExpr = lowerScalarSsaExprToOpaque(guard, state);
  const thenState = cloneScalarSsaLowerStateForBranch(state);
  lowerScalarSsaStmtToVersions(thenBody, thenState);

  const elseState = cloneScalarSsaLowerStateForBranch(state);
  elseState.writeIndex = thenState.writeIndex;
  elseState.joinIndex = thenState.joinIndex;
  if (stmt.otherwise !== null) {
    lowerScalarSsaStmtToVersions(stmt.otherwise, elseState);
  }

  state.writeIndex = elseState.writeIndex;
  state.joinIndex = elseState.joinIndex;

  const touched = new Set([...thenState.writtenKeys, ...elseState.writtenKeys]);
  for (const key of touched) {
    const location =
      thenState.locations.get(key) ??
      elseState.locations.get(key) ??
      state.locations.get(key);
    if (location === undefined) {
      throw new Error("scalar SSA branch touched an unknown location");
    }
    const thenVersion =
      thenState.currentVersions.get(key) ??
      initialVersionFor(key, location, thenState);
    const elseVersion =
      elseState.currentVersions.get(key) ??
      initialVersionFor(key, location, elseState);
    if (thenVersion === elseVersion) {
      state.currentVersions.set(key, thenVersion);
      state.locations.set(key, location);
      state.writtenKeys.add(key);
      continue;
    }
    const join = nextScalarSsaJoin(state, location);
    if (
      !sameScalarSsaVersion(
        join.thenVersion,
        thenVersion,
        state.canonicalize,
      ) ||
      !sameScalarSsaVersion(
        join.elseVersion,
        elseVersion,
        state.canonicalize,
      ) ||
      !sameScalarSsaLocation(join.location, location, state.canonicalize)
    ) {
      throw new Error("scalar SSA join order did not match branch versions");
    }
    const thenExpr = resolveScalarSsaVersionExpr(thenVersion, thenState);
    const elseExpr = resolveScalarSsaVersionExpr(elseVersion, elseState);
    const joinExpr = state.lowerOpaque(
      ast.cond([
        [guardExpr, thenExpr],
        [ast.litBool(true), elseExpr],
      ]),
    );
    state.currentVersions.set(key, join.joinVersion);
    state.locations.set(key, location);
    state.versionExprs.set(join.joinVersion, joinExpr);
    state.writtenKeys.add(key);
  }
}

function lowerScalarSsaExprToOpaque(
  expr: IR1Expr,
  state: ScalarSsaLowerState,
): OpaqueExpr {
  const ast = getAst();
  switch (expr.kind) {
    case "member": {
      const { key, location } = scalarLocationForMember(expr, state);
      const version =
        state.currentVersions.get(key) ??
        initialVersionFor(key, location, state);
      return resolveScalarSsaVersionExpr(version, state);
    }
    case "var": {
      const local = scalarSsaLocalBindingVersion(expr.name, state);
      if (local !== null) {
        return resolveScalarSsaVersionExpr(local.version, state);
      }
      return state.lowerOpaque(lowerScalarOpaqueExpr(expr, state.canonicalize));
    }
    case "lit":
      return state.lowerOpaque(lowerScalarOpaqueExpr(expr, state.canonicalize));
    case "binop":
      return state.lowerOpaque(
        ast.binop(
          lowerScalarBinop(expr.op),
          lowerScalarSsaExprToOpaque(expr.lhs, state),
          lowerScalarSsaExprToOpaque(expr.rhs, state),
        ),
      );
    case "unop":
      return state.lowerOpaque(
        ast.unop(
          lowerScalarUnop(expr.op),
          lowerScalarSsaExprToOpaque(expr.arg, state),
        ),
      );
    case "app": {
      const callee = lowerScalarSsaExprToOpaque(expr.callee, state);
      const args = expr.args.map((arg) =>
        lowerScalarSsaExprToOpaque(arg, state),
      );
      return state.lowerOpaque(ast.app(callee, args));
    }
    case "cond": {
      const arms: Array<[OpaqueExpr, OpaqueExpr]> = expr.arms.map(
        ([armGuard, value]) => [
          lowerScalarSsaExprToOpaque(armGuard, state),
          lowerScalarSsaExprToOpaque(value, state),
        ],
      );
      return state.lowerOpaque(
        ast.cond([
          ...arms,
          [
            ast.litBool(true),
            lowerScalarSsaExprToOpaque(expr.otherwise, state),
          ],
        ]),
      );
    }
    case "is-nullish":
      return state.lowerOpaque(
        ast.binop(
          ast.opEq(),
          ast.unop(
            ast.opCard(),
            lowerScalarSsaExprToOpaque(expr.operand, state),
          ),
          ast.litNat(0),
        ),
      );
    case "each":
      return state.lowerOpaque(
        ast.each(
          [],
          [
            ast.gIn(expr.binder, lowerScalarSsaExprToOpaque(expr.src, state)),
            ...expr.guards.map((guard) =>
              ast.gExpr(lowerScalarSsaExprToOpaque(guard, state)),
            ),
          ],
          lowerScalarSsaExprToOpaque(expr.proj, state),
        ),
      );
    case "comb-typed":
      return state.lowerOpaque(
        ast.eachComb(
          [ast.param(expr.binder, ast.tName(expr.binderType))],
          expr.guards.map((guard) =>
            ast.gExpr(lowerScalarSsaExprToOpaque(guard, state)),
          ),
          expr.combiner === "min" ? ast.combMin() : ast.combMax(),
          lowerScalarSsaExprToOpaque(expr.proj, state),
        ),
      );
    case "forall": {
      const guards =
        expr.guard === undefined
          ? []
          : [ast.gExpr(lowerScalarSsaExprToOpaque(expr.guard, state))];
      return state.lowerOpaque(
        ast.forall(
          [ast.param(expr.binder, ast.tName(expr.binderType))],
          guards,
          lowerScalarSsaExprToOpaque(expr.body, state),
        ),
      );
    }
    case "exists": {
      const guards =
        expr.guard === undefined
          ? []
          : [ast.gExpr(lowerScalarSsaExprToOpaque(expr.guard, state))];
      return state.lowerOpaque(
        ast.exists(
          [ast.param(expr.binder, ast.tName(expr.binderType))],
          guards,
          lowerScalarSsaExprToOpaque(expr.body, state),
        ),
      );
    }
    case "map-read":
    case "set-read":
      throw new Error(`${expr.kind} is not a scalar SSA expression`);
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      throw new Error("unsupported IR1 expression in scalar SSA lowering");
    }
  }
}

function resolveScalarSsaVersionExpr(
  version: IR1SsaVersion,
  state: Pick<
    ScalarSsaLowerState,
    "versionExprs" | "canonicalize" | "lowerOpaque" | "initialPropertyValues"
  >,
): OpaqueExpr {
  const existing = state.versionExprs.get(version);
  if (existing !== undefined) {
    return existing;
  }
  if (version.origin !== "initial" || version.location.kind !== "property") {
    throw new Error("scalar SSA lowering expected a property initial version");
  }
  const receiver = state.lowerOpaque(
    lowerScalarOpaqueExpr(version.location.receiver, state.canonicalize),
  );
  const initialKey = scalarOpaquePropertyKey(
    version.location.ruleName,
    receiver,
  );
  const initialValue = state.initialPropertyValues.get(initialKey);
  if (initialValue !== undefined) {
    const loweredInitialValue = state.lowerOpaque(initialValue);
    state.versionExprs.set(version, loweredInitialValue);
    return loweredInitialValue;
  }
  const ast = getAst();
  const identity = state.lowerOpaque(
    ast.app(ast.var(version.location.ruleName), [receiver]),
  );
  state.versionExprs.set(version, identity);
  return identity;
}

function cloneScalarSsaLowerStateForBranch(
  state: ScalarSsaLowerState,
): ScalarSsaLowerState {
  return {
    writes: state.writes,
    joins: state.joins,
    writeIndex: state.writeIndex,
    joinIndex: state.joinIndex,
    currentVersions: new Map(state.currentVersions),
    initialVersions: state.initialVersions,
    locations: new Map(state.locations),
    versionExprs: new Map(state.versionExprs),
    writtenKeys: new Set(),
    canonicalize: state.canonicalize,
    lowerOpaque: state.lowerOpaque,
    initialPropertyValues: state.initialPropertyValues,
  };
}

function nextScalarSsaWrite(
  state: ScalarSsaLowerState,
  expectedRule?: string,
): IR1SsaWrite {
  const write = state.writes[state.writeIndex];
  if (write === undefined) {
    throw new Error("scalar SSA lowering ran out of writes");
  }
  state.writeIndex += 1;
  if (
    expectedRule !== undefined &&
    write.location.kind !== "local-binding" &&
    ir1SsaRuleOfLocation(write.location) !== expectedRule
  ) {
    throw new Error("scalar SSA write sequence did not match the source order");
  }
  return write;
}

function nextScalarSsaJoin(
  state: ScalarSsaLowerState,
  location: IR1SsaLocation,
): IR1SsaJoin {
  const join = state.joins[state.joinIndex];
  if (join === undefined) {
    throw new Error("scalar SSA lowering ran out of joins");
  }
  state.joinIndex += 1;
  if (!sameScalarSsaLocation(join.location, location, state.canonicalize)) {
    throw new Error("scalar SSA join sequence did not match the source order");
  }
  return join;
}

function lowerScalarCondStmt(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: ScalarSsaState,
): void {
  if (stmt.arms.length !== 1) {
    throw new Error("multi-armed cond-stmt is not supported by scalar SSA");
  }
  const [guard, thenBody] = stmt.arms[0]!;
  scalarSsaReadExpr(guard, state);

  const thenState = cloneScalarSsaStateForBranch(state);
  lowerScalarSsaL1Body(thenBody, thenState);

  const elseState = cloneScalarSsaStateForBranch(state);
  if (stmt.otherwise !== null) {
    lowerScalarSsaL1Body(stmt.otherwise, elseState);
  }

  state.reads.push(...thenState.reads, ...elseState.reads);
  state.writes.push(...thenState.writes, ...elseState.writes);
  state.joins.push(...thenState.joins, ...elseState.joins);
  for (const [key, location] of thenState.locations) {
    if (location.kind === "local-binding") {
      continue;
    }
    state.locations.set(key, location);
  }
  for (const [key, location] of elseState.locations) {
    if (location.kind === "local-binding") {
      continue;
    }
    if (!state.locations.has(key)) {
      state.locations.set(key, location);
    }
  }
  for (const rule of thenState.declaredRules) {
    state.declaredRules.add(rule);
  }
  for (const rule of elseState.declaredRules) {
    state.declaredRules.add(rule);
  }
  for (const rule of thenState.modifiedRules) {
    state.modifiedRules.add(rule);
  }
  for (const rule of elseState.modifiedRules) {
    state.modifiedRules.add(rule);
  }

  const touched = new Set([...thenState.writtenKeys, ...elseState.writtenKeys]);
  for (const key of touched) {
    const location =
      thenState.locations.get(key) ??
      elseState.locations.get(key) ??
      state.locations.get(key);
    if (location === undefined) {
      throw new Error("scalar SSA branch touched an unknown location");
    }
    const thenVersion =
      thenState.currentVersions.get(key) ??
      initialVersionFor(key, location, state);
    const elseVersion =
      elseState.currentVersions.get(key) ??
      initialVersionFor(key, location, state);
    const join = ir1SsaJoin(location, thenVersion, elseVersion);
    if (join.kind === "ssa-join") {
      state.joins.push(join);
      state.currentVersions.set(key, join.joinVersion);
    } else {
      state.currentVersions.set(key, join);
    }
    state.writtenKeys.add(key);
  }
}

function cloneScalarSsaStateForBranch(state: ScalarSsaState): ScalarSsaState {
  return {
    currentVersions: new Map(state.currentVersions),
    initialVersions: state.initialVersions,
    locations: new Map(state.locations),
    reads: [],
    writes: [],
    joins: [],
    writtenKeys: new Set(),
    declaredRules: new Set(state.declaredRules),
    modifiedRules: new Set(),
    canonicalize: state.canonicalize,
  };
}

function scalarSsaReadMember(
  member: Extract<IR1Expr, { kind: "member" }>,
  state: ScalarSsaState,
): IR1SsaRead {
  const { key, location } = scalarLocationForMember(member, state);
  const version =
    state.currentVersions.get(key) ?? initialVersionFor(key, location, state);
  const read = ir1SsaRead(location, version, true);
  state.reads.push(read);
  return read;
}

function scalarSsaReadLocalBinding(
  name: string,
  state: ScalarSsaState,
): IR1SsaRead | null {
  const local = scalarSsaLocalBindingVersion(name, state);
  if (local === null) {
    return null;
  }
  const read = ir1SsaRead(local.location, local.version, true);
  state.reads.push(read);
  return read;
}

function scalarSsaLocalBindingVersion(
  name: string,
  state: ScalarSsaInitialVersionState & {
    currentVersions: Map<string, IR1SsaVersion>;
    locations: Map<string, IR1SsaLocation>;
  },
): { key: string; location: IR1SsaLocation; version: IR1SsaVersion } | null {
  const key = scalarLocalBindingKey(name);
  const location = state.locations.get(key);
  if (location === undefined) {
    return null;
  }
  if (location.kind !== "local-binding") {
    throw new Error("local binding key resolved to a non-local SSA location");
  }
  const version =
    state.currentVersions.get(key) ?? initialVersionFor(key, location, state);
  return { key, location, version };
}

function scalarLocationForMember(
  member: Extract<IR1Expr, { kind: "member" }>,
  state: ScalarSsaLocationState,
): { key: string; location: IR1SsaLocation } {
  const receiver = scalarSsaCanonicalReceiverKey(member.receiver, state);
  const key = scalarPropertyKey(member.name, receiver);
  const existing = state.locations.get(key);
  if (existing !== undefined) {
    return { key, location: existing };
  }
  const location = ir1SsaPropertyLocation(
    member.name,
    member.receiver,
    member.name,
  );
  state.locations.set(key, location);
  state.declaredRules?.add(ir1SsaRuleOfLocation(location));
  return { key, location };
}

function initialVersionFor(
  key: string,
  location: IR1SsaLocation,
  state: ScalarSsaInitialVersionState,
): IR1SsaVersion {
  const existing = state.initialVersions.get(key);
  if (existing !== undefined) {
    return existing;
  }
  const initial = ir1SsaInitialVersion(location);
  state.initialVersions.set(key, initial);
  return initial;
}

function scalarPropertyKey(prop: string, receiver: string): string {
  return `${prop}::${receiver}`;
}

function scalarLocalBindingKey(name: string): string {
  return `local-binding::${name}`;
}

function scalarOpaquePropertyKey(prop: string, receiver: OpaqueExpr): string {
  return `${prop}::${getAst().strExpr(receiver)}`;
}

function scalarSsaCanonicalReceiverKey(
  receiver: IR1Expr,
  state: Pick<ScalarSsaLocationState, "canonicalize">,
): string {
  try {
    return scalarSsaExprKey(state.canonicalize(receiver));
  } catch (err) {
    if (err instanceof Error) {
      return scalarSsaExprKey(receiver);
    }
    throw err;
  }
}

function scalarSsaExprKey(expr: IR1Expr): string {
  switch (expr.kind) {
    case "var":
      return `var:${expr.name}:${expr.primed ?? false}`;
    case "lit":
      return `lit:${expr.value.kind}:${"value" in expr.value ? expr.value.value : ""}`;
    case "member":
      return `member:${scalarSsaExprKey(expr.receiver)}:${expr.name}`;
    case "binop":
      return `binop:${expr.op}:${scalarSsaExprKey(expr.lhs)}:${scalarSsaExprKey(expr.rhs)}`;
    case "unop":
      return `unop:${expr.op}:${scalarSsaExprKey(expr.arg)}`;
    case "app":
      return `app:${scalarSsaExprKey(expr.callee)}(${expr.args
        .map(scalarSsaExprKey)
        .join(",")})`;
    case "cond":
      return `cond:${expr.arms
        .map(
          ([guard, value]) =>
            `${scalarSsaExprKey(guard)}=>${scalarSsaExprKey(value)}`,
        )
        .join("|")}:${scalarSsaExprKey(expr.otherwise)}`;
    case "is-nullish":
      return `is-nullish:${scalarSsaExprKey(expr.operand)}`;
    case "each":
      return `each:${expr.binder}:${scalarSsaExprKey(expr.src)}:${expr.guards
        .map(scalarSsaExprKey)
        .join(",")}:${scalarSsaExprKey(expr.proj)}`;
    case "comb-typed":
      return `comb-typed:${expr.combiner}:${expr.binder}:${expr.binderType}:${expr.guards
        .map(scalarSsaExprKey)
        .join(",")}:${scalarSsaExprKey(expr.proj)}`;
    case "forall":
    case "exists":
      return `${expr.kind}:${expr.binder}:${expr.binderType}:${
        expr.guard === undefined ? "" : scalarSsaExprKey(expr.guard)
      }:${scalarSsaExprKey(expr.body)}`;
    case "map-read":
    case "set-read":
      throw new Error(`${expr.kind} is not a scalar SSA expression`);
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return "";
    }
  }
}

function lowerScalarOpaqueExpr(
  expr: IR1Expr,
  canonicalize: (e: IR1Expr) => IR1Expr,
): OpaqueExpr {
  return lowerExpr(lowerL1Expr(canonicalize(expr)));
}

function sameScalarSsaLocation(
  left: IR1SsaLocation,
  right: IR1SsaLocation,
  canonicalize: (e: IR1Expr) => IR1Expr,
): boolean {
  if (left.kind !== right.kind) {
    return false;
  }
  switch (left.kind) {
    case "property":
      return (
        right.kind === "property" &&
        left.ruleName === right.ruleName &&
        left.property === right.property &&
        scalarSsaCanonicalReceiverKey(left.receiver, { canonicalize }) ===
          scalarSsaCanonicalReceiverKey(right.receiver, { canonicalize })
      );
    case "map-value":
      return (
        right.kind === "map-value" &&
        left.ruleName === right.ruleName &&
        left.ownerType === right.ownerType &&
        left.keyType === right.keyType &&
        scalarSsaCanonicalReceiverKey(left.receiver, { canonicalize }) ===
          scalarSsaCanonicalReceiverKey(right.receiver, { canonicalize }) &&
        scalarSsaCanonicalReceiverKey(left.key, { canonicalize }) ===
          scalarSsaCanonicalReceiverKey(right.key, { canonicalize })
      );
    case "map-membership":
      return (
        right.kind === "map-membership" &&
        left.ruleName === right.ruleName &&
        left.keyPredName === right.keyPredName &&
        left.ownerType === right.ownerType &&
        left.keyType === right.keyType &&
        scalarSsaCanonicalReceiverKey(left.receiver, { canonicalize }) ===
          scalarSsaCanonicalReceiverKey(right.receiver, { canonicalize }) &&
        scalarSsaCanonicalReceiverKey(left.key, { canonicalize }) ===
          scalarSsaCanonicalReceiverKey(right.key, { canonicalize })
      );
    case "set-membership":
      return (
        right.kind === "set-membership" &&
        left.ruleName === right.ruleName &&
        left.ownerType === right.ownerType &&
        left.elemType === right.elemType &&
        scalarSsaCanonicalReceiverKey(left.receiver, { canonicalize }) ===
          scalarSsaCanonicalReceiverKey(right.receiver, { canonicalize })
      );
    case "return-value":
      return right.kind === "return-value" && left.ruleName === right.ruleName;
    case "local-binding":
      return right.kind === "local-binding" && left.name === right.name;
    default: {
      const _exhaustive: never = left;
      void _exhaustive;
      return false;
    }
  }
}

function sameScalarSsaVersion(
  left: IR1SsaVersion,
  right: IR1SsaVersion,
  canonicalize: (e: IR1Expr) => IR1Expr,
): boolean {
  return (
    left.origin === right.origin &&
    sameScalarSsaLocation(left.location, right.location, canonicalize)
  );
}

function lowerScalarBinop(op: Extract<IR1Expr, { kind: "binop" }>["op"]) {
  const ast = getAst();
  switch (op) {
    case "add":
      return ast.opAdd();
    case "sub":
      return ast.opSub();
    case "mul":
      return ast.opMul();
    case "div":
      return ast.opDiv();
    case "eq":
      return ast.opEq();
    case "neq":
      return ast.opNeq();
    case "lt":
      return ast.opLt();
    case "le":
      return ast.opLe();
    case "gt":
      return ast.opGt();
    case "ge":
      return ast.opGe();
    case "and":
      return ast.opAnd();
    case "or":
      return ast.opOr();
    case "in":
      return ast.opIn();
    default:
      throw new Error(`unsupported scalar SSA binop: ${String(op)}`);
  }
}

function lowerScalarUnop(op: Extract<IR1Expr, { kind: "unop" }>["op"]) {
  const ast = getAst();
  switch (op) {
    case "not":
      return ast.opNot();
    case "neg":
      return ast.opNeg();
    case "card":
      return ast.opCard();
    default:
      throw new Error(`unsupported scalar SSA unop: ${String(op)}`);
  }
}

function isScalarSsaExpr(expr: IR1Expr): boolean {
  switch (expr.kind) {
    case "var":
    case "lit":
      return true;
    case "member":
      return isScalarSsaExpr(expr.receiver);
    case "binop":
      return isScalarSsaExpr(expr.lhs) && isScalarSsaExpr(expr.rhs);
    case "unop":
      return isScalarSsaExpr(expr.arg);
    case "app":
      return isScalarSsaExpr(expr.callee) && expr.args.every(isScalarSsaExpr);
    case "cond":
      return (
        expr.arms.every(
          ([guard, value]) => isScalarSsaExpr(guard) && isScalarSsaExpr(value),
        ) && isScalarSsaExpr(expr.otherwise)
      );
    case "is-nullish":
      return isScalarSsaExpr(expr.operand);
    case "each":
      return (
        isScalarSsaExpr(expr.src) &&
        expr.guards.every(isScalarSsaExpr) &&
        isScalarSsaExpr(expr.proj)
      );
    case "comb-typed":
      return expr.guards.every(isScalarSsaExpr) && isScalarSsaExpr(expr.proj);
    case "forall":
    case "exists":
      return (
        (expr.guard === undefined || isScalarSsaExpr(expr.guard)) &&
        isScalarSsaExpr(expr.body)
      );
    case "map-read":
    case "set-read":
      return false;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return false;
    }
  }
}
