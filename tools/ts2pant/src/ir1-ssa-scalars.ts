import {
  type IR1Expr,
  type IR1SsaJoin,
  type IR1SsaLocation,
  type IR1SsaProgram,
  type IR1SsaRead,
  type IR1SsaVersion,
  type IR1SsaWrite,
  type IR1Stmt,
  ir1SsaInitialVersion,
  ir1SsaJoin,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRead,
  ir1SsaRuleOfLocation,
  ir1SsaWrite,
} from "./ir1.js";

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
    loopSummaries: [],
    declaredRules: [...declaredRules],
    modifiedRules: [...state.modifiedRules],
    framedRules,
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
    case "map-effect":
    case "set-effect":
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "throw":
    case "let":
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
    case "map-effect":
    case "set-effect":
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "throw":
    case "let":
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
    case "var":
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
    state.locations.set(key, location);
  }
  for (const [key, location] of elseState.locations) {
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

function scalarLocationForMember(
  member: Extract<IR1Expr, { kind: "member" }>,
  state: ScalarSsaState,
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
  state.declaredRules.add(ir1SsaRuleOfLocation(location));
  return { key, location };
}

function initialVersionFor(
  key: string,
  location: IR1SsaLocation,
  state: ScalarSsaState,
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

function scalarSsaCanonicalReceiverKey(
  receiver: IR1Expr,
  state: ScalarSsaState,
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
