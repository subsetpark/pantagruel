import { lowerBinop, lowerExpr } from "./ir-emit.js";
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
  ir1SsaMapMembershipLocation,
  ir1SsaMapMembershipValue,
  ir1SsaMapSetValue,
  ir1SsaMapValueLocation,
  ir1SsaRead,
  ir1SsaRuleOfLocation,
  ir1SsaSetClearValue,
  ir1SsaSetMembershipLocation,
  ir1SsaSetMembershipValue,
  ir1SsaWrite,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr, OpaqueParam } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PropResult } from "./types.js";

export type CollectionSsaLocation =
  | Extract<IR1SsaLocation, { kind: "map-value" }>
  | Extract<IR1SsaLocation, { kind: "map-membership" }>
  | Extract<IR1SsaLocation, { kind: "set-membership" }>;

export interface CollectionSsaState {
  currentVersions: Map<string, IR1SsaVersion>;
  initialVersions: Map<string, IR1SsaVersion>;
  locations: Map<string, CollectionSsaLocation>;
  reads: IR1SsaRead[];
  writes: IR1SsaWrite[];
  joins: IR1SsaJoin[];
  diagnostics: Array<Extract<PropResult, { kind: "unsupported" }>>;
  writtenKeys: Set<string>;
  declaredRules: Set<string>;
  modifiedRules: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
}

export interface CollectionSsaBuildOptions {
  declaredRules?: Iterable<string>;
  canonicalize?: (e: IR1Expr) => IR1Expr;
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
}

export interface CollectionSsaFinalMapValueEntry {
  kind: "map-value";
  location: Extract<IR1SsaLocation, { kind: "map-value" }>;
  version: IR1SsaVersion;
  keyTuple?: OpaqueExpr;
  value?: OpaqueExpr;
}

export interface CollectionSsaFinalMapMembershipEntry {
  kind: "map-membership";
  location: Extract<IR1SsaLocation, { kind: "map-membership" }>;
  version: IR1SsaVersion;
  keyTuple?: OpaqueExpr;
  value?: OpaqueExpr;
}

export interface CollectionSsaFinalSetMembershipEntry {
  kind: "set-membership";
  location: Extract<IR1SsaLocation, { kind: "set-membership" }>;
  version: IR1SsaVersion;
}

export type CollectionSsaFinalEntry =
  | CollectionSsaFinalMapValueEntry
  | CollectionSsaFinalMapMembershipEntry
  | CollectionSsaFinalSetMembershipEntry;

export interface CollectionSsaLowerResult {
  program: IR1SsaProgram;
  finalEntries: CollectionSsaFinalEntry[];
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: Array<Extract<PropResult, { kind: "unsupported" }>>;
}

export interface CollectionSsaUnsupportedResult {
  unsupported: string;
  diagnostics: Array<Extract<PropResult, { kind: "unsupported" }>>;
}

export type CollectionSsaProgramBuildResult =
  | IR1SsaProgram
  | CollectionSsaUnsupportedResult;

interface CollectionSsaLocationState {
  locations: Map<string, CollectionSsaLocation>;
  declaredRules?: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
}

interface CollectionSsaInitialVersionState {
  initialVersions: Map<string, IR1SsaVersion>;
}

export function makeCollectionSsaState(
  options: CollectionSsaBuildOptions = {},
): CollectionSsaState {
  return {
    currentVersions: new Map(),
    initialVersions: new Map(),
    locations: new Map(),
    reads: [],
    writes: [],
    joins: [],
    diagnostics: [],
    writtenKeys: new Set(),
    declaredRules: new Set(options.declaredRules ?? []),
    modifiedRules: new Set(),
    canonicalize: options.canonicalize ?? ((e) => e),
  };
}

export function buildCollectionSsaProgram(
  stmt: IR1Stmt,
  options: CollectionSsaBuildOptions = {},
): CollectionSsaProgramBuildResult {
  const state = makeCollectionSsaState(options);
  lowerCollectionSsaL1Body(stmt, state);
  if (state.diagnostics.length > 0) {
    return {
      unsupported: state.diagnostics[0]!.reason,
      diagnostics: state.diagnostics,
    };
  }
  return collectionSsaProgramFromState(state);
}

export function lowerCollectionSsaToResult(
  stmt: IR1Stmt,
  options: CollectionSsaBuildOptions = {},
): CollectionSsaLowerResult {
  const state = makeCollectionSsaState(options);
  lowerCollectionSsaL1Body(stmt, state);
  const program = collectionSsaProgramFromState(state);
  if (state.diagnostics.length > 0) {
    return {
      program,
      finalEntries: collectionSsaFinalEntries(state),
      propositions: [],
      modifiedRules: [...program.modifiedRules],
      diagnostics: state.diagnostics,
    };
  }

  const lowerState = makeCollectionSsaLowerState(
    program,
    options.canonicalize,
    options.lowerOpaque,
  );
  lowerState.initialVersions = new Map(state.initialVersions);
  lowerCollectionSsaStmtToVersions(stmt, lowerState);
  const finalEntries = collectionSsaLowerFinalEntries(lowerState);

  return {
    program,
    finalEntries,
    propositions: lowerMapFinalEntriesToProps(finalEntries),
    modifiedRules: [...program.modifiedRules],
    diagnostics: [],
  };
}

function collectionSsaProgramFromState(
  state: CollectionSsaState,
): IR1SsaProgram {
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

export function lowerCollectionSsaToProps(
  stmt: IR1Stmt,
  options: CollectionSsaBuildOptions = {},
): CollectionSsaLowerResult {
  return lowerCollectionSsaToResult(stmt, options);
}

export function collectionSsaFinalEntries(
  state: Pick<
    CollectionSsaState,
    "currentVersions" | "initialVersions" | "locations"
  >,
): CollectionSsaFinalEntry[] {
  const entries: CollectionSsaFinalEntry[] = [];
  for (const [key, location] of state.locations) {
    const version =
      state.currentVersions.get(key) ?? state.initialVersions.get(key);
    if (version === undefined) {
      continue;
    }
    entries.push({
      kind: location.kind,
      location,
      version,
    } as CollectionSsaFinalEntry);
  }
  return entries;
}

export function isCollectionSsaL1Body(stmt: IR1Stmt): boolean {
  switch (stmt.kind) {
    case "assign":
      return (
        stmt.target.kind === "member" &&
        isCollectionSsaExpr(stmt.target.receiver) &&
        isCollectionSsaExpr(stmt.value)
      );
    case "map-effect":
      return (
        isCollectionSsaExpr(stmt.objExpr) &&
        isCollectionSsaExpr(stmt.keyExpr) &&
        (stmt.valueExpr === null || isCollectionSsaExpr(stmt.valueExpr))
      );
    case "set-effect":
      return (
        isCollectionSsaExpr(stmt.objExpr) &&
        (stmt.elemExpr === null || isCollectionSsaExpr(stmt.elemExpr))
      );
    case "block":
      return stmt.stmts.every(isCollectionSsaL1Body);
    case "cond-stmt":
      return (
        stmt.arms.length === 1 &&
        stmt.arms.every(
          ([guard, body]) =>
            isCollectionSsaExpr(guard) && isCollectionSsaL1Body(body),
        ) &&
        (stmt.otherwise === null || isCollectionSsaL1Body(stmt.otherwise))
      );
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

export function lowerCollectionSsaL1Body(
  stmt: IR1Stmt,
  state: CollectionSsaState,
): void {
  switch (stmt.kind) {
    case "assign":
      if (stmt.target.kind !== "member") {
        collectionSsaUnsupported(
          state,
          "collection SSA assignment target must be a property member",
        );
        return;
      }
      collectionSsaReadExpr(stmt.target.receiver, state);
      if (state.diagnostics.length > 0) {
        return;
      }
      collectionSsaReadExpr(stmt.value, state);
      return;
    case "map-effect":
      lowerCollectionMapEffect(stmt, state);
      return;
    case "set-effect":
      lowerCollectionSetEffect(stmt, state);
      return;
    case "block":
      for (const child of stmt.stmts) {
        lowerCollectionSsaL1Body(child, state);
        if (state.diagnostics.length > 0) {
          return;
        }
      }
      return;
    case "cond-stmt":
      lowerCollectionCondStmt(stmt, state);
      return;
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "throw":
    case "let":
    case "expr-stmt":
      collectionSsaUnsupported(
        state,
        `collection SSA does not support ${stmt.kind} in this pass`,
      );
      return;
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      collectionSsaUnsupported(
        state,
        "collection SSA does not support this IR1 statement in this pass",
      );
      return;
    }
  }
}

export function collectionSsaReadExpr(
  expr: IR1Expr,
  state: CollectionSsaState,
): IR1Expr {
  switch (expr.kind) {
    case "map-read":
      collectionSsaReadMap(expr, state);
      collectionSsaReadExpr(expr.receiver, state);
      collectionSsaReadExpr(expr.key, state);
      return expr;
    case "set-read":
      collectionSsaReadSet(expr, state);
      collectionSsaReadExpr(expr.receiver, state);
      collectionSsaReadExpr(expr.elem, state);
      return expr;
    case "member":
      collectionSsaReadExpr(expr.receiver, state);
      return expr;
    case "binop":
      collectionSsaReadExpr(expr.lhs, state);
      collectionSsaReadExpr(expr.rhs, state);
      return expr;
    case "unop":
      collectionSsaReadExpr(expr.arg, state);
      return expr;
    case "app":
      collectionSsaUnsupported(
        state,
        "collection SSA does not support call expressions in this pass",
      );
      return expr;
    case "cond":
      collectionSsaUnsupported(
        state,
        "collection SSA does not support value-position conditionals in this pass",
      );
      return expr;
    case "is-nullish":
      collectionSsaReadExpr(expr.operand, state);
      return expr;
    case "each":
      collectionSsaReadExpr(expr.src, state);
      for (const guard of expr.guards) {
        collectionSsaReadExpr(guard, state);
      }
      collectionSsaReadExpr(expr.proj, state);
      return expr;
    case "var":
    case "lit":
      return expr;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return expr;
    }
  }
}

export function mapValueLocationForReadOrWrite(
  input: {
    ruleName: string;
    keyPredName: string;
    ownerType: string;
    keyType: string;
    receiver: IR1Expr;
    key: IR1Expr;
  },
  state: CollectionSsaLocationState,
): { key: string; location: Extract<IR1SsaLocation, { kind: "map-value" }> } {
  return mapLocationForInput("map-value", input, state);
}

export function mapMembershipLocationForReadOrWrite(
  input: {
    ruleName: string;
    keyPredName: string;
    ownerType: string;
    keyType: string;
    receiver: IR1Expr;
    key: IR1Expr;
  },
  state: CollectionSsaLocationState,
): {
  key: string;
  location: Extract<IR1SsaLocation, { kind: "map-membership" }>;
} {
  return mapLocationForInput("map-membership", input, state);
}

export function setMembershipLocationForReadOrWrite(
  input: {
    ruleName: string;
    ownerType: string;
    elemType: string;
    receiver: IR1Expr;
  },
  state: CollectionSsaLocationState,
): {
  key: string;
  location: Extract<IR1SsaLocation, { kind: "set-membership" }>;
} {
  const receiver = collectionSsaCanonicalExpr(input.receiver, state);
  const receiverKey = collectionSsaExprKey(receiver);
  const key = setMembershipLocationKey(
    input.ruleName,
    input.ownerType,
    input.elemType,
    receiverKey,
  );
  const existing = state.locations.get(key);
  if (existing !== undefined) {
    if (existing.kind !== "set-membership") {
      throw new Error("collection SSA location key kind mismatch");
    }
    return { key, location: existing };
  }
  const location = ir1SsaSetMembershipLocation(
    input.ruleName,
    input.ownerType,
    input.elemType,
    receiver,
  ) as Extract<IR1SsaLocation, { kind: "set-membership" }>;
  state.locations.set(key, location);
  state.declaredRules?.add(ir1SsaRuleOfLocation(location));
  return { key, location };
}

function lowerCollectionMapEffect(
  stmt: Extract<IR1Stmt, { kind: "map-effect" }>,
  state: CollectionSsaState,
): void {
  collectionSsaReadExpr(stmt.objExpr, state);
  if (state.diagnostics.length > 0) {
    return;
  }
  collectionSsaReadExpr(stmt.keyExpr, state);
  if (state.diagnostics.length > 0) {
    return;
  }
  const input = {
    ruleName: stmt.ruleName,
    keyPredName: stmt.keyPredName,
    ownerType: stmt.ownerType,
    keyType: stmt.keyType,
    receiver: stmt.objExpr,
    key: stmt.keyExpr,
  };

  if (stmt.op === "set") {
    collectionSsaReadExpr(stmt.valueExpr, state);
    if (state.diagnostics.length > 0) {
      return;
    }
    const valueLocation = mapValueLocationForReadOrWrite(input, state);
    const valueWrite = ir1SsaWrite(
      valueLocation.location,
      ir1SsaMapSetValue(stmt.valueExpr),
    );
    recordCollectionWrite(valueLocation.key, valueWrite, state);
  }

  const membershipLocation = mapMembershipLocationForReadOrWrite(input, state);
  const membershipWrite = ir1SsaWrite(
    membershipLocation.location,
    ir1SsaMapMembershipValue(stmt.op),
  );
  recordCollectionWrite(membershipLocation.key, membershipWrite, state);
}

function lowerCollectionSetEffect(
  stmt: Extract<IR1Stmt, { kind: "set-effect" }>,
  state: CollectionSsaState,
): void {
  collectionSsaReadExpr(stmt.objExpr, state);
  if (state.diagnostics.length > 0) {
    return;
  }
  const location = setMembershipLocationForReadOrWrite(
    {
      ruleName: stmt.ruleName,
      ownerType: stmt.ownerType,
      elemType: stmt.elemType,
      receiver: stmt.objExpr,
    },
    state,
  );
  const value =
    stmt.op === "clear"
      ? ir1SsaSetClearValue()
      : ir1SsaSetMembershipValue(stmt.op, stmt.elemExpr);
  if (stmt.op !== "clear") {
    collectionSsaReadExpr(stmt.elemExpr, state);
    if (state.diagnostics.length > 0) {
      return;
    }
  }
  const write = ir1SsaWrite(location.location, value);
  recordCollectionWrite(location.key, write, state);
}

function recordCollectionWrite(
  key: string,
  write: IR1SsaWrite,
  state: CollectionSsaState,
): void {
  state.writes.push(write);
  state.currentVersions.set(key, write.version);
  state.writtenKeys.add(key);
  state.modifiedRules.add(ir1SsaRuleOfLocation(write.location));
}

interface CollectionSsaLowerState {
  writes: readonly IR1SsaWrite[];
  joins: readonly IR1SsaJoin[];
  writeIndex: number;
  joinIndex: number;
  currentVersions: Map<string, IR1SsaVersion>;
  initialVersions: Map<string, IR1SsaVersion>;
  locations: Map<string, CollectionSsaLocation>;
  versionExprs: Map<IR1SsaVersion, OpaqueExpr>;
  writtenKeys: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr;
}

function makeCollectionSsaLowerState(
  program: IR1SsaProgram,
  canonicalize?: (e: IR1Expr) => IR1Expr,
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr,
): CollectionSsaLowerState {
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
  };
}

function lowerCollectionSsaStmtToVersions(
  stmt: IR1Stmt,
  state: CollectionSsaLowerState,
): void {
  switch (stmt.kind) {
    case "assign":
      if (stmt.target.kind !== "member") {
        throw new Error(
          "collection SSA assignment target must be a property member",
        );
      }
      lowerCollectionSsaExprToOpaque(stmt.target.receiver, state);
      lowerCollectionSsaExprToOpaque(stmt.value, state);
      return;
    case "map-effect":
      lowerCollectionSsaMapEffectToVersions(stmt, state);
      return;
    case "set-effect":
      lowerCollectionSsaSetEffectToVersions(stmt, state);
      return;
    case "block":
      for (const child of stmt.stmts) {
        lowerCollectionSsaStmtToVersions(child, state);
      }
      return;
    case "cond-stmt":
      lowerCollectionSsaCondToVersions(stmt, state);
      return;
    case "foreach":
    case "for":
    case "while":
    case "return":
    case "throw":
    case "let":
    case "expr-stmt":
      throw new Error(`${stmt.kind} is not supported by collection SSA`);
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      throw new Error("unsupported IR1 statement in collection SSA lowering");
    }
  }
}

function lowerCollectionSsaMapEffectToVersions(
  stmt: Extract<IR1Stmt, { kind: "map-effect" }>,
  state: CollectionSsaLowerState,
): void {
  lowerCollectionSsaExprToOpaque(stmt.objExpr, state);
  lowerCollectionSsaExprToOpaque(stmt.keyExpr, state);
  const input = {
    ruleName: stmt.ruleName,
    keyPredName: stmt.keyPredName,
    ownerType: stmt.ownerType,
    keyType: stmt.keyType,
    receiver: stmt.objExpr,
    key: stmt.keyExpr,
  };

  if (stmt.op === "set") {
    const valueWrite = nextCollectionSsaWrite(state, "map-value");
    const { key, location } = mapValueLocationForReadOrWrite(input, state);
    if (!sameCollectionSsaLocation(valueWrite.location, location, state)) {
      throw new Error("collection SSA map-value write order mismatch");
    }
    const valueExpr = lowerCollectionSsaExprToOpaque(stmt.valueExpr, state);
    state.currentVersions.set(key, valueWrite.version);
    state.locations.set(key, location);
    state.versionExprs.set(valueWrite.version, valueExpr);
    state.writtenKeys.add(key);
  }

  const membershipWrite = nextCollectionSsaWrite(state, "map-membership");
  const { key, location } = mapMembershipLocationForReadOrWrite(input, state);
  if (!sameCollectionSsaLocation(membershipWrite.location, location, state)) {
    throw new Error("collection SSA map-membership write order mismatch");
  }
  const ast = getAst();
  state.currentVersions.set(key, membershipWrite.version);
  state.locations.set(key, location);
  state.versionExprs.set(
    membershipWrite.version,
    ast.litBool(stmt.op === "set"),
  );
  state.writtenKeys.add(key);
}

function lowerCollectionSsaSetEffectToVersions(
  stmt: Extract<IR1Stmt, { kind: "set-effect" }>,
  state: CollectionSsaLowerState,
): void {
  lowerCollectionSsaExprToOpaque(stmt.objExpr, state);
  if (stmt.op !== "clear") {
    lowerCollectionSsaExprToOpaque(stmt.elemExpr, state);
  }
  const write = nextCollectionSsaWrite(state, "set-membership");
  const { key, location } = setMembershipLocationForReadOrWrite(
    {
      ruleName: stmt.ruleName,
      ownerType: stmt.ownerType,
      elemType: stmt.elemType,
      receiver: stmt.objExpr,
    },
    state,
  );
  if (!sameCollectionSsaLocation(write.location, location, state)) {
    throw new Error("collection SSA set-membership write order mismatch");
  }
  state.currentVersions.set(key, write.version);
  state.locations.set(key, location);
  state.versionExprs.set(write.version, setMembershipWriteExpr(stmt));
  state.writtenKeys.add(key);
}

function lowerCollectionSsaCondToVersions(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: CollectionSsaLowerState,
): void {
  if (stmt.arms.length !== 1) {
    throw new Error("multi-armed cond-stmt is not supported by collection SSA");
  }
  const ast = getAst();
  const [guard, thenBody] = stmt.arms[0]!;
  const guardExpr = lowerCollectionSsaExprToOpaque(guard, state);

  const thenState = cloneCollectionSsaLowerStateForBranch(state);
  lowerCollectionSsaStmtToVersions(thenBody, thenState);

  const elseState = cloneCollectionSsaLowerStateForBranch(state);
  elseState.writeIndex = thenState.writeIndex;
  elseState.joinIndex = thenState.joinIndex;
  if (stmt.otherwise !== null) {
    lowerCollectionSsaStmtToVersions(stmt.otherwise, elseState);
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
      throw new Error("collection SSA branch touched an unknown location");
    }
    const thenVersion =
      thenState.currentVersions.get(key) ??
      initialCollectionVersionFor(key, location, thenState);
    const elseVersion =
      elseState.currentVersions.get(key) ??
      initialCollectionVersionFor(key, location, elseState);
    if (thenVersion === elseVersion) {
      state.currentVersions.set(key, thenVersion);
      state.locations.set(key, location);
      state.writtenKeys.add(key);
      continue;
    }
    const join = nextCollectionSsaJoin(state, location);
    if (
      !sameCollectionSsaVersion(join.thenVersion, thenVersion, state) ||
      !sameCollectionSsaVersion(join.elseVersion, elseVersion, state) ||
      !sameCollectionSsaLocation(join.location, location, state)
    ) {
      throw new Error(
        "collection SSA join order did not match branch versions",
      );
    }
    const thenExpr = resolveCollectionSsaVersionExpr(thenVersion, thenState);
    const elseExpr = resolveCollectionSsaVersionExpr(elseVersion, elseState);
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

function lowerCollectionSsaExprToOpaque(
  expr: IR1Expr,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const ast = getAst();
  switch (expr.kind) {
    case "map-read":
      return lowerCollectionSsaMapReadToOpaque(expr, state);
    case "set-read":
      return lowerCollectionSsaSetReadToOpaque(expr, state);
    case "var":
    case "lit":
      return state.lowerOpaque(lowerCollectionOpaqueExpr(expr, state));
    case "member":
      return state.lowerOpaque(
        ast.app(ast.var(expr.name), [
          lowerCollectionSsaExprToOpaque(expr.receiver, state),
        ]),
      );
    case "binop":
      return state.lowerOpaque(
        ast.binop(
          lowerBinop(expr.op),
          lowerCollectionSsaExprToOpaque(expr.lhs, state),
          lowerCollectionSsaExprToOpaque(expr.rhs, state),
        ),
      );
    case "unop": {
      const op =
        expr.op === "not"
          ? ast.opNot()
          : expr.op === "neg"
            ? ast.opNeg()
            : ast.opCard();
      return state.lowerOpaque(
        ast.unop(op, lowerCollectionSsaExprToOpaque(expr.arg, state)),
      );
    }
    case "app": {
      const args = expr.args.map((arg) =>
        lowerCollectionSsaExprToOpaque(arg, state),
      );
      if (expr.callee.kind === "var" && !expr.callee.primed) {
        return state.lowerOpaque(ast.app(ast.var(expr.callee.name), args));
      }
      return state.lowerOpaque(
        ast.app(lowerCollectionSsaExprToOpaque(expr.callee, state), args),
      );
    }
    case "cond": {
      const arms: Array<[OpaqueExpr, OpaqueExpr]> = expr.arms.map(
        ([guard, value]) => [
          lowerCollectionSsaExprToOpaque(guard, state),
          lowerCollectionSsaExprToOpaque(value, state),
        ],
      );
      arms.push([
        ast.litBool(true),
        lowerCollectionSsaExprToOpaque(expr.otherwise, state),
      ]);
      return state.lowerOpaque(ast.cond(arms));
    }
    case "is-nullish":
      return state.lowerOpaque(
        ast.binop(
          ast.opEq(),
          ast.unop(
            ast.opCard(),
            lowerCollectionSsaExprToOpaque(expr.operand, state),
          ),
          ast.litNat(0),
        ),
      );
    case "each":
      return state.lowerOpaque(
        ast.each(
          [],
          [
            ast.gIn(
              expr.binder,
              lowerCollectionSsaExprToOpaque(expr.src, state),
            ),
            ...expr.guards.map((guard) =>
              ast.gExpr(lowerCollectionSsaExprToOpaque(guard, state)),
            ),
          ],
          lowerCollectionSsaExprToOpaque(expr.proj, state),
        ),
      );
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      throw new Error("unsupported IR1 expression in collection SSA lowering");
    }
  }
}

function lowerCollectionSsaMapReadToOpaque(
  expr: Extract<IR1Expr, { kind: "map-read" }>,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const ast = getAst();
  const input = {
    ruleName: expr.ruleName,
    keyPredName: expr.keyPredName,
    ownerType: expr.ownerType,
    keyType: expr.keyType,
    receiver: expr.receiver,
    key: expr.key,
  };
  const receiver = lowerCollectionSsaExprToOpaque(expr.receiver, state);
  const keyExpr = lowerCollectionSsaExprToOpaque(expr.key, state);
  const keyTuple = ast.tuple([receiver, keyExpr]);
  if (expr.op === "has") {
    const loc = mapMembershipLocationForReadOrWrite(input, state);
    const version =
      state.currentVersions.get(loc.key) ??
      initialCollectionVersionFor(loc.key, loc.location, state);
    const value = resolveCollectionSsaVersionExpr(version, state);
    return mapOverrideApp(expr.keyPredName, keyTuple, value, receiver, keyExpr);
  }

  const valueLoc = mapValueLocationForReadOrWrite(input, state);
  const valueVersion =
    state.currentVersions.get(valueLoc.key) ??
    initialCollectionVersionFor(valueLoc.key, valueLoc.location, state);
  if (valueVersion.origin === "initial") {
    return state.lowerOpaque(
      ast.app(ast.var(expr.ruleName), [receiver, keyExpr]),
    );
  }

  const membershipLoc = mapMembershipLocationForReadOrWrite(input, state);
  const membershipVersion =
    state.currentVersions.get(membershipLoc.key) ??
    initialCollectionVersionFor(
      membershipLoc.key,
      membershipLoc.location,
      state,
    );
  if (
    isOpaqueLiteralFalse(
      resolveCollectionSsaVersionExpr(membershipVersion, state),
    )
  ) {
    return state.lowerOpaque(
      ast.app(ast.var(expr.ruleName), [receiver, keyExpr]),
    );
  }
  const value = resolveCollectionSsaVersionExpr(valueVersion, state);
  return mapOverrideApp(expr.ruleName, keyTuple, value, receiver, keyExpr);
}

function lowerCollectionSsaSetReadToOpaque(
  expr: Extract<IR1Expr, { kind: "set-read" }>,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const ast = getAst();
  const receiver = lowerCollectionSsaExprToOpaque(expr.receiver, state);
  const elem = lowerCollectionSsaExprToOpaque(expr.elem, state);
  return state.lowerOpaque(
    ast.binop(ast.opIn(), elem, ast.app(ast.var(expr.ruleName), [receiver])),
  );
}

function mapOverrideApp(
  ruleName: string,
  keyTuple: OpaqueExpr,
  value: OpaqueExpr,
  receiver: OpaqueExpr,
  keyExpr: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  return ast.app(ast.override(ruleName, [[keyTuple, value]]), [
    receiver,
    keyExpr,
  ]);
}

function setMembershipWriteExpr(
  stmt: Extract<IR1Stmt, { kind: "set-effect" }>,
): OpaqueExpr {
  const ast = getAst();
  if (stmt.op === "clear") {
    return ast.litBool(false);
  }
  return ast.litBool(stmt.op === "add");
}

function resolveCollectionSsaVersionExpr(
  version: IR1SsaVersion,
  state: Pick<
    CollectionSsaLowerState,
    "versionExprs" | "canonicalize" | "lowerOpaque"
  >,
): OpaqueExpr {
  const existing = state.versionExprs.get(version);
  if (existing !== undefined) {
    return existing;
  }
  if (version.origin !== "initial") {
    throw new Error("collection SSA lowering expected an initial version");
  }
  const ast = getAst();
  const location = version.location;
  let identity: OpaqueExpr;
  switch (location.kind) {
    case "map-value": {
      const receiver = state.lowerOpaque(
        lowerCollectionOpaqueExpr(location.receiver, state),
      );
      const key = state.lowerOpaque(
        lowerCollectionOpaqueExpr(location.key, state),
      );
      identity = ast.app(ast.var(location.ruleName), [receiver, key]);
      break;
    }
    case "map-membership": {
      const receiver = state.lowerOpaque(
        lowerCollectionOpaqueExpr(location.receiver, state),
      );
      const key = state.lowerOpaque(
        lowerCollectionOpaqueExpr(location.key, state),
      );
      identity = ast.app(ast.var(location.keyPredName), [receiver, key]);
      break;
    }
    case "set-membership": {
      identity = ast.litBool(false);
      break;
    }
    case "property":
      throw new Error("collection SSA cannot resolve property versions");
    default: {
      const _exhaustive: never = location;
      void _exhaustive;
      throw new Error("unsupported collection SSA location");
    }
  }
  const lowered = state.lowerOpaque(identity);
  state.versionExprs.set(version, lowered);
  return lowered;
}

function cloneCollectionSsaLowerStateForBranch(
  state: CollectionSsaLowerState,
): CollectionSsaLowerState {
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
  };
}

function nextCollectionSsaWrite(
  state: CollectionSsaLowerState,
  expectedKind: CollectionSsaLocation["kind"],
): IR1SsaWrite {
  const write = state.writes[state.writeIndex];
  if (write === undefined) {
    throw new Error("collection SSA lowering ran out of writes");
  }
  state.writeIndex += 1;
  if (write.location.kind !== expectedKind) {
    throw new Error("collection SSA write sequence did not match source order");
  }
  return write;
}

function nextCollectionSsaJoin(
  state: CollectionSsaLowerState,
  location: CollectionSsaLocation,
): IR1SsaJoin {
  const join = state.joins[state.joinIndex];
  if (join === undefined) {
    throw new Error("collection SSA lowering ran out of joins");
  }
  state.joinIndex += 1;
  if (!sameCollectionSsaLocation(join.location, location, state)) {
    throw new Error("collection SSA join sequence did not match source order");
  }
  return join;
}

function collectionSsaLowerFinalEntries(
  state: CollectionSsaLowerState,
): CollectionSsaFinalEntry[] {
  const entries: CollectionSsaFinalEntry[] = [];
  const ast = getAst();
  for (const [key, location] of state.locations) {
    const version =
      state.currentVersions.get(key) ?? state.initialVersions.get(key);
    if (version === undefined) {
      continue;
    }
    if (location.kind === "map-value" || location.kind === "map-membership") {
      const receiver = state.lowerOpaque(
        lowerCollectionOpaqueExpr(location.receiver, state),
      );
      const keyExpr = state.lowerOpaque(
        lowerCollectionOpaqueExpr(location.key, state),
      );
      entries.push({
        kind: location.kind,
        location,
        version,
        keyTuple: ast.tuple([receiver, keyExpr]),
        value: resolveCollectionSsaVersionExpr(version, state),
      } as CollectionSsaFinalEntry);
      continue;
    }
    entries.push({ kind: "set-membership", location, version });
  }
  return entries;
}

function lowerMapFinalEntriesToProps(
  entries: readonly CollectionSsaFinalEntry[],
): PropResult[] {
  const ast = getAst();
  const props: PropResult[] = [];
  const groups = new Map<
    string,
    {
      ruleName: string;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      valueOverrides: Array<[OpaqueExpr, OpaqueExpr]>;
      membershipOverrides: Array<[OpaqueExpr, OpaqueExpr]>;
    }
  >();
  for (const entry of entries) {
    if (entry.kind !== "map-value" && entry.kind !== "map-membership") {
      continue;
    }
    if (entry.keyTuple === undefined || entry.value === undefined) {
      throw new Error("lowered map SSA entry is missing override data");
    }
    const groupKey = [
      entry.location.ruleName,
      entry.location.keyPredName,
      entry.location.ownerType,
      entry.location.keyType,
    ].join("::");
    let group = groups.get(groupKey);
    if (group === undefined) {
      group = {
        ruleName: entry.location.ruleName,
        keyPredName: entry.location.keyPredName,
        ownerType: entry.location.ownerType,
        keyType: entry.location.keyType,
        valueOverrides: [],
        membershipOverrides: [],
      };
      groups.set(groupKey, group);
    }
    if (entry.kind === "map-value") {
      group.valueOverrides.push([entry.keyTuple, entry.value]);
    } else {
      group.membershipOverrides.push([entry.keyTuple, entry.value]);
    }
  }

  for (const group of groups.values()) {
    if (group.valueOverrides.length > 0) {
      const m = "m";
      const k = "k";
      props.push({
        kind: "equation",
        quantifiers: [
          ast.param(m, ast.tName(group.ownerType)),
          ast.param(k, ast.tName(group.keyType)),
        ] as OpaqueParam[],
        lhs: ast.app(ast.primed(group.ruleName), [ast.var(m), ast.var(k)]),
        rhs: ast.app(ast.override(group.ruleName, group.valueOverrides), [
          ast.var(m),
          ast.var(k),
        ]),
      });
    }
    if (group.membershipOverrides.length > 0) {
      const m = "m";
      const k = "k";
      props.push({
        kind: "equation",
        quantifiers: [
          ast.param(m, ast.tName(group.ownerType)),
          ast.param(k, ast.tName(group.keyType)),
        ] as OpaqueParam[],
        lhs: ast.app(ast.primed(group.keyPredName), [ast.var(m), ast.var(k)]),
        rhs: ast.app(
          ast.override(group.keyPredName, group.membershipOverrides),
          [ast.var(m), ast.var(k)],
        ),
      });
    }
  }
  return props;
}

function collectionSsaReadMap(
  expr: Extract<IR1Expr, { kind: "map-read" }>,
  state: CollectionSsaState,
): IR1SsaRead {
  const input = {
    ruleName: expr.ruleName,
    keyPredName: expr.keyPredName,
    ownerType: expr.ownerType,
    keyType: expr.keyType,
    receiver: expr.receiver,
    key: expr.key,
  };
  if (expr.op === "has") {
    const membership = mapMembershipLocationForReadOrWrite(input, state);
    return recordCollectionRead(membership.key, membership.location, state);
  }
  const value = mapValueLocationForReadOrWrite(input, state);
  const valueVersion = currentMapMembershipIsDelete(input, state)
    ? initialCollectionVersionFor(value.key, value.location, state)
    : undefined;
  const valueRead = recordCollectionRead(
    value.key,
    value.location,
    state,
    valueVersion,
  );
  const membership = mapMembershipLocationForReadOrWrite(input, state);
  recordCollectionRead(membership.key, membership.location, state);
  return valueRead;
}

function recordCollectionRead(
  key: string,
  location: CollectionSsaLocation,
  state: CollectionSsaState,
  versionOverride?: IR1SsaVersion,
): IR1SsaRead {
  const version =
    versionOverride ??
    state.currentVersions.get(key) ??
    initialCollectionVersionFor(key, location, state);
  const read = ir1SsaRead(location, version, true);
  state.reads.push(read);
  return read;
}

function currentMapMembershipIsDelete(
  input: {
    ruleName: string;
    keyPredName: string;
    ownerType: string;
    keyType: string;
    receiver: IR1Expr;
    key: IR1Expr;
  },
  state: CollectionSsaState,
): boolean {
  const membership = mapMembershipLocationForReadOrWrite(input, state);
  const version = state.currentVersions.get(membership.key);
  if (version === undefined) {
    return false;
  }
  const write = state.writes.find((w) => w.version === version);
  return write?.value.kind === "map-membership" && write.value.op === "delete";
}

function collectionSsaReadSet(
  expr: Extract<IR1Expr, { kind: "set-read" }>,
  state: CollectionSsaState,
): IR1SsaRead {
  const { key, location } = setMembershipLocationForReadOrWrite(
    {
      ruleName: expr.ruleName,
      ownerType: expr.ownerType,
      elemType: expr.elemType,
      receiver: expr.receiver,
    },
    state,
  );
  const version =
    state.currentVersions.get(key) ??
    initialCollectionVersionFor(key, location, state);
  const read = ir1SsaRead(location, version, true);
  state.reads.push(read);
  return read;
}

function lowerCollectionCondStmt(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: CollectionSsaState,
): void {
  if (stmt.arms.length !== 1) {
    collectionSsaUnsupported(
      state,
      "collection SSA does not support multi-armed cond-stmt in this pass",
    );
    return;
  }
  const [guard, thenBody] = stmt.arms[0]!;
  collectionSsaReadExpr(guard, state);

  const thenState = cloneCollectionSsaStateForBranch(state);
  lowerCollectionSsaL1Body(thenBody, thenState);
  if (thenState.diagnostics.length > 0) {
    state.diagnostics.push(...thenState.diagnostics);
    return;
  }

  const elseState = cloneCollectionSsaStateForBranch(state);
  if (stmt.otherwise !== null) {
    lowerCollectionSsaL1Body(stmt.otherwise, elseState);
    if (elseState.diagnostics.length > 0) {
      state.diagnostics.push(...elseState.diagnostics);
      return;
    }
  }

  state.reads.push(...thenState.reads, ...elseState.reads);
  state.writes.push(...thenState.writes, ...elseState.writes);
  state.joins.push(...thenState.joins, ...elseState.joins);
  mergeBranchLocations(thenState, state);
  mergeBranchLocations(elseState, state);
  mergeBranchRules(thenState, state);
  mergeBranchRules(elseState, state);

  const touched = new Set([...thenState.writtenKeys, ...elseState.writtenKeys]);
  for (const key of touched) {
    const location =
      thenState.locations.get(key) ??
      elseState.locations.get(key) ??
      state.locations.get(key);
    if (location === undefined) {
      throw new Error("collection SSA branch touched an unknown location");
    }
    const thenVersion =
      thenState.currentVersions.get(key) ??
      initialCollectionVersionFor(key, location, state);
    const elseVersion =
      elseState.currentVersions.get(key) ??
      initialCollectionVersionFor(key, location, state);
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

function cloneCollectionSsaStateForBranch(
  state: CollectionSsaState,
): CollectionSsaState {
  return {
    currentVersions: new Map(state.currentVersions),
    initialVersions: state.initialVersions,
    locations: new Map(state.locations),
    reads: [],
    writes: [],
    joins: [],
    diagnostics: [],
    writtenKeys: new Set(),
    declaredRules: new Set(state.declaredRules),
    modifiedRules: new Set(),
    canonicalize: state.canonicalize,
  };
}

function mergeBranchLocations(
  branch: CollectionSsaState,
  target: CollectionSsaState,
): void {
  for (const [key, location] of branch.locations) {
    if (!target.locations.has(key)) {
      target.locations.set(key, location);
    }
  }
}

function mergeBranchRules(
  branch: CollectionSsaState,
  target: CollectionSsaState,
): void {
  for (const rule of branch.declaredRules) {
    target.declaredRules.add(rule);
  }
  for (const rule of branch.modifiedRules) {
    target.modifiedRules.add(rule);
  }
}

function initialCollectionVersionFor(
  key: string,
  location: CollectionSsaLocation,
  state: CollectionSsaInitialVersionState,
): IR1SsaVersion {
  const existing = state.initialVersions.get(key);
  if (existing !== undefined) {
    return existing;
  }
  const initial = ir1SsaInitialVersion(location);
  state.initialVersions.set(key, initial);
  return initial;
}

function lowerCollectionOpaqueExpr(
  expr: IR1Expr,
  state: Pick<CollectionSsaLocationState, "canonicalize">,
): OpaqueExpr {
  return lowerExpr(lowerL1Expr(state.canonicalize(expr)));
}

function isOpaqueLiteralFalse(expr: OpaqueExpr): boolean {
  const ast = getAst();
  return ast.strExpr(expr) === ast.strExpr(ast.litBool(false));
}

function sameCollectionSsaLocation(
  left: IR1SsaLocation,
  right: IR1SsaLocation,
  state: Pick<CollectionSsaLocationState, "canonicalize">,
): boolean {
  if (left.kind !== right.kind) {
    return false;
  }
  switch (left.kind) {
    case "map-value":
      return (
        right.kind === "map-value" &&
        left.ruleName === right.ruleName &&
        left.keyPredName === right.keyPredName &&
        left.ownerType === right.ownerType &&
        left.keyType === right.keyType &&
        collectionSsaExprKey(
          collectionSsaCanonicalExpr(left.receiver, state),
        ) ===
          collectionSsaExprKey(
            collectionSsaCanonicalExpr(right.receiver, state),
          ) &&
        collectionSsaExprKey(collectionSsaCanonicalExpr(left.key, state)) ===
          collectionSsaExprKey(collectionSsaCanonicalExpr(right.key, state))
      );
    case "map-membership":
      return (
        right.kind === "map-membership" &&
        left.ruleName === right.ruleName &&
        left.keyPredName === right.keyPredName &&
        left.ownerType === right.ownerType &&
        left.keyType === right.keyType &&
        collectionSsaExprKey(
          collectionSsaCanonicalExpr(left.receiver, state),
        ) ===
          collectionSsaExprKey(
            collectionSsaCanonicalExpr(right.receiver, state),
          ) &&
        collectionSsaExprKey(collectionSsaCanonicalExpr(left.key, state)) ===
          collectionSsaExprKey(collectionSsaCanonicalExpr(right.key, state))
      );
    case "set-membership":
      return (
        right.kind === "set-membership" &&
        left.ruleName === right.ruleName &&
        left.ownerType === right.ownerType &&
        left.elemType === right.elemType &&
        collectionSsaExprKey(
          collectionSsaCanonicalExpr(left.receiver, state),
        ) ===
          collectionSsaExprKey(
            collectionSsaCanonicalExpr(right.receiver, state),
          )
      );
    case "property":
      return false;
    default: {
      const _exhaustive: never = left;
      void _exhaustive;
      return false;
    }
  }
}

function sameCollectionSsaVersion(
  left: IR1SsaVersion,
  right: IR1SsaVersion,
  state: Pick<CollectionSsaLocationState, "canonicalize">,
): boolean {
  if (left === right) {
    return true;
  }
  if (left.origin !== "initial" || right.origin !== "initial") {
    return false;
  }
  return sameCollectionSsaLocation(left.location, right.location, state);
}

function mapLocationForInput<K extends "map-value" | "map-membership">(
  kind: K,
  input: {
    ruleName: string;
    keyPredName: string;
    ownerType: string;
    keyType: string;
    receiver: IR1Expr;
    key: IR1Expr;
  },
  state: CollectionSsaLocationState,
): {
  key: string;
  location: Extract<IR1SsaLocation, { kind: K }>;
} {
  const receiver = collectionSsaCanonicalExpr(input.receiver, state);
  const element = collectionSsaCanonicalExpr(input.key, state);
  const receiverKey = collectionSsaExprKey(receiver);
  const elementKey = collectionSsaExprKey(element);
  const key = mapLocationKey(
    kind,
    input.ruleName,
    input.keyPredName,
    input.ownerType,
    input.keyType,
    receiverKey,
    elementKey,
  );
  const existing = state.locations.get(key);
  if (existing !== undefined) {
    if (existing.kind !== kind) {
      throw new Error("collection SSA location key kind mismatch");
    }
    return { key, location: existing as Extract<IR1SsaLocation, { kind: K }> };
  }
  const location =
    kind === "map-value"
      ? ir1SsaMapValueLocation(
          input.ruleName,
          input.keyPredName,
          input.ownerType,
          input.keyType,
          receiver,
          element,
        )
      : ir1SsaMapMembershipLocation(
          input.ruleName,
          input.keyPredName,
          input.ownerType,
          input.keyType,
          receiver,
          element,
        );
  state.locations.set(key, location as CollectionSsaLocation);
  state.declaredRules?.add(ir1SsaRuleOfLocation(location));
  return { key, location: location as Extract<IR1SsaLocation, { kind: K }> };
}

function mapLocationKey(
  kind: "map-value" | "map-membership",
  ruleName: string,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  receiverKey: string,
  keyExprKey: string,
): string {
  return JSON.stringify([
    kind,
    ruleName,
    keyPredName,
    ownerType,
    keyType,
    receiverKey,
    keyExprKey,
  ]);
}

function setMembershipLocationKey(
  ruleName: string,
  ownerType: string,
  elemType: string,
  receiverKey: string,
): string {
  return JSON.stringify([
    "set-membership",
    ruleName,
    ownerType,
    elemType,
    receiverKey,
  ]);
}

function collectionSsaCanonicalExpr(
  expr: IR1Expr,
  state: Pick<CollectionSsaLocationState, "canonicalize">,
): IR1Expr {
  return state.canonicalize(expr);
}

function collectionSsaExprKey(expr: IR1Expr): string {
  return JSON.stringify(collectionSsaExprKeyData(expr));
}

function collectionSsaExprKeyData(expr: IR1Expr): unknown {
  switch (expr.kind) {
    case "var":
      return ["var", expr.name, expr.primed ?? false];
    case "lit":
      return [
        "lit",
        expr.value.kind,
        "value" in expr.value ? expr.value.value : null,
      ];
    case "member":
      return ["member", collectionSsaExprKeyData(expr.receiver), expr.name];
    case "binop":
      return [
        "binop",
        expr.op,
        collectionSsaExprKeyData(expr.lhs),
        collectionSsaExprKeyData(expr.rhs),
      ];
    case "unop":
      return ["unop", expr.op, collectionSsaExprKeyData(expr.arg)];
    case "app":
      return [
        "app",
        collectionSsaExprKeyData(expr.callee),
        expr.args.map(collectionSsaExprKeyData),
      ];
    case "cond":
      return [
        "cond",
        expr.arms.map(([guard, value]) => [
          collectionSsaExprKeyData(guard),
          collectionSsaExprKeyData(value),
        ]),
        collectionSsaExprKeyData(expr.otherwise),
      ];
    case "is-nullish":
      return ["is-nullish", collectionSsaExprKeyData(expr.operand)];
    case "each":
      return [
        "each",
        expr.binder,
        collectionSsaExprKeyData(expr.src),
        expr.guards.map(collectionSsaExprKeyData),
        collectionSsaExprKeyData(expr.proj),
      ];
    case "map-read":
      return [
        "map-read",
        expr.op,
        expr.ruleName,
        expr.keyPredName,
        expr.ownerType,
        expr.keyType,
        collectionSsaExprKeyData(expr.receiver),
        collectionSsaExprKeyData(expr.key),
      ];
    case "set-read":
      return [
        "set-read",
        expr.ruleName,
        expr.ownerType,
        expr.elemType,
        collectionSsaExprKeyData(expr.receiver),
        collectionSsaExprKeyData(expr.elem),
      ];
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return ["unknown"];
    }
  }
}

function collectionSsaUnsupported(
  state: CollectionSsaState,
  reason: string,
): void {
  state.diagnostics.push({ kind: "unsupported", reason });
}

function isCollectionSsaExpr(expr: IR1Expr): boolean {
  switch (expr.kind) {
    case "var":
    case "lit":
      return true;
    case "member":
      return isCollectionSsaExpr(expr.receiver);
    case "binop":
      return isCollectionSsaExpr(expr.lhs) && isCollectionSsaExpr(expr.rhs);
    case "unop":
      return isCollectionSsaExpr(expr.arg);
    case "app":
    case "cond":
      return false;
    case "is-nullish":
      return isCollectionSsaExpr(expr.operand);
    case "each":
      return (
        isCollectionSsaExpr(expr.src) &&
        expr.guards.every(isCollectionSsaExpr) &&
        isCollectionSsaExpr(expr.proj)
      );
    case "map-read":
      return (
        isCollectionSsaExpr(expr.receiver) && isCollectionSsaExpr(expr.key)
      );
    case "set-read":
      return (
        isCollectionSsaExpr(expr.receiver) && isCollectionSsaExpr(expr.elem)
      );
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return false;
    }
  }
}
