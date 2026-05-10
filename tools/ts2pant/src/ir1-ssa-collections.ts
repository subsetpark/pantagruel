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
}

export interface CollectionSsaFinalMapValueEntry {
  kind: "map-value";
  location: Extract<IR1SsaLocation, { kind: "map-value" }>;
  version: IR1SsaVersion;
}

export interface CollectionSsaFinalMapMembershipEntry {
  kind: "map-membership";
  location: Extract<IR1SsaLocation, { kind: "map-membership" }>;
  version: IR1SsaVersion;
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
  return {
    program,
    finalEntries: collectionSsaFinalEntries(state),
    propositions: [],
    modifiedRules: [...program.modifiedRules],
    diagnostics: state.diagnostics,
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
  const valueRead = recordCollectionRead(value.key, value.location, state);
  const membership = mapMembershipLocationForReadOrWrite(input, state);
  recordCollectionRead(membership.key, membership.location, state);
  return valueRead;
}

function recordCollectionRead(
  key: string,
  location: CollectionSsaLocation,
  state: CollectionSsaState,
): IR1SsaRead {
  const version =
    state.currentVersions.get(key) ??
    initialCollectionVersionFor(key, location, state);
  const read = ir1SsaRead(location, version, true);
  state.reads.push(read);
  return read;
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
