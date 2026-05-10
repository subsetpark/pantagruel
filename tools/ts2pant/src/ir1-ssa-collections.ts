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
    writtenKeys: new Set(),
    declaredRules: new Set(options.declaredRules ?? []),
    modifiedRules: new Set(),
    canonicalize: options.canonicalize ?? ((e) => e),
  };
}

export function buildCollectionSsaProgram(
  stmt: IR1Stmt,
  options: CollectionSsaBuildOptions = {},
): IR1SsaProgram {
  const state = makeCollectionSsaState(options);
  lowerCollectionSsaL1Body(stmt, state);
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
  state: Pick<CollectionSsaState, "currentVersions" | "locations">,
): CollectionSsaFinalEntry[] {
  const entries: CollectionSsaFinalEntry[] = [];
  for (const [key, version] of state.currentVersions) {
    const location = state.locations.get(key);
    if (location === undefined) {
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
        throw new Error(
          "collection SSA assignment target must be a property member",
        );
      }
      collectionSsaReadExpr(stmt.target.receiver, state);
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
      throw new Error(`${stmt.kind} is not supported by collection SSA`);
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      throw new Error("unsupported IR1 statement in collection SSA lowering");
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
      collectionSsaReadExpr(expr.callee, state);
      for (const arg of expr.args) {
        collectionSsaReadExpr(arg, state);
      }
      return expr;
    case "cond":
      for (const [guard, value] of expr.arms) {
        collectionSsaReadExpr(guard, state);
        collectionSsaReadExpr(value, state);
      }
      collectionSsaReadExpr(expr.otherwise, state);
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
  const receiverKey = collectionSsaCanonicalExprKey(input.receiver, state);
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
    input.receiver,
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
  collectionSsaReadExpr(stmt.keyExpr, state);
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
  const { key, location } =
    expr.op === "has"
      ? mapMembershipLocationForReadOrWrite(input, state)
      : mapValueLocationForReadOrWrite(input, state);
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
    throw new Error("multi-armed cond-stmt is not supported by collection SSA");
  }
  const [guard, thenBody] = stmt.arms[0]!;
  collectionSsaReadExpr(guard, state);

  const thenState = cloneCollectionSsaStateForBranch(state);
  lowerCollectionSsaL1Body(thenBody, thenState);

  const elseState = cloneCollectionSsaStateForBranch(state);
  if (stmt.otherwise !== null) {
    lowerCollectionSsaL1Body(stmt.otherwise, elseState);
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
  const receiverKey = collectionSsaCanonicalExprKey(input.receiver, state);
  const elementKey = collectionSsaCanonicalExprKey(input.key, state);
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
          input.receiver,
          input.key,
        )
      : ir1SsaMapMembershipLocation(
          input.ruleName,
          input.keyPredName,
          input.ownerType,
          input.keyType,
          input.receiver,
          input.key,
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
  return [
    kind,
    ruleName,
    keyPredName,
    ownerType,
    keyType,
    receiverKey,
    keyExprKey,
  ].join("::");
}

function setMembershipLocationKey(
  ruleName: string,
  ownerType: string,
  elemType: string,
  receiverKey: string,
): string {
  return ["set-membership", ruleName, ownerType, elemType, receiverKey].join(
    "::",
  );
}

function collectionSsaCanonicalExprKey(
  expr: IR1Expr,
  state: Pick<CollectionSsaLocationState, "canonicalize">,
): string {
  try {
    return collectionSsaExprKey(state.canonicalize(expr));
  } catch (err) {
    if (err instanceof Error) {
      return collectionSsaExprKey(expr);
    }
    throw err;
  }
}

function collectionSsaExprKey(expr: IR1Expr): string {
  switch (expr.kind) {
    case "var":
      return `var:${expr.name}:${expr.primed ?? false}`;
    case "lit":
      return `lit:${expr.value.kind}:${"value" in expr.value ? expr.value.value : ""}`;
    case "member":
      return `member:${collectionSsaExprKey(expr.receiver)}:${expr.name}`;
    case "binop":
      return `binop:${expr.op}:${collectionSsaExprKey(expr.lhs)}:${collectionSsaExprKey(expr.rhs)}`;
    case "unop":
      return `unop:${expr.op}:${collectionSsaExprKey(expr.arg)}`;
    case "app":
      return `app:${collectionSsaExprKey(expr.callee)}(${expr.args
        .map(collectionSsaExprKey)
        .join(",")})`;
    case "cond":
      return `cond:${expr.arms
        .map(
          ([guard, value]) =>
            `${collectionSsaExprKey(guard)}=>${collectionSsaExprKey(value)}`,
        )
        .join("|")}:${collectionSsaExprKey(expr.otherwise)}`;
    case "is-nullish":
      return `is-nullish:${collectionSsaExprKey(expr.operand)}`;
    case "each":
      return `each:${expr.binder}:${collectionSsaExprKey(expr.src)}:${expr.guards
        .map(collectionSsaExprKey)
        .join(",")}:${collectionSsaExprKey(expr.proj)}`;
    case "map-read":
      return `map-read:${expr.op}:${expr.ruleName}:${expr.keyPredName}:${expr.ownerType}:${expr.keyType}:${collectionSsaExprKey(expr.receiver)}:${collectionSsaExprKey(expr.key)}`;
    case "set-read":
      return `set-read:${expr.ruleName}:${expr.ownerType}:${expr.elemType}:${collectionSsaExprKey(expr.receiver)}:${collectionSsaExprKey(expr.elem)}`;
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return "";
    }
  }
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
      return (
        isCollectionSsaExpr(expr.callee) && expr.args.every(isCollectionSsaExpr)
      );
    case "cond":
      return (
        expr.arms.every(
          ([guard, value]) =>
            isCollectionSsaExpr(guard) && isCollectionSsaExpr(value),
        ) && isCollectionSsaExpr(expr.otherwise)
      );
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
