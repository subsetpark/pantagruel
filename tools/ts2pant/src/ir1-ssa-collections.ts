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
  ir1OpaqueOriginId,
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

export interface CollectionSsaFinalPropertyEntry {
  kind: "property";
  prop: string;
  objExpr: OpaqueExpr;
  rhs: OpaqueExpr;
}

export interface CollectionSsaLowerResult {
  program: IR1SsaProgram;
  finalEntries: CollectionSsaFinalEntry[];
  finalProperties: CollectionSsaFinalPropertyEntry[];
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

interface SetMembershipOverride {
  elemExpr: OpaqueExpr;
  value: OpaqueExpr;
}

interface MapOverride {
  keyTuple: OpaqueExpr;
  objExpr: OpaqueExpr;
  keyExpr: OpaqueExpr;
  value: OpaqueExpr;
}

interface LoweredPropertyWrite {
  prop: string;
  objExpr: OpaqueExpr;
  value: OpaqueExpr;
}

interface LoweredSetMembershipVersion {
  kind: "set-membership";
  ruleName: string;
  ownerType: string;
  elemType: string;
  receiver: OpaqueExpr;
  memberOverrides: SetMembershipOverride[];
  cleared: OpaqueExpr;
}

interface CollectionSsaLowerState {
  writes: IR1SsaWrite[];
  joins: IR1SsaJoin[];
  writeIndex: number;
  joinIndex: number;
  currentVersions: Map<string, IR1SsaVersion>;
  initialVersions: Map<string, IR1SsaVersion>;
  locations: Map<string, CollectionSsaLocation>;
  versionValues: Map<IR1SsaVersion, OpaqueExpr | LoweredSetMembershipVersion>;
  propertyWrites: Map<string, LoweredPropertyWrite>;
  propertyWrittenKeys: Set<string>;
  writtenKeys: Set<string>;
  canonicalize: (e: IR1Expr) => IR1Expr;
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr;
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
      finalProperties: [],
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
  const lowered = finishCollectionSsaLowering(program, lowerState);
  return {
    ...lowered,
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
    loopHeaderJoins: [],
    loopBodies: [],
    declaredRules: [...declaredRules],
    modifiedRules: [...state.modifiedRules],
    framedRules,
  };
}

export function lowerCollectionSsaToProps(
  stmt: IR1Stmt,
  options: CollectionSsaBuildOptions = {},
): CollectionSsaLowerResult {
  if (!isCollectionSsaL1Body(stmt)) {
    const declaredRules = [...new Set(options.declaredRules ?? [])];
    return {
      program: {
        reads: [],
        writes: [],
        joins: [],
        loopHeaderJoins: [],
        loopBodies: [],
        declaredRules,
        modifiedRules: [],
        framedRules: declaredRules,
      },
      finalEntries: [],
      finalProperties: [],
      propositions: [],
      modifiedRules: [],
      diagnostics: [
        {
          kind: "unsupported",
          reason: "statement is not supported by collection SSA lowering",
        },
      ],
    };
  }

  const program = buildCollectionSsaProgram(stmt, options);
  if ("unsupported" in program) {
    const declaredRules = [...new Set(options.declaredRules ?? [])];
    return {
      program: {
        reads: [],
        writes: [],
        joins: [],
        loopHeaderJoins: [],
        loopBodies: [],
        declaredRules,
        modifiedRules: [],
        framedRules: declaredRules,
      },
      finalEntries: [],
      finalProperties: [],
      propositions: [],
      modifiedRules: [],
      diagnostics: program.diagnostics,
    };
  }
  const lowerState = makeCollectionSsaLowerState(
    program,
    options.canonicalize,
    options.lowerOpaque,
  );
  lowerCollectionSsaStmtToVersions(stmt, lowerState);

  return {
    ...finishCollectionSsaLowering(program, lowerState),
    diagnostics: [],
  };
}

function finishCollectionSsaLowering(
  program: IR1SsaProgram,
  lowerState: CollectionSsaLowerState,
): Omit<CollectionSsaLowerResult, "diagnostics"> {
  const finalEntries: CollectionSsaFinalEntry[] = [];
  const propositions: PropResult[] = [];
  for (const key of lowerState.locations.keys()) {
    const version =
      lowerState.currentVersions.get(key) ??
      lowerState.initialVersions.get(key);
    if (version === undefined) {
      continue;
    }
    const finalLocation = version.location as CollectionSsaLocation;
    finalEntries.push({
      kind: finalLocation.kind,
      location: finalLocation,
      version,
    } as CollectionSsaFinalEntry);
    if (finalLocation.kind === "set-membership") {
      const value = resolveSetMembershipVersion(version, lowerState);
      emitSetMembershipSsaEquation(value, propositions);
    }
  }
  emitMapSsaEquations(finalEntries, lowerState, propositions);
  const finalProperties = [...lowerState.propertyWrites.values()].map(
    (entry) =>
      ({
        kind: "property",
        prop: entry.prop,
        objExpr: entry.objExpr,
        rhs: entry.value,
      }) satisfies CollectionSsaFinalPropertyEntry,
  );
  const ast = getAst();
  for (const entry of finalProperties) {
    propositions.push({
      kind: "equation",
      quantifiers: [],
      lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
      rhs: entry.rhs,
    });
  }
  const modifiedRules = [
    ...new Set([
      ...program.modifiedRules,
      ...finalProperties.map((entry) => entry.prop),
    ]),
  ];

  return {
    program,
    finalEntries,
    finalProperties,
    propositions,
    modifiedRules,
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
    case "break":
    case "continue":
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
    case "break":
    case "continue":
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
    case "comb-typed":
      for (const guard of expr.guards) {
        collectionSsaReadExpr(guard, state);
      }
      collectionSsaReadExpr(expr.proj, state);
      return expr;
    case "forall":
    case "exists":
      if (expr.guard !== undefined) {
        collectionSsaReadExpr(expr.guard, state);
      }
      collectionSsaReadExpr(expr.body, state);
      return expr;
    case "var":
    case "lit":
    case "opaque":
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
    versionValues: new Map(),
    propertyWrites: new Map(),
    propertyWrittenKeys: new Set(),
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
      lowerCollectionSsaAssignToVersions(stmt, state);
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
    case "break":
    case "continue":
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

function lowerCollectionSsaAssignToVersions(
  stmt: Extract<IR1Stmt, { kind: "assign" }>,
  state: CollectionSsaLowerState,
): void {
  if (stmt.target.kind !== "member") {
    throw new Error(
      "collection SSA assignment target must be a property member",
    );
  }
  const objExpr = lowerCollectionSsaExprToOpaque(stmt.target.receiver, state);
  const value = lowerCollectionSsaExprToOpaque(stmt.value, state);
  const key = propertyWriteKey(stmt.target.name, objExpr);
  state.propertyWrites.set(key, {
    prop: stmt.target.name,
    objExpr,
    value,
  });
  state.propertyWrittenKeys.add(key);
}

function lowerCollectionSsaSetEffectToVersions(
  stmt: Extract<IR1Stmt, { kind: "set-effect" }>,
  state: CollectionSsaLowerState,
): void {
  lowerCollectionSsaExprToOpaque(stmt.objExpr, state);
  const { key, location } = setMembershipLocationForReadOrWrite(
    {
      ruleName: stmt.ruleName,
      ownerType: stmt.ownerType,
      elemType: stmt.elemType,
      receiver: stmt.objExpr,
    },
    state,
  );
  const write = nextCollectionSsaWrite(state, location);
  if (write.value.kind !== "set-membership") {
    throw new Error("collection SSA Set effect lowered to a non-Set write");
  }

  const previous =
    state.currentVersions.get(key) ??
    initialCollectionVersionFor(key, location, state);
  const previousValue = resolveSetMembershipVersion(previous, state);
  const nextValue: LoweredSetMembershipVersion =
    write.value.op === "clear"
      ? {
          ...previousValue,
          memberOverrides: [],
          cleared: getAst().litBool(true),
        }
      : lowerSetMembershipElementWrite(write.value, previousValue, state);

  state.currentVersions.set(key, write.version);
  state.locations.set(key, location);
  state.versionValues.set(write.version, nextValue);
  state.writtenKeys.add(key);
}

function lowerSetMembershipElementWrite(
  value: Extract<
    IR1SsaWrite["value"],
    { kind: "set-membership"; op: "add" | "delete" }
  >,
  previous: LoweredSetMembershipVersion,
  state: CollectionSsaLowerState,
): LoweredSetMembershipVersion {
  const ast = getAst();
  const elemExpr = lowerCollectionSsaExprToOpaque(value.elem, state);
  const elemText = ast.strExpr(elemExpr);
  const memberOverrides = previous.memberOverrides.filter(
    (o) => ast.strExpr(o.elemExpr) !== elemText,
  );
  memberOverrides.push({
    elemExpr,
    value: ast.litBool(value.op === "add"),
  });
  return {
    ...previous,
    memberOverrides,
  };
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
    const valueExpr = lowerCollectionSsaExprToOpaque(stmt.valueExpr, state);
    const valueLocation = mapValueLocationForReadOrWrite(input, state);
    const valueWrite = nextCollectionSsaWrite(state, valueLocation.location);
    if (valueWrite.value.kind !== "map-value") {
      throw new Error("collection SSA Map value effect lowered incorrectly");
    }
    state.currentVersions.set(valueLocation.key, valueWrite.version);
    state.locations.set(valueLocation.key, valueLocation.location);
    state.versionValues.set(valueWrite.version, valueExpr);
    state.writtenKeys.add(valueLocation.key);
  }
  const membershipLocation = mapMembershipLocationForReadOrWrite(input, state);
  const membershipWrite = nextCollectionSsaWrite(
    state,
    membershipLocation.location,
  );
  if (membershipWrite.value.kind !== "map-membership") {
    throw new Error("collection SSA Map membership effect lowered incorrectly");
  }
  state.currentVersions.set(membershipLocation.key, membershipWrite.version);
  state.locations.set(membershipLocation.key, membershipLocation.location);
  state.versionValues.set(
    membershipWrite.version,
    getAst().litBool(membershipWrite.value.op === "set"),
  );
  state.writtenKeys.add(membershipLocation.key);
}

function lowerCollectionSsaCondToVersions(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: CollectionSsaLowerState,
): void {
  if (stmt.arms.length !== 1) {
    throw new Error("multi-armed cond-stmt is not supported by collection SSA");
  }
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
    if (location.kind === "set-membership") {
      const thenValue = resolveSetMembershipVersion(thenVersion, thenState);
      const elseValue = resolveSetMembershipVersion(elseVersion, elseState);
      state.versionValues.set(
        join.joinVersion,
        joinSetMembershipVersions(guardExpr, thenValue, elseValue),
      );
    } else {
      state.versionValues.set(
        join.joinVersion,
        state.lowerOpaque(
          getAst().cond([
            [guardExpr, resolveMapVersion(thenVersion, thenState)],
            [getAst().litBool(true), resolveMapVersion(elseVersion, elseState)],
          ]),
        ),
      );
    }
    state.currentVersions.set(key, join.joinVersion);
    state.locations.set(key, location);
    state.writtenKeys.add(key);
  }

  const touchedProperties = new Set([
    ...thenState.propertyWrittenKeys,
    ...elseState.propertyWrittenKeys,
  ]);
  for (const key of touchedProperties) {
    const thenEntry = thenState.propertyWrites.get(key);
    const elseEntry = elseState.propertyWrites.get(key);
    const pick = (thenEntry ?? elseEntry)!;
    const identity = state.lowerOpaque(
      getAst().app(getAst().var(pick.prop), [pick.objExpr]),
    );
    state.propertyWrites.set(key, {
      prop: pick.prop,
      objExpr: pick.objExpr,
      value: state.lowerOpaque(
        getAst().cond([
          [guardExpr, thenEntry?.value ?? identity],
          [getAst().litBool(true), elseEntry?.value ?? identity],
        ]),
      ),
    });
    state.propertyWrittenKeys.add(key);
  }
}

function joinSetMembershipVersions(
  guardExpr: OpaqueExpr,
  thenValue: LoweredSetMembershipVersion,
  elseValue: LoweredSetMembershipVersion,
): LoweredSetMembershipVersion {
  const ast = getAst();
  const combine = (a: OpaqueExpr, b: OpaqueExpr): OpaqueExpr =>
    ast.cond([
      [guardExpr, a],
      [ast.litBool(true), b],
    ]);
  const baseIn = (o: SetMembershipOverride): OpaqueExpr =>
    ast.binop(
      ast.opIn(),
      o.elemExpr,
      ast.app(ast.var(thenValue.ruleName), [thenValue.receiver]),
    );
  const thenCleared = thenValue.cleared;
  const elseCleared = elseValue.cleared;
  const mergedOverrides = mergeSetMembershipOverrides(
    thenValue.memberOverrides,
    elseValue.memberOverrides,
    (o) => clearedSetFallback(elseCleared, baseIn(o)),
    combine,
    (o) => clearedSetFallback(thenCleared, baseIn(o)),
  );
  return {
    ...thenValue,
    memberOverrides: mergedOverrides,
    cleared: mergeSetClearedCond(guardExpr, thenCleared, elseCleared),
  };
}

function mergeSetMembershipOverrides(
  aSide: readonly SetMembershipOverride[],
  bSide: readonly SetMembershipOverride[],
  fallbackForMissingB: (o: SetMembershipOverride) => OpaqueExpr,
  combine: (vA: OpaqueExpr, vB: OpaqueExpr) => OpaqueExpr,
  fallbackForMissingA: (o: SetMembershipOverride) => OpaqueExpr,
): SetMembershipOverride[] {
  const ast = getAst();
  const canonical = (t: OpaqueExpr) => ast.strExpr(t);
  const bByKey = new Map<string, OpaqueExpr>();
  for (const o of bSide) {
    bByKey.set(canonical(o.elemExpr), o.value);
  }
  const seen = new Set<string>();
  const out: SetMembershipOverride[] = [];
  for (const o of aSide) {
    const key = canonical(o.elemExpr);
    seen.add(key);
    out.push({
      ...o,
      value: combine(o.value, bByKey.get(key) ?? fallbackForMissingB(o)),
    });
  }
  for (const o of bSide) {
    const key = canonical(o.elemExpr);
    if (seen.has(key)) {
      continue;
    }
    out.push({
      ...o,
      value: combine(fallbackForMissingA(o), o.value),
    });
  }
  return out;
}

function lowerCollectionSsaExprToOpaque(
  expr: IR1Expr,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const ast = getAst();
  switch (expr.kind) {
    case "set-read": {
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
      return readSetMembershipVersion(
        version,
        lowerCollectionSsaExprToOpaque(expr.elem, state),
        state,
      );
    }
    case "map-read":
      return lowerCollectionSsaMapRead(expr, state);
    case "member": {
      const receiver = lowerCollectionSsaExprToOpaque(expr.receiver, state);
      const staged = state.propertyWrites.get(
        propertyWriteKey(expr.name, receiver),
      );
      if (staged !== undefined) {
        return staged.value;
      }
      return state.lowerOpaque(ast.app(ast.var(expr.name), [receiver]));
    }
    case "var":
    case "lit":
    case "opaque":
      return state.lowerOpaque(
        lowerExpr(lowerL1Expr(state.canonicalize(expr))),
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
    case "cond":
      return state.lowerOpaque(
        ast.cond([
          ...expr.arms.map(
            ([guard, value]) =>
              [
                lowerCollectionSsaExprToOpaque(guard, state),
                lowerCollectionSsaExprToOpaque(value, state),
              ] as [OpaqueExpr, OpaqueExpr],
          ),
          [
            ast.litBool(true),
            lowerCollectionSsaExprToOpaque(expr.otherwise, state),
          ],
        ]),
      );
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
    case "comb-typed":
      return state.lowerOpaque(
        ast.eachComb(
          [ast.param(expr.binder, ast.tName(expr.binderType))],
          expr.guards.map((guard) =>
            ast.gExpr(lowerCollectionSsaExprToOpaque(guard, state)),
          ),
          expr.combiner === "min" ? ast.combMin() : ast.combMax(),
          lowerCollectionSsaExprToOpaque(expr.proj, state),
        ),
      );
    case "forall": {
      const guards =
        expr.guard === undefined
          ? []
          : [ast.gExpr(lowerCollectionSsaExprToOpaque(expr.guard, state))];
      return state.lowerOpaque(
        ast.forall(
          [ast.param(expr.binder, ast.tName(expr.binderType))],
          guards,
          lowerCollectionSsaExprToOpaque(expr.body, state),
        ),
      );
    }
    case "exists": {
      const guards =
        expr.guard === undefined
          ? []
          : [ast.gExpr(lowerCollectionSsaExprToOpaque(expr.guard, state))];
      return state.lowerOpaque(
        ast.exists(
          [ast.param(expr.binder, ast.tName(expr.binderType))],
          guards,
          lowerCollectionSsaExprToOpaque(expr.body, state),
        ),
      );
    }
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      throw new Error("unsupported IR1 expression in collection SSA lowering");
    }
  }
}

function readSetMembershipVersion(
  version: IR1SsaVersion,
  queryExpr: OpaqueExpr,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const value = resolveSetMembershipVersion(version, state);
  return projectSetMembershipValue(value, queryExpr);
}

function lowerCollectionSsaMapRead(
  expr: Extract<IR1Expr, { kind: "map-read" }>,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const input = {
    ruleName: expr.ruleName,
    keyPredName: expr.keyPredName,
    ownerType: expr.ownerType,
    keyType: expr.keyType,
    receiver: expr.receiver,
    key: expr.key,
  };
  const location =
    expr.op === "has"
      ? mapMembershipLocationForReadOrWrite(input, state)
      : mapValueLocationForReadOrWrite(input, state);
  const version =
    state.currentVersions.get(location.key) ??
    initialCollectionVersionFor(location.key, location.location, state);
  if (expr.op === "get") {
    const membershipLocation = mapMembershipLocationForReadOrWrite(
      input,
      state,
    );
    const membershipVersion =
      state.currentVersions.get(membershipLocation.key) ??
      initialCollectionVersionFor(
        membershipLocation.key,
        membershipLocation.location,
        state,
      );
    if (isStaticBoolLit(resolveMapVersion(membershipVersion, state), false)) {
      return resolveMapVersion(
        initialCollectionVersionFor(location.key, location.location, state),
        state,
      );
    }
  }
  return resolveMapVersion(version, state);
}

function resolveMapVersion(
  version: IR1SsaVersion,
  state: CollectionSsaLowerState,
): OpaqueExpr {
  const existing = state.versionValues.get(version);
  if (existing !== undefined) {
    if (typeof existing !== "object" || !("kind" in existing)) {
      return existing as OpaqueExpr;
    }
    if ((existing as { kind: string }).kind !== "set-membership") {
      return existing as unknown as OpaqueExpr;
    }
  }
  const location = version.location;
  if (location.kind === "map-value") {
    const value = state.lowerOpaque(
      getAst().app(getAst().var(location.ruleName), [
        lowerCollectionSsaExprToOpaque(location.receiver, state),
        lowerCollectionSsaExprToOpaque(location.key, state),
      ]),
    );
    state.versionValues.set(version, value);
    return value;
  }
  if (location.kind === "map-membership") {
    const value = state.lowerOpaque(
      getAst().app(getAst().var(location.keyPredName), [
        lowerCollectionSsaExprToOpaque(location.receiver, state),
        lowerCollectionSsaExprToOpaque(location.key, state),
      ]),
    );
    state.versionValues.set(version, value);
    return value;
  }
  throw new Error("collection SSA expected a Map version");
}

function projectSetMembershipValue(
  value: LoweredSetMembershipVersion,
  queryExpr: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  const preState = ast.binop(
    ast.opIn(),
    queryExpr,
    ast.app(ast.var(value.ruleName), [value.receiver]),
  );
  const tail = clearedSetFallback(value.cleared, preState);
  if (value.memberOverrides.length === 0) {
    return tail;
  }
  return ast.cond([
    ...value.memberOverrides.map(
      (o) =>
        [ast.binop(ast.opEq(), queryExpr, o.elemExpr), o.value] as [
          OpaqueExpr,
          OpaqueExpr,
        ],
    ),
    [ast.litBool(true), tail],
  ]);
}

function resolveSetMembershipVersion(
  version: IR1SsaVersion,
  state: CollectionSsaLowerState,
): LoweredSetMembershipVersion {
  const existing = state.versionValues.get(version);
  if (existing !== undefined) {
    if (typeof existing === "object" && "kind" in existing) {
      const kind = (existing as { kind: string }).kind;
      if (kind === "set-membership") {
        return existing as LoweredSetMembershipVersion;
      }
    }
    throw new Error("collection SSA expected a Set membership value");
  }
  if (
    version.origin !== "initial" ||
    version.location.kind !== "set-membership"
  ) {
    throw new Error(
      "collection SSA lowering expected a Set membership version",
    );
  }
  const initial: LoweredSetMembershipVersion = {
    kind: "set-membership",
    ruleName: version.location.ruleName,
    ownerType: version.location.ownerType,
    elemType: version.location.elemType,
    receiver: state.lowerOpaque(
      lowerExpr(lowerL1Expr(state.canonicalize(version.location.receiver))),
    ),
    memberOverrides: [],
    cleared: getAst().litBool(false),
  };
  state.versionValues.set(version, initial);
  return initial;
}

function emitMapSsaEquations(
  finalEntries: readonly CollectionSsaFinalEntry[],
  state: CollectionSsaLowerState,
  propositions: PropResult[],
): void {
  const ast = getAst();
  const groups = new Map<
    string,
    {
      ruleName: string;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      valueOverrides: MapOverride[];
      membershipOverrides: MapOverride[];
    }
  >();
  const groupFor = (
    location:
      | Extract<IR1SsaLocation, { kind: "map-value" }>
      | Extract<IR1SsaLocation, { kind: "map-membership" }>,
  ) => {
    const key = `${location.ruleName}\0${location.keyPredName}\0${location.ownerType}\0${location.keyType}`;
    const existing = groups.get(key);
    if (existing !== undefined) {
      return existing;
    }
    const created = {
      ruleName: location.ruleName,
      keyPredName: location.keyPredName,
      ownerType: location.ownerType,
      keyType: location.keyType,
      valueOverrides: [],
      membershipOverrides: [],
    };
    groups.set(key, created);
    return created;
  };

  for (const entry of finalEntries) {
    if (entry.kind !== "map-value" && entry.kind !== "map-membership") {
      continue;
    }
    if (entry.version.origin === "initial") {
      continue;
    }
    const location = entry.location;
    const objExpr = lowerCollectionSsaExprToOpaque(location.receiver, state);
    const keyExpr = lowerCollectionSsaExprToOpaque(location.key, state);
    const override = {
      keyTuple: ast.tuple([objExpr, keyExpr]),
      objExpr,
      keyExpr,
      value: resolveMapVersion(entry.version, state),
    };
    const group = groupFor(location);
    if (entry.kind === "map-value") {
      group.valueOverrides.push(override);
    } else {
      group.membershipOverrides.push(override);
    }
  }

  for (const group of groups.values()) {
    if (group.valueOverrides.length > 0) {
      const m = freshMapBinder(group, "m");
      const k = freshMapBinder(group, "k");
      propositions.push({
        kind: "equation",
        quantifiers: [
          ast.param(m, ast.tName(group.ownerType)),
          ast.param(k, ast.tName(group.keyType)),
        ] as OpaqueParam[],
        lhs: ast.app(ast.primed(group.ruleName), [ast.var(m), ast.var(k)]),
        rhs: ast.app(
          ast.override(
            group.ruleName,
            group.valueOverrides.map(
              (o) => [o.keyTuple, o.value] as [OpaqueExpr, OpaqueExpr],
            ),
          ),
          [ast.var(m), ast.var(k)],
        ),
      });
    }
    if (group.membershipOverrides.length > 0) {
      const m = freshMapBinder(group, "m");
      const k = freshMapBinder(group, "k");
      propositions.push({
        kind: "equation",
        quantifiers: [
          ast.param(m, ast.tName(group.ownerType)),
          ast.param(k, ast.tName(group.keyType)),
        ] as OpaqueParam[],
        lhs: ast.app(ast.primed(group.keyPredName), [ast.var(m), ast.var(k)]),
        rhs: ast.app(
          ast.override(
            group.keyPredName,
            group.membershipOverrides.map(
              (o) => [o.keyTuple, o.value] as [OpaqueExpr, OpaqueExpr],
            ),
          ),
          [ast.var(m), ast.var(k)],
        ),
      });
    }
  }
}

function freshMapBinder(
  group: {
    valueOverrides: readonly MapOverride[];
    membershipOverrides: readonly MapOverride[];
  },
  hint: string,
): string {
  const ast = getAst();
  const usedText = [...group.valueOverrides, ...group.membershipOverrides]
    .flatMap((o) => [
      ast.strExpr(o.objExpr),
      ast.strExpr(o.keyExpr),
      ast.strExpr(o.value),
    ])
    .join("\n");
  let candidate = hint;
  let suffix = 1;
  while (new RegExp(`\\b${escapeRegExp(candidate)}\\b`, "u").test(usedText)) {
    candidate = `${hint}${suffix}`;
    suffix += 1;
  }
  return candidate;
}

function emitSetMembershipSsaEquation(
  value: LoweredSetMembershipVersion,
  propositions: PropResult[],
): void {
  if (
    value.memberOverrides.length === 0 &&
    isStaticBoolLit(value.cleared, false)
  ) {
    return;
  }
  const ast = getAst();
  const y = freshSetMembershipBinder(value, "y");
  const yVar = ast.var(y);
  const memberIn = (rule: OpaqueExpr) => ast.binop(ast.opIn(), yVar, rule);
  const primedApp = ast.app(ast.primed(value.ruleName), [value.receiver]);
  const preApp = ast.app(ast.var(value.ruleName), [value.receiver]);
  const tail = clearedSetFallback(value.cleared, memberIn(preApp));
  const condArms = value.memberOverrides.map(
    (o) =>
      [ast.binop(ast.opEq(), yVar, o.elemExpr), o.value] as [
        OpaqueExpr,
        OpaqueExpr,
      ],
  );
  const rhs =
    condArms.length === 0
      ? tail
      : ast.cond([...condArms, [ast.litBool(true), tail]]);
  propositions.push({
    kind: "assertion",
    quantifiers: [ast.param(y, ast.tName(value.elemType))] as OpaqueParam[],
    body: ast.binop(ast.opIff(), memberIn(primedApp), rhs),
  });
}

function freshSetMembershipBinder(
  value: LoweredSetMembershipVersion,
  hint: string,
): string {
  const ast = getAst();
  const usedText = [
    ast.strExpr(value.receiver),
    ast.strExpr(value.cleared),
    ...value.memberOverrides.flatMap((o) => [
      ast.strExpr(o.elemExpr),
      ast.strExpr(o.value),
    ]),
  ].join("\n");
  let candidate = hint;
  let i = 1;
  while (nameAppearsInOpaqueText(candidate, usedText)) {
    candidate = `${hint}${i}`;
    i += 1;
  }
  return candidate;
}

function nameAppearsInOpaqueText(name: string, text: string): boolean {
  return new RegExp(
    `(^|[^A-Za-z0-9_])${escapeRegExp(name)}([^A-Za-z0-9_]|$)`,
    "u",
  ).test(text);
}

function escapeRegExp(input: string): string {
  return input.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
}

function nextCollectionSsaWrite(
  state: CollectionSsaLowerState,
  location?: CollectionSsaLocation,
): IR1SsaWrite {
  const write = state.writes[state.writeIndex];
  if (write === undefined) {
    throw new Error("collection SSA lowering ran out of writes");
  }
  state.writeIndex += 1;
  if (
    location !== undefined &&
    !sameCollectionSsaLocation(write.location, location, state.canonicalize)
  ) {
    throw new Error("collection SSA write order did not match source order");
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
  if (!sameCollectionSsaLocation(join.location, location, state.canonicalize)) {
    throw new Error("collection SSA join sequence did not match source order");
  }
  return join;
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
    versionValues: new Map(state.versionValues),
    propertyWrites: new Map(state.propertyWrites),
    propertyWrittenKeys: new Set(),
    writtenKeys: new Set(),
    canonicalize: state.canonicalize,
    lowerOpaque: state.lowerOpaque,
  };
}

function propertyWriteKey(prop: string, objExpr: OpaqueExpr): string {
  return `property::${prop}::${getAst().strExpr(objExpr)}`;
}

function clearedSetFallback(cleared: OpaqueExpr, pre: OpaqueExpr): OpaqueExpr {
  const ast = getAst();
  if (isStaticBoolLit(cleared, false)) {
    return pre;
  }
  if (isStaticBoolLit(cleared, true)) {
    return ast.litBool(false);
  }
  return ast.cond([
    [cleared, ast.litBool(false)],
    [ast.litBool(true), pre],
  ]);
}

function mergeSetClearedCond(
  guardExpr: OpaqueExpr,
  thenCleared: OpaqueExpr,
  elseCleared: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  if (ast.strExpr(thenCleared) === ast.strExpr(elseCleared)) {
    return thenCleared;
  }
  if (
    isStaticBoolLit(thenCleared, true) &&
    isStaticBoolLit(elseCleared, false)
  ) {
    return guardExpr;
  }
  if (
    isStaticBoolLit(thenCleared, false) &&
    isStaticBoolLit(elseCleared, true)
  ) {
    return ast.unop(ast.opNot(), guardExpr);
  }
  return ast.cond([
    [guardExpr, thenCleared],
    [ast.litBool(true), elseCleared],
  ]);
}

function isStaticBoolLit(expr: OpaqueExpr, value: boolean): boolean {
  const ast = getAst();
  return ast.strExpr(expr) === ast.strExpr(ast.litBool(value));
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
    case "opaque":
      return ["opaque", expr.sort, ir1OpaqueOriginId(expr.origin)];
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
    case "comb-typed":
      return [
        "comb-typed",
        expr.combiner,
        expr.binder,
        expr.binderType,
        expr.guards.map(collectionSsaExprKeyData),
        collectionSsaExprKeyData(expr.proj),
      ];
    case "forall":
    case "exists":
      return [
        expr.kind,
        expr.binder,
        expr.binderType,
        expr.guard === undefined ? null : collectionSsaExprKeyData(expr.guard),
        collectionSsaExprKeyData(expr.body),
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

function sameCollectionSsaLocation(
  left: IR1SsaLocation,
  right: CollectionSsaLocation,
  canonicalize: (e: IR1Expr) => IR1Expr,
): boolean {
  const canonicalKey = (expr: IR1Expr) =>
    collectionSsaExprKey(collectionSsaCanonicalExpr(expr, { canonicalize }));
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
        canonicalKey(left.receiver) === canonicalKey(right.receiver) &&
        canonicalKey(left.key) === canonicalKey(right.key)
      );
    case "map-membership":
      return (
        right.kind === "map-membership" &&
        left.ruleName === right.ruleName &&
        left.keyPredName === right.keyPredName &&
        left.ownerType === right.ownerType &&
        left.keyType === right.keyType &&
        canonicalKey(left.receiver) === canonicalKey(right.receiver) &&
        canonicalKey(left.key) === canonicalKey(right.key)
      );
    case "set-membership":
      return (
        right.kind === "set-membership" &&
        left.ruleName === right.ruleName &&
        left.ownerType === right.ownerType &&
        left.elemType === right.elemType &&
        canonicalKey(left.receiver) === canonicalKey(right.receiver)
      );
    default: {
      const _exhaustive: never = left;
      void _exhaustive;
      return false;
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
    case "opaque":
      return true;
    case "member":
      return isCollectionSsaExpr(expr.receiver);
    case "binop":
      return isCollectionSsaExpr(expr.lhs) && isCollectionSsaExpr(expr.rhs);
    case "unop":
      return isCollectionSsaExpr(expr.arg);
    case "cond":
      return (
        expr.arms.every(
          ([guard, value]) =>
            isCollectionSsaExpr(guard) && isCollectionSsaExpr(value),
        ) && isCollectionSsaExpr(expr.otherwise)
      );
    case "app":
      return false;
    case "is-nullish":
      return isCollectionSsaExpr(expr.operand);
    case "each":
      return (
        isCollectionSsaExpr(expr.src) &&
        expr.guards.every(isCollectionSsaExpr) &&
        isCollectionSsaExpr(expr.proj)
      );
    case "comb-typed":
      return (
        expr.guards.every(isCollectionSsaExpr) && isCollectionSsaExpr(expr.proj)
      );
    case "forall":
    case "exists":
      return (
        (expr.guard === undefined || isCollectionSsaExpr(expr.guard)) &&
        isCollectionSsaExpr(expr.body)
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
