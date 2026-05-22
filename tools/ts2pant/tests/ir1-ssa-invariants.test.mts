import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import ts from "typescript";

import { createSourceFile, getChecker } from "../src/extract.js";
import {
  buildL1AssignStmt,
  buildL1EffectCall,
  buildL1ForEachCall,
  buildL1ForOfMutation,
  buildL1IfMutation,
  type BuildBodyCtx,
  isUnsupported,
} from "../src/ir1-build-body.js";
import { lowerL1BodyToSsaProps } from "../src/ir1-lower-body.js";
import {
  scalarSsaBodyLowerResult,
  collectionSsaBodyLowerResult,
  type IR1SsaBodyLowerResult,
} from "../src/ir1-ssa-lower.js";
import {
  type IR1SsaJoin,
  type IR1SsaLocation,
  type IR1SsaLoopSummary,
  type IR1SsaProgram,
  type IR1SsaRead,
  type IR1SsaWrite,
  type IR1Stmt,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1CondStmt,
  ir1For,
  ir1Foreach,
  ir1LitBool,
  ir1LitNat,
  ir1MapRead,
  ir1MapSet,
  ir1Member,
  ir1SsaInitialVersion,
  ir1SsaJoin,
  ir1SsaMapMembershipLocation,
  ir1SsaMapValueLocation,
  ir1SsaPropertyLocation,
  ir1SsaRuleOfLocation,
  ir1SsaSetMembershipLocation,
  ir1SetAddOrDelete,
  ir1SetClear,
  ir1Var,
  ir1While,
} from "../src/ir1.js";
import { lowerCollectionSsaToResult } from "../src/ir1-ssa-collections.js";
import { lowerScalarSsaToProps } from "../src/ir1-ssa-scalars.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { IntStrategy } from "../src/translate-types.js";
import { buildDocumentFromSourceFile } from "./helpers.mts";

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");

type RepresentativeProgramKind =
  | "scalar"
  | "map"
  | "set"
  | "branch"
  | "foreach-shape-a"
  | "foreach-shape-b";

interface RepresentativeProgram {
  name: string;
  kind: RepresentativeProgramKind;
  build: () => Promise<IR1SsaBodyLowerResult>;
}

interface BodyLoweredRepresentativeProgram {
  name: string;
  build: () => Promise<{
    result: IR1SsaBodyLowerResult;
    declaredRules: string[];
  }>;
}

interface SsaProgramVisitor {
  onWrite?: (write: IR1SsaWrite, index: number) => void;
  onRead?: (read: IR1SsaRead, index: number) => void;
  onJoin?: (join: IR1SsaJoin, index: number) => void;
  onSummary?: (summary: IR1SsaLoopSummary, index: number) => void;
}

function loadFixture(fileName: string) {
  return createSourceFile(resolve(CONSTRUCTS_DIR, fileName));
}

async function buildFixtureBodyLowerResult(
  fileName: string,
  functionName: string,
): Promise<IR1SsaBodyLowerResult> {
  const sourceFile = loadFixture(fileName);
  const stmt = buildFixtureSupportedSsaBody(sourceFile, functionName);
  const doc = await buildDocumentFromSourceFile(sourceFile, functionName);
  return lowerL1BodyToSsaProps(stmt, doc.declarations, {
    applyConst: (expr) => expr,
  });
}

function buildFixtureSupportedSsaBody(
  sourceFile: ReturnType<typeof createSourceFile>,
  functionName: string,
): IR1Stmt {
  const fn = sourceFile.getFunctionOrThrow(functionName);
  const body = fn.getBodyOrThrow().compilerNode;
  assert.ok(ts.isBlock(body), `${functionName} should have a block body`);

  const checker = getChecker(sourceFile);
  const paramNames = new Map(
    fn.getParameters().map((param) => [param.getName(), param.getName()]),
  );
  const ctx: BuildBodyCtx = {
    checker,
    strategy: IntStrategy,
    paramNames,
    state: {
      writes: new Map(),
      writtenKeys: new Set(),
      modifiedProps: new Set(),
      canonicalize: (expr) => expr,
    },
    supply: { n: 0 } as BuildBodyCtx["supply"],
    applyConst: (expr) => expr,
  };

  const stmts: IR1Stmt[] = [];
  for (const stmt of body.statements) {
    if (ts.isReturnStatement(stmt) && stmt.expression === undefined) {
      continue;
    }
    if (ts.isVariableStatement(stmt)) {
      assert.fail(
        `${functionName} uses local declarations that Patch 1's fixture bridge does not mirror yet`,
      );
    }
    const built = buildSupportedFixtureStatement(stmt, ctx);
    if (isUnsupported(built)) {
      assert.fail(`${functionName}: ${built.unsupported}`);
    }
    stmts.push(built);
  }

  assert.ok(stmts.length > 0, `${functionName} should produce an SSA body`);
  return stmts.length === 1 ? stmts[0]! : ir1Block([stmts[0]!, ...stmts.slice(1)]);
}

function buildSupportedFixtureStatement(
  stmt: ts.Statement,
  ctx: BuildBodyCtx,
): IR1Stmt | { unsupported: string } {
  if (ts.isBlock(stmt)) {
    const children: IR1Stmt[] = [];
    for (const child of stmt.statements) {
      if (ts.isReturnStatement(child) && child.expression === undefined) {
        continue;
      }
      const built = buildSupportedFixtureStatement(child, ctx);
      if (isUnsupported(built)) {
        return built;
      }
      children.push(built);
    }
    if (children.length === 0) {
      return { unsupported: "empty mutating body" };
    }
    return children.length === 1
      ? children[0]!
      : ir1Block([children[0]!, ...children.slice(1)]);
  }
  if (ts.isIfStatement(stmt)) {
    return buildL1IfMutation(stmt, ctx);
  }
  if (ts.isForOfStatement(stmt)) {
    return buildL1ForOfMutation(stmt, ctx);
  }
  if (ts.isExpressionStatement(stmt)) {
    const expr = ts.skipPartiallyEmittedExpressions(stmt.expression);
    if (
      ts.isCallExpression(expr) &&
      ts.isPropertyAccessExpression(expr.expression) &&
      expr.expression.name.text === "forEach"
    ) {
      return buildL1ForEachCall(expr, ctx);
    }
    if (ts.isCallExpression(expr)) {
      return buildL1EffectCall(expr, ctx);
    }
    if (ts.isBinaryExpression(expr)) {
      return buildL1AssignStmt(stmt, ctx);
    }
  }
  return {
    unsupported: "statement is not supported by Patch 1's fixture bridge",
  };
}

function buildCollectionResult(stmt: IR1Stmt): IR1SsaBodyLowerResult {
  return collectionSsaBodyLowerResult(lowerCollectionSsaToResult(stmt));
}

function buildScalarResult(stmt: IR1Stmt): IR1SsaBodyLowerResult {
  return scalarSsaBodyLowerResult(lowerScalarSsaToProps(stmt));
}

function representativePrograms(): RepresentativeProgram[] {
  return [
    {
      name: "functions-mutating.ts > deposit",
      kind: "scalar",
      build: () => buildFixtureBodyLowerResult("functions-mutating.ts", "deposit"),
    },
    {
      name: "expressions-map-mutation.ts > setAndCopy",
      kind: "map",
      build: async () =>
        buildCollectionResult(
          ir1Block([
            ir1MapSet(
              "stringToIntMap",
              "stringToIntMapKey",
              "MapStringInt",
              "String",
              ir1Var("m"),
              ir1Var("kSrc"),
              ir1Var("v"),
            ),
            ir1MapSet(
              "stringToIntMap",
              "stringToIntMapKey",
              "MapStringInt",
              "String",
              ir1Var("m"),
              ir1Var("kDst"),
              ir1MapRead(
                "get",
                "stringToIntMap",
                "stringToIntMapKey",
                "MapStringInt",
                "String",
                ir1Var("m"),
                ir1Var("kSrc"),
              ),
            ),
          ]),
        ),
    },
    {
      name: "expressions-set-mutation-field.ts > tagClearAndAdd",
      kind: "set",
      build: async () =>
        buildCollectionResult(
          ir1Block([
            ir1SetClear("Tagged_tags", "Tagged", "String", ir1Var("c")),
            ir1SetAddOrDelete(
              "add",
              "Tagged_tags",
              "Tagged",
              "String",
              ir1Var("c"),
              ir1Var("x"),
            ),
          ]),
        ),
    },
    {
      name: "expressions-state-aware-reads.ts > entrySetThenCheck",
      kind: "map",
      build: () =>
        buildFixtureBodyLowerResult(
          "expressions-state-aware-reads.ts",
          "entrySetThenCheck",
        ),
    },
    {
      name: "expressions-state-aware-reads.ts > tagThenCheck",
      kind: "set",
      build: () =>
        buildFixtureBodyLowerResult(
          "expressions-state-aware-reads.ts",
          "tagThenCheck",
        ),
    },
    {
      name: "expressions-state-aware-reads.ts > bumpInBranch",
      kind: "branch",
      build: async () =>
        buildScalarResult(
          ir1CondStmt(
            [
              [
                ir1Var("g"),
                ir1Assign(
                  ir1Member(ir1Var("account"), "Account_balance"),
                  ir1Binop(
                    "add",
                    ir1Member(ir1Var("account"), "Account_balance"),
                    ir1LitNat(1),
                  ),
                ),
              ],
            ],
            ir1Assign(
              ir1Member(ir1Var("account"), "Account_balance"),
              ir1LitNat(2),
            ),
          ),
        ),
    },
    {
      name: "scalar branch join continuation read",
      kind: "branch",
      build: async () => {
        const balance = ir1Member(ir1Var("account"), "Account_balance");
        return buildScalarResult(
          ir1Block([
            ir1CondStmt(
              [
                [
                  ir1Var("g"),
                  ir1Assign(balance, ir1LitNat(1)),
                ],
              ],
              ir1Assign(balance, ir1LitNat(2)),
            ),
            ir1Assign(balance, ir1Binop("add", balance, ir1LitNat(1))),
          ]),
        );
      },
    },
    {
      name: "functions-mutating-loop.ts > forEachActivate",
      kind: "foreach-shape-a",
      build: () =>
        buildFixtureBodyLowerResult(
          "functions-mutating-loop.ts",
          "forEachActivate",
        ),
    },
    {
      name: "functions-mutating-loop.ts > forEachSum",
      kind: "foreach-shape-b",
      build: () =>
        buildFixtureBodyLowerResult("functions-mutating-loop.ts", "forEachSum"),
    },
  ];
}

function bodyLoweredRepresentativePrograms(): BodyLoweredRepresentativeProgram[] {
  return [
    {
      name: "functions-mutating.ts > deposit",
      build: async () => {
        const sourceFile = loadFixture("functions-mutating.ts");
        const doc = await buildDocumentFromSourceFile(sourceFile, "deposit");
        return {
          result: await buildFixtureBodyLowerResult("functions-mutating.ts", "deposit"),
          declaredRules: doc.declarations
            .filter((decl) => decl.kind === "rule")
            .map((decl) => decl.name),
        };
      },
    },
    {
      name: "expressions-state-aware-reads.ts > entrySetThenCheck",
      build: async () => {
        const sourceFile = loadFixture("expressions-state-aware-reads.ts");
        const doc = await buildDocumentFromSourceFile(
          sourceFile,
          "entrySetThenCheck",
        );
        return {
          result: await buildFixtureBodyLowerResult(
            "expressions-state-aware-reads.ts",
            "entrySetThenCheck",
          ),
          declaredRules: doc.declarations
            .filter((decl) => decl.kind === "rule")
            .map((decl) => decl.name),
        };
      },
    },
    {
      name: "expressions-state-aware-reads.ts > tagThenCheck",
      build: async () => {
        const sourceFile = loadFixture("expressions-state-aware-reads.ts");
        const doc = await buildDocumentFromSourceFile(sourceFile, "tagThenCheck");
        return {
          result: await buildFixtureBodyLowerResult(
            "expressions-state-aware-reads.ts",
            "tagThenCheck",
          ),
          declaredRules: doc.declarations
            .filter((decl) => decl.kind === "rule")
            .map((decl) => decl.name),
        };
      },
    },
    {
      name: "functions-mutating-conditional.ts > asymmetric",
      build: async () => {
        const sourceFile = loadFixture("functions-mutating-conditional.ts");
        const doc = await buildDocumentFromSourceFile(sourceFile, "asymmetric");
        return {
          result: await buildFixtureBodyLowerResult(
            "functions-mutating-conditional.ts",
            "asymmetric",
          ),
          declaredRules: doc.declarations
            .filter((decl) => decl.kind === "rule")
            .map((decl) => decl.name),
        };
      },
    },
    {
      name: "functions-mutating-loop.ts > forEachActivate",
      build: async () => {
        const sourceFile = loadFixture("functions-mutating-loop.ts");
        const doc = await buildDocumentFromSourceFile(sourceFile, "forEachActivate");
        return {
          result: await buildFixtureBodyLowerResult(
            "functions-mutating-loop.ts",
            "forEachActivate",
          ),
          declaredRules: doc.declarations
            .filter((decl) => decl.kind === "rule")
            .map((decl) => decl.name),
        };
      },
    },
    {
      name: "functions-mutating-loop.ts > forEachSum",
      build: async () => {
        const sourceFile = loadFixture("functions-mutating-loop.ts");
        const doc = await buildDocumentFromSourceFile(sourceFile, "forEachSum");
        return {
          result: await buildFixtureBodyLowerResult(
            "functions-mutating-loop.ts",
            "forEachSum",
          ),
          declaredRules: doc.declarations
            .filter((decl) => decl.kind === "rule")
            .map((decl) => decl.name),
        };
      },
    },
  ];
}

function walkSsaProgram(program: IR1SsaProgram, visit: SsaProgramVisitor): void {
  for (const [index, write] of program.writes.entries()) {
    visit.onWrite?.(write, index);
  }
  for (const [index, read] of program.reads.entries()) {
    visit.onRead?.(read, index);
  }
  for (const [index, join] of program.joins.entries()) {
    visit.onJoin?.(join, index);
  }
  for (const [index, summary] of program.loopSummaries.entries()) {
    visit.onSummary?.(summary, index);
  }
}

function locationSignature(location: IR1SsaLocation): string {
  return JSON.stringify(location);
}

function readSignature(read: IR1SsaRead): string {
  return [
    `${read.location.kind} read at ${locationSignature(read.location)}`,
    `using ${read.version.origin} version ${String(read.version.id)}`,
  ].join(" ");
}

function frameRuleNames(result: IR1SsaBodyLowerResult): string[] {
  const ast = getAst();
  return result.propositions
    .filter((prop) => prop.kind === "equation")
    .flatMap((prop) => {
      const lhs = ast.strExpr(prop.lhs);
      const rhs = ast.strExpr(prop.rhs);
      const match = /^([^'\s]+)'(?:\s|$)/u.exec(lhs);
      if (match === null) {
        return [];
      }
      const rule = match[1]!;
      return rhs === lhs.replace("'", "") ? [rule] : [];
    });
}

function assertSupportedRepresentativeResult(
  representativeName: string,
  result: IR1SsaBodyLowerResult,
): void {
  assert.equal(
    result.diagnostics.length,
    0,
    `${representativeName} should lower without diagnostics`,
  );
  assert.ok(
    result.programs.length > 0,
    `${representativeName} should emit at least one SSA program`,
  );
}

function assertFreshVersionsAndJoinLocations(
  representativeName: string,
  result: IR1SsaBodyLowerResult,
): void {
  assertSupportedRepresentativeResult(representativeName, result);

  for (const [programIndex, program] of result.programs.entries()) {
    const seenVersionIds = new Map<string, symbol[]>();

    const trackFreshVersion = (
      occurrence:
        | { version: IR1SsaWrite["version"]; location: IR1SsaWrite["location"] }
        | { version: IR1SsaJoin["joinVersion"]; location: IR1SsaJoin["location"] }
        | {
            version: IR1SsaLoopSummary["summaryVersion"];
            location: IR1SsaLoopSummary["location"];
          },
      detail: string,
    ): void => {
      assert.equal(
        locationSignature(occurrence.version.location),
        locationSignature(occurrence.location),
        `${representativeName} [program ${programIndex}] ${detail} should keep its version at the same location`,
      );
      const signature = locationSignature(occurrence.location);
      const priorIds = seenVersionIds.get(signature) ?? [
        ir1SsaInitialVersion(occurrence.location).id,
      ];
      for (const priorId of priorIds) {
        assert.notEqual(
          occurrence.version.id,
          priorId,
          `${representativeName} [program ${programIndex}] ${detail} should allocate a fresh SSA version`,
        );
      }
      seenVersionIds.set(signature, [...priorIds, occurrence.version.id]);
    };

    walkSsaProgram(program, {
      onWrite(write, index) {
        trackFreshVersion(
          { version: write.version, location: write.location },
          `write #${index}`,
        );
      },
      onJoin(join, index) {
        assert.equal(
          locationSignature(join.thenVersion.location),
          locationSignature(join.location),
          `${representativeName} [program ${programIndex}] join #${index} then-version should stay at the join location`,
        );
        assert.equal(
          locationSignature(join.elseVersion.location),
          locationSignature(join.location),
          `${representativeName} [program ${programIndex}] join #${index} else-version should stay at the join location`,
        );
        assert.notEqual(
          join.thenVersion.id,
          join.elseVersion.id,
          `${representativeName} [program ${programIndex}] join #${index} should merge distinct incoming versions`,
        );
        trackFreshVersion(
          { version: join.joinVersion, location: join.location },
          `join #${index}`,
        );
        assert.notEqual(
          join.joinVersion.id,
          join.thenVersion.id,
          `${representativeName} [program ${programIndex}] join #${index} should allocate a fresh result version`,
        );
        assert.notEqual(
          join.joinVersion.id,
          join.elseVersion.id,
          `${representativeName} [program ${programIndex}] join #${index} should allocate a fresh result version`,
        );
      },
      onSummary(summary, index) {
        trackFreshVersion(
          {
            version: summary.summaryVersion,
            location: summary.location,
          },
          `loop summary #${index} (${summary.shape})`,
        );
      },
    });
  }
}

function assertDominatingReads(
  representativeName: string,
  result: IR1SsaBodyLowerResult,
): void {
  assertSupportedRepresentativeResult(representativeName, result);

  for (const [programIndex, program] of result.programs.entries()) {
    walkSsaProgram(program, {
      onRead(read, readIndex) {
        const readDetail = [
          `${representativeName} [program ${programIndex}]`,
          `read #${readIndex}: ${readSignature(read)}`,
        ].join(" ");
        assert.equal(
          locationSignature(read.version.location),
          locationSignature(read.location),
          `${readDetail} should keep its version at the read location`,
        );
        assert.equal(
          read.dominated,
          true,
          `${readDetail} should be marked as dominated`,
        );

        const location = locationSignature(read.location);
        const initial = ir1SsaInitialVersion(read.location);
        const resolvesToInitial =
          read.version.origin === initial.origin &&
          locationSignature(read.version.location) ===
            locationSignature(initial.location);
        const resolvesToWrite = program.writes.some(
          (write) =>
            write.version === read.version &&
            locationSignature(write.location) === location,
        );
        const resolvesToJoin = program.joins.some(
          (join) =>
            join.joinVersion === read.version &&
            locationSignature(join.location) === location,
        );
        const resolvesToLoopSummary = program.loopSummaries.some(
          (summary) =>
            summary.summaryVersion === read.version &&
            locationSignature(summary.location) === location,
        );

        assert.ok(
          resolvesToInitial ||
            resolvesToWrite ||
            resolvesToJoin ||
            resolvesToLoopSummary,
          `${readDetail} did not resolve to an initial, write, join, or loop-summary version at its own location`,
        );
      },
    });
  }
}

function assertFrameSuppression(
  detail: string,
  result: IR1SsaBodyLowerResult,
  declaredRules: readonly string[],
): void {
  assert.equal(
    result.diagnostics.length,
    0,
    `${detail} should lower without diagnostics`,
  );

  const modifiedRules = result.modifiedRules;
  assert.equal(
    new Set(modifiedRules).size,
    modifiedRules.length,
    `${detail} should list each modified rule at most once`,
  );

  const declaredRuleSet = new Set(declaredRules);
  const modifiedRuleSet = new Set(modifiedRules);
  const expectedFramedRules = declaredRules.filter(
    (rule) => !modifiedRuleSet.has(rule),
  );
  const actualFramedRules = frameRuleNames(result);
  const actualFramedRuleSet = new Set(actualFramedRules);

  assert.deepEqual(
    actualFramedRules,
    expectedFramedRules,
    `${detail} should append exactly one identity frame for each declared but unmodified rule`,
  );
  assert.equal(
    actualFramedRuleSet.size,
    actualFramedRules.length,
    `${detail} should append each frame at most once`,
  );

  for (const rule of modifiedRuleSet) {
    assert.ok(
      declaredRuleSet.has(rule),
      `${detail} should only mark declared rules as modified`,
    );
    assert.ok(
      !actualFramedRuleSet.has(rule),
      `${detail} should not frame a modified rule`,
    );
  }

  for (const rule of expectedFramedRules) {
    assert.ok(
      declaredRuleSet.has(rule),
      `${detail} should only frame declared rules`,
    );
    assert.ok(
      !modifiedRuleSet.has(rule),
      `${detail} should keep framed rules disjoint from modified rules`,
    );
  }

  for (const [programIndex, program] of result.programs.entries()) {
    const programDetail = `${detail} [program ${programIndex}]`;
    const programModifiedRules = new Set(program.modifiedRules);
    const programRuleSources = new Set([
      ...program.writes.map((write) => ir1SsaRuleOfLocation(write.location)),
      ...program.loopSummaries.map((summary) =>
        ir1SsaRuleOfLocation(summary.location)
      ),
    ]);

    for (const modifiedRule of program.modifiedRules) {
      assert.ok(
        programRuleSources.has(modifiedRule),
        `${programDetail} should only mark rules modified when a write or loop summary produced them`,
      );
    }

    for (const [writeIndex, write] of program.writes.entries()) {
      const rule = ir1SsaRuleOfLocation(write.location);
      assert.ok(
        programModifiedRules.has(rule),
        `${programDetail} write #${writeIndex} should mark ${rule} as modified`,
      );
    }

    for (const [summaryIndex, summary] of program.loopSummaries.entries()) {
      const rule = ir1SsaRuleOfLocation(summary.location);
      assert.ok(
        programModifiedRules.has(rule),
        `${programDetail} loop summary #${summaryIndex} should mark ${rule} as modified`,
      );
    }
  }
}

function assertUnsupportedDiagnosticShape(
  detail: string,
  result: IR1SsaBodyLowerResult,
  reason: RegExp,
): void {
  assert.equal(
    result.diagnostics.length,
    1,
    `${detail} should emit exactly one unsupported diagnostic`,
  );
  assert.match(
    result.diagnostics[0]!.reason,
    reason,
    `${detail} should emit the targeted unsupported reason family`,
  );
  assert.equal(
    result.propositions.filter((prop) => prop.kind === "equation").length,
    0,
    `${detail} should emit no equations after rejection`,
  );
  assert.deepEqual(
    frameRuleNames(result),
    [],
    `${detail} should emit no frames after rejection`,
  );
}

before(async () => {
  await loadAst();
});

describe("ir1-ssa-invariants", () => {
  void representativePrograms;
  void walkSsaProgram;

  it(
    "each write, join, and summary produces a fresh SSA version at its location",
    async () => {
      for (const representative of representativePrograms()) {
        const result = await representative.build();
        assertFreshVersionsAndJoinLocations(representative.name, result);
      }
    },
  );

  it(
    "ir1SsaJoin rejects mismatched-location inputs across all four location kinds",
    () => {
      const propertyBalance = ir1SsaPropertyLocation(
        "Account_balance",
        ir1Var("account"),
        "balance",
      );
      const propertyLimit = ir1SsaPropertyLocation(
        "Account_limit",
        ir1Var("account"),
        "limit",
      );
      const mapValue = ir1SsaMapValueLocation(
        "Account_score",
        "Account_hasScore",
        "Account",
        "User",
        ir1Var("account"),
        ir1Var("user"),
      );
      const mapMembership = ir1SsaMapMembershipLocation(
        "Account_score",
        "Account_hasScore",
        "Account",
        "User",
        ir1Var("account"),
        ir1Var("user"),
      );
      const setMembership = ir1SsaSetMembershipLocation(
        "Account_tags",
        "Account",
        "Tag",
        ir1Var("account"),
      );
      const otherSetMembership = ir1SsaSetMembershipLocation(
        "Account_tags",
        "Account",
        "Tag",
        ir1Var("otherAccount"),
      );

      const mismatchedPairs: ReadonlyArray<
        readonly [name: string, expected: IR1SsaLocation, actual: IR1SsaLocation]
      > = [
        ["property vs property", propertyLimit, propertyBalance],
        ["property vs map-value", propertyBalance, mapValue],
        ["property vs set-membership", propertyBalance, setMembership],
        ["map-value vs map-membership", mapValue, mapMembership],
        [
          "set-membership vs set-membership receiver mismatch",
          setMembership,
          otherSetMembership,
        ],
      ];

      for (const [name, expected, actual] of mismatchedPairs) {
        const initial = ir1SsaInitialVersion(actual);
        assert.throws(
          () => ir1SsaJoin(expected, initial, initial),
          /location mismatch/u,
          `${name} should reject incompatible locations`,
        );
      }

      const version = ir1SsaInitialVersion(propertyBalance);
      const degenerate = ir1SsaJoin(propertyBalance, version, version);
      assert.equal(degenerate, version);
      assert.notEqual(degenerate.kind, "ssa-join");
    },
  );

  it(
    "every read resolves to a dominating version, initial version, or explicit loop summary at its own location",
    async () => {
      for (const representative of representativePrograms()) {
        const result = await representative.build();
        assertDominatingReads(representative.name, result);
      }
    },
  );

  it(
    "every modified rule suppresses frame generation exactly once and every framed rule is unmodified",
    async () => {
      for (const representative of bodyLoweredRepresentativePrograms()) {
        const { result, declaredRules } = await representative.build();
        assertFrameSuppression(representative.name, result, declaredRules);
      }
    },
  );

  it(
    "unsupported loops and branch shapes return targeted diagnostics with no equations or frames",
    async () => {
      const mutatingSourceFile = loadFixture("functions-mutating.ts");
      const mutatingDoc = await buildDocumentFromSourceFile(
        mutatingSourceFile,
        "deposit",
      );
      const loopDeclarations = mutatingDoc.declarations;
      const balance = ir1Member(ir1Var("account"), "Account_balance");

      const unsupportedCases: Array<{
        name: string;
        build: () => IR1SsaBodyLowerResult;
        reason: RegExp;
      }> = [
        {
          name: "while loop on property",
          build: () =>
            scalarSsaBodyLowerResult(
              lowerScalarSsaToProps(
                ir1While(ir1LitBool(true), ir1Assign(balance, ir1LitNat(1))),
              ),
            ),
          reason: /scalar SSA lowering/u,
        },
        {
          name: "for loop on property",
          build: () =>
            scalarSsaBodyLowerResult(
              lowerScalarSsaToProps(
                ir1For(
                  null,
                  ir1LitBool(true),
                  null,
                  ir1Assign(balance, ir1LitNat(1)),
                ),
              ),
            ),
          reason: /scalar SSA lowering/u,
        },
        {
          name: "multi-arm scalar cond-stmt",
          build: () =>
            scalarSsaBodyLowerResult(
              lowerScalarSsaToProps(
                ir1CondStmt(
                  [
                    [ir1Var("g1"), ir1Assign(balance, ir1LitNat(1))],
                    [ir1Var("g2"), ir1Assign(balance, ir1LitNat(2))],
                  ],
                  ir1Assign(balance, ir1LitNat(3)),
                ),
              ),
            ),
          reason: /scalar SSA lowering/u,
        },
        {
          name: "foreach Shape A assignment to non-property target",
          build: () =>
            lowerL1BodyToSsaProps(
              ir1Foreach(
                "$0",
                ir1Var("xs"),
                ir1Assign(ir1Var("acc"), ir1LitBool(true)),
              ),
              loopDeclarations,
              { applyConst: (expr) => expr },
            ),
            reason:
              /foreach Shape A summary assignment target must be a property/u,
        },
        {
          name: "nested proposition-emitting loop body",
          build: () => {
            const inner = ir1Foreach(
              "$1",
              ir1Var("ys"),
              ir1Assign(ir1Member(ir1Var("$1"), "active"), ir1LitBool(true)),
            );
            const outer = ir1Foreach(
              "$0",
              ir1Var("xs"),
              inner as unknown as Parameters<typeof ir1Foreach>[2],
            );
            return lowerL1BodyToSsaProps(outer, loopDeclarations, {
              applyConst: (expr) => expr,
            });
          },
          reason: /nested proposition-emitting loop body/u,
        },
      ];

      for (const unsupportedCase of unsupportedCases) {
        assertUnsupportedDiagnosticShape(
          unsupportedCase.name,
          unsupportedCase.build(),
          unsupportedCase.reason,
        );
      }
    },
  );

  it(
    "the table-driven corpus satisfies all SSA invariants for scalar, Map, Set, branch, foreach Shape A, and foreach Shape B programs",
    async () => {
      const representatives = representativePrograms();
      const presentKinds = new Set(representatives.map((r) => r.kind));

      for (const kind of [
        "scalar",
        "map",
        "set",
        "branch",
        "foreach-shape-a",
        "foreach-shape-b",
      ] satisfies RepresentativeProgramKind[]) {
        assert.ok(
          presentKinds.has(kind),
          `representativePrograms() should cover ${kind}`,
        );
      }

      for (const representative of representatives) {
        const result = await representative.build();
        assertSupportedRepresentativeResult(representative.name, result);
        assertFreshVersionsAndJoinLocations(representative.name, result);
        assertDominatingReads(representative.name, result);
      }

      for (const representative of bodyLoweredRepresentativePrograms()) {
        const { result, declaredRules } = await representative.build();
        assertFrameSuppression(representative.name, result, declaredRules);
      }
    },
  );

  it.skip(
    "L7 invariant: every loop-header join has exactly one preheader and one loop-back input",
    () => {
      // PENDING Patch 7: verify loop-header join preheader and loop-back uniqueness.
    },
  );

  it.skip(
    "L7 invariant: every loop body resolves break/continue continuations to the correct join",
    () => {
      // PENDING Patch 7: verify loop-body continuation handle resolution.
    },
  );

  it.skip(
    "L7 invariant: every bounded loop carries a non-null termination metric, every fixed-point loop does not",
    () => {
      // PENDING Patch 7: verify metric presence by loop class.
    },
  );

  it.skip(
    "L7 invariant: every modified rule appears in IR1SsaProgram.modifiedRules exactly once",
    () => {
      // PENDING Patch 7: verify loop-modified rule frame suppression.
    },
  );
});
