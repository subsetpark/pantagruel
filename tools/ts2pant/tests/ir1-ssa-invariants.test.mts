// @archlint.module test
// @archlint.domain ts2pant.ir1-ssa-invariants

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import ts from "typescript";

import { createSourceFile, getChecker } from "../src/extract.js";
import {
  type IR1FoldLeaf,
  type IR1SsaJoin,
  type IR1SsaLocation,
  type IR1SsaLoopBody,
  type IR1SsaLoopHeaderJoin,
  type IR1SsaProgram,
  type IR1SsaRead,
  type IR1SsaWrite,
  type IR1Stmt,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1Break,
  ir1CondStmt,
  ir1Continue,
  ir1For,
  ir1Foreach,
  ir1Let,
  ir1LitBool,
  ir1LitNat,
  ir1MapRead,
  ir1MapSet,
  ir1Member,
  ir1Return,
  ir1SetAddOrDelete,
  ir1SetClear,
  ir1SsaInitialVersion,
  ir1SsaJoin,
  ir1SsaMapMembershipLocation,
  ir1SsaMapValueLocation,
  ir1SsaPropertyLocation,
  ir1SsaRuleOfLocation,
  ir1SsaSetMembershipLocation,
  ir1Throw,
  ir1Var,
  ir1While,
} from "../src/ir1.js";
import { createL1AssumptionEnv } from "../src/ir1-build.js";
import {
  type BuildBodyCtx,
  buildL1AssignStmt,
  buildL1EffectCall,
  buildL1ForEachCall,
  buildL1ForOfMutation,
  buildL1IfMutation,
  isUnsupported,
} from "../src/ir1-build-body.js";
import { lowerL1BodyToSsaProps } from "../src/ir1-lower-body.js";
import { lowerCollectionSsaToResult } from "../src/ir1-ssa-collections.js";
import { lowerCounterLoopL1Body } from "../src/ir1-ssa-counter-loop.js";
import { lowerFixedPointLoopL1Body } from "../src/ir1-ssa-fixed-point.js";
import {
  lowerForeachShapeAAsGeneralLoop,
  lowerForeachShapeBAsGeneralLoop,
} from "../src/ir1-ssa-foreach.js";
import {
  collectionSsaBodyLowerResult,
  type IR1SsaBodyLowerResult,
  scalarSsaBodyLowerResult,
} from "../src/ir1-ssa-lower.js";
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
    env: createL1AssumptionEnv(),
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
  return stmts.length === 1
    ? stmts[0]!
    : ir1Block([stmts[0]!, ...stmts.slice(1)]);
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
      build: () =>
        buildFixtureBodyLowerResult("functions-mutating.ts", "deposit"),
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
              [[ir1Var("g"), ir1Assign(balance, ir1LitNat(1))]],
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
          result: await buildFixtureBodyLowerResult(
            "functions-mutating.ts",
            "deposit",
          ),
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
        const doc = await buildDocumentFromSourceFile(
          sourceFile,
          "tagThenCheck",
        );
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
        const doc = await buildDocumentFromSourceFile(
          sourceFile,
          "forEachActivate",
        );
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

function walkSsaProgram(
  program: IR1SsaProgram,
  visit: SsaProgramVisitor,
): void {
  for (const [index, write] of program.writes.entries()) {
    visit.onWrite?.(write, index);
  }
  for (const [index, read] of program.reads.entries()) {
    visit.onRead?.(read, index);
  }
  for (const [index, join] of program.joins.entries()) {
    visit.onJoin?.(join, index);
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
        | {
            version: IR1SsaJoin["joinVersion"];
            location: IR1SsaJoin["location"];
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
        assert.ok(
          resolvesToInitial || resolvesToWrite || resolvesToJoin,
          `${readDetail} did not resolve to an initial, write, or join version at its own location`,
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
    ]);

    for (const modifiedRule of program.modifiedRules) {
      assert.ok(
        programRuleSources.has(modifiedRule),
        `${programDetail} should only mark rules modified when a write produced them`,
      );
    }

    for (const [writeIndex, write] of program.writes.entries()) {
      const rule = ir1SsaRuleOfLocation(write.location);
      assert.ok(
        programModifiedRules.has(rule),
        `${programDetail} write #${writeIndex} should mark ${rule} as modified`,
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

type LoopClass =
  | "counter"
  | "bounded-while"
  | "fixed-point-while"
  | "foreach-shape-a"
  | "foreach-shape-b";

interface LoopInvariantCase {
  name: string;
  loopClass: LoopClass;
  result: IR1SsaBodyLowerResult;
}

const loopAccount = ir1Var("loopAccount");
const loopItems = ir1Var("loopItems");
const loopItem = ir1Var("loopItem");
const loopTotal = ir1Member(loopAccount, "Loop_total");

function counterLoopStmt(): Extract<IR1Stmt, { kind: "for" }> {
  return ir1For(
    ir1Let("i", ir1LitNat(0)),
    ir1Binop("lt", ir1Var("i"), ir1Var("n")),
    ir1Assign(ir1Var("i"), ir1Binop("add", ir1Var("i"), ir1LitNat(1))),
    ir1Assign(loopTotal, ir1Binop("add", loopTotal, ir1Var("i"))),
  ) as Extract<IR1Stmt, { kind: "for" }>;
}

function boundedWhileLoopStmt(): Extract<IR1Stmt, { kind: "for" }> {
  return ir1For(
    ir1Let("j", ir1LitNat(0)),
    ir1Binop("lt", ir1Var("j"), ir1Var("limit")),
    ir1Assign(ir1Var("j"), ir1Binop("add", ir1Var("j"), ir1LitNat(1))),
    ir1Assign(loopTotal, ir1Var("j")),
  ) as Extract<IR1Stmt, { kind: "for" }>;
}

function fixedPointLoopStmt(
  body: IR1Stmt = ir1Assign(
    loopTotal,
    ir1Binop("add", loopTotal, ir1Var("step")),
  ),
): Extract<IR1Stmt, { kind: "while" }> {
  return ir1While(ir1Binop("lt", loopTotal, ir1Var("target")), body) as Extract<
    IR1Stmt,
    { kind: "while" }
  >;
}

function foreachShapeBFoldLeaves(): IR1FoldLeaf[] {
  return [
    {
      target: loopAccount,
      prop: "Loop_total",
      combiner: "add",
      outerOp: "add",
      rhs: ir1Member(loopItem, "Item_amount"),
      guard: null,
    },
  ];
}

function loopInvariantCases(): LoopInvariantCase[] {
  const shapeA = lowerForeachShapeAAsGeneralLoop({
    binder: "loopItem",
    source: loopItems,
    body: ir1Assign(ir1Member(loopItem, "Item_seen"), ir1LitBool(true)),
  });
  const shapeB = lowerForeachShapeBAsGeneralLoop({
    binder: "loopItem",
    source: loopItems,
    foldLeaves: foreachShapeBFoldLeaves(),
  });

  return [
    {
      name: "counter loop",
      loopClass: "counter",
      result: lowerCounterLoopL1Body(counterLoopStmt()),
    },
    {
      name: "bounded while loop normalized to counter SSA",
      loopClass: "bounded-while",
      result: lowerCounterLoopL1Body(boundedWhileLoopStmt()),
    },
    {
      name: "fixed-point while loop",
      loopClass: "fixed-point-while",
      result: lowerFixedPointLoopL1Body(fixedPointLoopStmt()),
    },
    {
      name: "foreach Shape A",
      loopClass: "foreach-shape-a",
      result: {
        programs: [shapeA.program],
        propositions: shapeA.propositions,
        diagnostics: shapeA.diagnostics,
        modifiedRules: shapeA.modifiedRules,
      },
    },
    {
      name: "foreach Shape B",
      loopClass: "foreach-shape-b",
      result: {
        programs: [shapeB.program],
        propositions: shapeB.propositions,
        diagnostics: shapeB.diagnostics,
        modifiedRules: shapeB.modifiedRules,
      },
    },
  ];
}

function continuationLoopCases(): LoopInvariantCase[] {
  const breakStmt = ir1CondStmt(
    [[ir1Var("shouldBreak"), ir1Break()]],
    ir1Assign(loopTotal, ir1Binop("add", loopTotal, ir1Var("step"))),
  );
  const continueStmt = ir1Block([
    ir1CondStmt([[ir1Var("shouldContinue"), ir1Continue()]], null),
    ir1Assign(loopTotal, ir1Binop("add", loopTotal, ir1Var("step"))),
  ]);
  const returnStmt = ir1Block([
    ir1CondStmt([[ir1Var("shouldReturn"), ir1Return(loopTotal)]], null),
    ir1Assign(loopTotal, ir1Binop("add", loopTotal, ir1Var("step"))),
  ]);
  const throwStmt = ir1Block([
    ir1CondStmt([[ir1Var("shouldThrow"), ir1Throw(ir1Var("err"))]], null),
    ir1Assign(loopTotal, ir1Binop("add", loopTotal, ir1Var("step"))),
  ]);

  return [
    ...loopInvariantCases(),
    {
      name: "fixed-point while loop with break",
      loopClass: "fixed-point-while",
      result: lowerFixedPointLoopL1Body(fixedPointLoopStmt(breakStmt)),
    },
    {
      name: "fixed-point while loop with continue",
      loopClass: "fixed-point-while",
      result: lowerFixedPointLoopL1Body(fixedPointLoopStmt(continueStmt)),
    },
    {
      name: "fixed-point while loop with valued return",
      loopClass: "fixed-point-while",
      result: lowerFixedPointLoopL1Body(fixedPointLoopStmt(returnStmt), {
        returnRuleName: "fn_result",
      }),
    },
    {
      name: "fixed-point while loop with throw",
      loopClass: "fixed-point-while",
      result: lowerFixedPointLoopL1Body(fixedPointLoopStmt(throwStmt)),
    },
  ];
}

function assertLoopInvariantCaseSupported(testCase: LoopInvariantCase): void {
  assert.equal(
    testCase.result.diagnostics.length,
    0,
    `${testCase.name} should lower without diagnostics`,
  );
  assert.ok(
    testCase.result.programs.length > 0,
    `${testCase.name} should emit at least one SSA program`,
  );
}

function assertClosedLoopHeaderJoin(
  detail: string,
  header: IR1SsaLoopHeaderJoin,
): void {
  assert.equal(
    header.kind,
    "ssa-loop-header-join",
    `${detail} should be a loop-header join`,
  );
  assert.equal(
    locationSignature(header.preheaderVersion.location),
    locationSignature(header.location),
    `${detail} preheader version should match the header location`,
  );
  assert.ok(
    header.loopBackVersion !== null,
    `${detail} should have a loop-back`,
  );
  assert.equal(
    locationSignature(header.loopBackVersion.location),
    locationSignature(header.location),
    `${detail} loop-back version should match the header location`,
  );
  assert.equal(header.closed, true, `${detail} should be closed`);
}

function assertLoopBodyContinuationsResolved(
  detail: string,
  body: IR1SsaLoopBody,
): void {
  const headersByLocation = new Map(
    body.headerJoins.map((header) => [
      locationSignature(header.location),
      header,
    ]),
  );
  const writeVersions = new Set(body.writes.map((write) => write.version));

  for (const [index, handle] of body.breakHandles.entries()) {
    const header = headersByLocation.get(locationSignature(handle.location));
    assert.ok(
      header !== undefined,
      `${detail} break #${index} should match a header location`,
    );
    assert.ok(
      writeVersions.has(handle.version),
      `${detail} break #${index} should resolve to a body write reaching the post-loop join`,
    );
  }

  for (const [index, handle] of body.continueHandles.entries()) {
    const header = headersByLocation.get(locationSignature(handle.location));
    assert.ok(
      header !== undefined,
      `${detail} continue #${index} should match a header location`,
    );
    assert.equal(
      handle.version,
      header.loopBackVersion,
      `${detail} continue #${index} should resolve to the header loop-back version`,
    );
  }

  for (const [index, handle] of body.returnHandles.entries()) {
    const header = headersByLocation.get(locationSignature(handle.location));
    assert.ok(
      header !== undefined,
      `${detail} return #${index} should match a header location`,
    );
    assert.ok(
      writeVersions.has(handle.version),
      `${detail} return #${index} should resolve to a body write before the function return continuation`,
    );
  }

  for (const [index, handle] of body.throwHandles.entries()) {
    const header = headersByLocation.get(locationSignature(handle.location));
    assert.ok(
      header !== undefined,
      `${detail} throw #${index} should match a header location`,
    );
    assert.ok(
      writeVersions.has(handle.version),
      `${detail} throw #${index} should resolve to a body write guarded by the iteration precondition`,
    );
    assert.ok(
      handle.guard !== null,
      `${detail} throw #${index} should carry a guard`,
    );
  }
}

function assertMetricMatchesLoopClass(
  detail: string,
  loopClass: LoopClass,
  body: IR1SsaLoopBody,
): void {
  switch (loopClass) {
    case "counter":
    case "bounded-while":
      assert.equal(
        body.terminationMetric?.kind,
        "ssa-termination-metric",
        `${detail} should use a counter-style termination metric`,
      );
      break;
    case "foreach-shape-a":
    case "foreach-shape-b":
      assert.equal(
        body.terminationMetric?.kind,
        "ssa-iterating-source-metric",
        `${detail} should use an iterating-source termination metric`,
      );
      break;
    case "fixed-point-while":
      assert.equal(
        body.terminationMetric,
        null,
        `${detail} should not carry a termination metric`,
      );
      break;
    default: {
      const _exhaustive: never = loopClass;
      void _exhaustive;
    }
  }
}

function assertLoopModifiedRulesAppearOnce(
  detail: string,
  program: IR1SsaProgram,
): void {
  const counts = new Map<string, number>();
  for (const rule of program.modifiedRules) {
    counts.set(rule, (counts.get(rule) ?? 0) + 1);
  }

  for (const [bodyIndex, body] of program.loopBodies.entries()) {
    for (const [writeIndex, write] of body.writes.entries()) {
      const rule = ir1SsaRuleOfLocation(write.location);
      assert.equal(
        counts.get(rule),
        1,
        `${detail} body #${bodyIndex} write #${writeIndex} should list ${rule} exactly once in modifiedRules`,
      );
    }
  }
}

before(async () => {
  await loadAst();
});

describe("ir1-ssa-invariants", () => {
  void representativePrograms;
  void walkSsaProgram;

  it("each write and join produces a fresh SSA version at its location", async () => {
    for (const representative of representativePrograms()) {
      const result = await representative.build();
      assertFreshVersionsAndJoinLocations(representative.name, result);
    }
  });

  it("ir1SsaJoin rejects mismatched-location inputs across all four location kinds", () => {
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
  });

  it("every read resolves to a dominating version or initial version at its own location", async () => {
    for (const representative of representativePrograms()) {
      const result = await representative.build();
      assertDominatingReads(representative.name, result);
    }
  });

  it("every modified rule suppresses frame generation exactly once and every framed rule is unmodified", async () => {
    for (const representative of bodyLoweredRepresentativePrograms()) {
      const { result, declaredRules } = await representative.build();
      assertFrameSuppression(representative.name, result, declaredRules);
    }
  });

  it("unsupported loops and branch shapes return targeted diagnostics with no equations or frames", async () => {
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
        reason: /foreach Shape A summary assignment target must be a property/u,
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
  });

  it("the table-driven corpus satisfies all SSA invariants for scalar, Map, Set, branch, foreach Shape A, and foreach Shape B programs", async () => {
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
  });

  it("L7 invariant: every loop-header join has exactly one preheader and one loop-back input", () => {
    for (const testCase of loopInvariantCases()) {
      assertLoopInvariantCaseSupported(testCase);
      for (const [
        programIndex,
        program,
      ] of testCase.result.programs.entries()) {
        for (const [headerIndex, header] of program.loopHeaderJoins.entries()) {
          assertClosedLoopHeaderJoin(
            `${testCase.name} [program ${programIndex}] header #${headerIndex}`,
            header,
          );
        }
        for (const [bodyIndex, body] of program.loopBodies.entries()) {
          for (const [headerIndex, header] of body.headerJoins.entries()) {
            assertClosedLoopHeaderJoin(
              `${testCase.name} [program ${programIndex}] body #${bodyIndex} header #${headerIndex}`,
              header,
            );
            assert.ok(
              program.loopHeaderJoins.includes(header),
              `${testCase.name} [program ${programIndex}] body #${bodyIndex} header #${headerIndex} should be registered on the program`,
            );
          }
        }
      }
    }
  });

  it("L7 invariant: every loop body resolves break/continue continuations to the correct join", () => {
    let observedBreak = false;
    let observedContinue = false;
    let observedReturn = false;
    let observedThrow = false;

    for (const testCase of continuationLoopCases()) {
      assertLoopInvariantCaseSupported(testCase);
      for (const [
        programIndex,
        program,
      ] of testCase.result.programs.entries()) {
        for (const [bodyIndex, body] of program.loopBodies.entries()) {
          assertLoopBodyContinuationsResolved(
            `${testCase.name} [program ${programIndex}] body #${bodyIndex}`,
            body,
          );
          observedBreak ||= body.breakHandles.length > 0;
          observedContinue ||= body.continueHandles.length > 0;
          observedReturn ||= body.returnHandles.length > 0;
          observedThrow ||= body.throwHandles.length > 0;
        }
      }
      if (testCase.name.includes("valued return")) {
        assert.ok(
          testCase.result.returnValue !== undefined,
          `${testCase.name} should expose the function-level return-value continuation`,
        );
      }
    }

    assert.equal(
      observedBreak,
      true,
      "continuation fixtures should cover break",
    );
    assert.equal(
      observedContinue,
      true,
      "continuation fixtures should cover continue",
    );
    assert.equal(
      observedReturn,
      true,
      "continuation fixtures should cover return",
    );
    assert.equal(
      observedThrow,
      true,
      "continuation fixtures should cover throw",
    );
  });

  it("L7 invariant: every bounded loop carries a non-null termination metric, every fixed-point loop does not", () => {
    const observed = new Set<LoopClass>();

    for (const testCase of loopInvariantCases()) {
      assertLoopInvariantCaseSupported(testCase);
      for (const [
        programIndex,
        program,
      ] of testCase.result.programs.entries()) {
        for (const [bodyIndex, body] of program.loopBodies.entries()) {
          observed.add(testCase.loopClass);
          assertMetricMatchesLoopClass(
            `${testCase.name} [program ${programIndex}] body #${bodyIndex}`,
            testCase.loopClass,
            body,
          );
        }
      }
    }

    assert.deepEqual(
      [...observed].sort(),
      [
        "bounded-while",
        "counter",
        "fixed-point-while",
        "foreach-shape-a",
        "foreach-shape-b",
      ].sort(),
    );
  });

  it("L7 invariant: every modified rule appears in IR1SsaProgram.modifiedRules exactly once", () => {
    for (const testCase of loopInvariantCases()) {
      assertLoopInvariantCaseSupported(testCase);
      for (const [
        programIndex,
        program,
      ] of testCase.result.programs.entries()) {
        assertLoopModifiedRulesAppearOnce(
          `${testCase.name} [program ${programIndex}]`,
          program,
        );
      }
    }
  });
});
