import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import { createSourceFile } from "../src/extract.js";
import {
  type IR1Expr,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1CondStmt,
  ir1LitBool,
  ir1LitNat,
  ir1Let,
  ir1Member,
  ir1Var,
} from "../src/ir1.js";
import {
  buildScalarSsaProgram,
  isScalarSsaL1Body,
  lowerScalarSsaL1Body,
  lowerScalarSsaToProps,
  makeScalarSsaState,
} from "../src/ir1-ssa-scalars.js";
import type { OpaqueExpr } from "../src/pant-ast.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");

function loadFixture(fileName: string) {
  return createSourceFile(resolve(CONSTRUCTS_DIR, fileName));
}

async function emitFixture(fileName: string, functionName: string) {
  const sourceFile = loadFixture(fileName);
  const doc = await buildDocumentFromSourceFile(sourceFile, functionName);
  return emitAndCheck(doc);
}

function scalarEquationRhs(stmt: Parameters<typeof lowerScalarSsaToProps>[0]) {
  const result = lowerScalarSsaToProps(stmt);
  assert.deepEqual(result.diagnostics, []);
  assert.equal(result.propositions.length, 1);
  const [eq] = result.propositions;
  assert.equal(eq?.kind, "equation");
  if (eq?.kind !== "equation") {
    assert.fail("expected a scalar SSA equation");
  }
  return getAst().strExpr(eq.rhs);
}

before(async () => {
  await loadAst();
});

describe("ir1-ssa-scalars", () => {
  // PENDING Patch 2: add the scalar SSA builder state and write/version recording.
  // PENDING Patch 3: lower final scalar versions back into the current Pant equations.
  // PENDING Patch 4: route scalar assignments and scalar if-mutation through SSA.
  // PENDING Patch 5: route early-exit continuation merges through SSA.

  it("records direct property assignment as an SSA write", () => {
    const target = ir1Member(ir1Var("a"), "Account_balance");
    const stmt = ir1Assign(target, ir1LitNat(1));

    assert.equal(isScalarSsaL1Body(stmt), true);
    const program = buildScalarSsaProgram(stmt);

    assert.equal(program.reads.length, 0);
    assert.equal(program.writes.length, 1);
    assert.equal(program.joins.length, 0);
    assert.deepEqual(program.modifiedRules, ["Account_balance"]);
    assert.deepEqual(program.framedRules, []);

    const write = program.writes[0]!;
    assert.equal(write.location.kind, "property");
    assert.equal(write.location.ruleName, "Account_balance");
    assert.equal(write.location.property, "Account_balance");
    assert.equal(write.version.location, write.location);
    assert.deepEqual(write.value, { kind: "property", value: ir1LitNat(1) });
    assert.equal(scalarEquationRhs(stmt), "1");
  });

  it("versions let bindings as local-binding writes and resolves later reads", () => {
    const stmt = ir1Block([
      ir1Let("x", ir1LitNat(5)),
      ir1Assign(ir1Member(ir1Var("a"), "Account_balance"), ir1Var("x")),
    ]);
    const state = makeScalarSsaState();

    assert.equal(isScalarSsaL1Body(stmt), true);
    lowerScalarSsaL1Body(stmt, state);

    assert.equal(state.writes.length, 2);
    assert.equal(state.reads.length, 1);
    const [letWrite, propertyWrite] = state.writes;
    assert.equal(letWrite!.location.kind, "local-binding");
    assert.equal(letWrite!.location.name, "x");
    assert.equal(letWrite!.version.location, letWrite!.location);
    assert.deepEqual(letWrite!.value, {
      kind: "local-binding",
      value: ir1LitNat(5),
    });
    assert.equal(state.reads[0]!.location, letWrite!.location);
    assert.equal(state.reads[0]!.version, letWrite!.version);
    assert.equal(state.reads[0]!.dominated, true);
    assert.equal(propertyWrite!.location.kind, "property");
  });

  it("keeps branch-local let bindings out of scalar joins", () => {
    const stmt = ir1CondStmt([[ir1Var("g"), ir1Let("x", ir1LitNat(5))]], null);
    const state = makeScalarSsaState();

    assert.equal(isScalarSsaL1Body(stmt), true);
    lowerScalarSsaL1Body(stmt, state);

    assert.equal(state.writes.length, 1);
    assert.equal(state.writes[0]!.location.kind, "local-binding");
    assert.equal(state.joins.length, 0);
    assert.equal(state.writtenKeys.size, 0);
    assert.equal(
      [...state.locations.values()].some(
        (location) => location.kind === "local-binding",
      ),
      false,
    );
  });

  it("resolves compound property assignment through the dominating prior version", () => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const stmt = ir1Block([
      ir1Assign(balance, ir1LitNat(1)),
      ir1Assign(balance, ir1Binop("add", balance, ir1LitNat(2))),
    ]);

    assert.equal(isScalarSsaL1Body(stmt), true);
    const program = buildScalarSsaProgram(stmt);

    assert.equal(program.writes.length, 2);
    assert.equal(program.reads.length, 1);
    const [firstWrite, secondWrite] = program.writes;
    const read = program.reads[0]!;
    assert.equal(read.location, firstWrite!.location);
    assert.equal(read.version, firstWrite!.version);
    assert.equal(read.dominated, true);
    assert.equal(secondWrite!.version.location, firstWrite!.location);
    assert.equal(scalarEquationRhs(stmt), "1 + 2");
  });

  it("resolves initial property values through normalized receivers", () => {
    const ast = getAst();
    const balance = ir1Member(ir1Var("x"), "Account_balance");
    const lowerOpaque = (expr: OpaqueExpr): OpaqueExpr => {
      switch (ast.strExpr(expr)) {
        case "x":
          return ast.var("a");
        case "Account_balance x":
          return ast.app(ast.var("Account_balance"), [ast.var("a")]);
        default:
          return expr;
      }
    };
    const result = lowerScalarSsaToProps(
      ir1Assign(balance, ir1Binop("add", balance, ir1LitNat(2))),
      {
        lowerOpaque,
        initialPropertyValues: new Map([
          ["Account_balance::a", ast.litNat(10)],
        ]),
      },
    );

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.propositions.length, 1);
    const [eq] = result.propositions;
    assert.equal(eq?.kind, "equation");
    if (eq?.kind !== "equation") {
      assert.fail("expected a scalar SSA equation");
    }
    assert.equal(ast.strExpr(eq.rhs), "10 + 2");
  });

  it("joins a single-arm if against the initial property version", () => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const stmt = ir1CondStmt(
      [[ir1Var("g"), ir1Assign(balance, ir1LitNat(1))]],
      null,
    );

    assert.equal(isScalarSsaL1Body(stmt), true);
    const program = buildScalarSsaProgram(stmt);

    assert.equal(program.writes.length, 1);
    assert.equal(program.joins.length, 1);
    const write = program.writes[0]!;
    const join = program.joins[0]!;
    assert.equal(join.location, write.location);
    assert.equal(join.thenVersion, write.version);
    assert.equal(join.elseVersion.origin, "initial");
    assert.equal(join.elseVersion.location, write.location);
    assert.equal(join.joinVersion.location, write.location);
    assert.notEqual(join.thenVersion, join.elseVersion);
    assert.equal(
      scalarEquationRhs(stmt),
      "cond g => 1, true => Account_balance a",
    );
  });

  it("lowers nested scalar if bodies through SSA joins", () => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const inner = ir1CondStmt(
      [[ir1Var("y"), ir1Assign(balance, ir1LitNat(1))]],
      ir1Assign(balance, ir1LitNat(2)),
    );
    const outer = ir1CondStmt(
      [[ir1Var("x"), inner]],
      ir1Assign(balance, ir1LitNat(3)),
    );

    assert.equal(isScalarSsaL1Body(outer), true);
    const program = buildScalarSsaProgram(outer);

    assert.equal(program.writes.length, 3);
    assert.equal(program.joins.length, 2);
    const [innerJoin, outerJoin] = program.joins;
    assert.equal(innerJoin!.thenVersion, program.writes[0]!.version);
    assert.equal(innerJoin!.elseVersion, program.writes[1]!.version);
    assert.equal(outerJoin!.thenVersion, innerJoin!.joinVersion);
    assert.equal(outerJoin!.elseVersion, program.writes[2]!.version);
    assert.equal(outerJoin!.joinVersion.location, innerJoin!.location);
    assert.equal(
      scalarEquationRhs(outer),
      "cond x => (cond y => 1, true => 2), true => 3",
    );
  });

  it("rejects non-scalar routing shapes", () => {
    const mapReadReceiver: IR1Expr = {
      kind: "map-read",
      op: "get",
      ruleName: "Cache_value",
      keyPredName: "Cache_hasKey",
      ownerType: "Owner",
      keyType: "Key",
      receiver: ir1Var("cache"),
      key: ir1Var("key"),
    };

    assert.equal(
      isScalarSsaL1Body(ir1Assign(ir1Var("x"), ir1LitNat(1))),
      false,
    );
    assert.equal(
      isScalarSsaL1Body(
        ir1Assign(ir1Member(mapReadReceiver, "Account_balance"), ir1LitNat(1)),
      ),
      false,
    );
    assert.equal(
      isScalarSsaL1Body(
        ir1CondStmt(
          [
            [
              ir1Var("g"),
              ir1Assign(ir1Member(ir1Var("a"), "A_x"), ir1LitNat(1)),
            ],
            [
              ir1Var("h"),
              ir1Assign(ir1Member(ir1Var("a"), "A_x"), ir1LitNat(2)),
            ],
          ],
          null,
        ),
      ),
      false,
    );
    assert.equal(
      isScalarSsaL1Body({
        kind: "expr-stmt",
        expr: ir1LitBool(true),
      }),
      false,
    );
  });

  it("preserves declared frame rules first seen inside scalar branches", () => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const limit = ir1Member(ir1Var("a"), "Account_limit");
    const stmt = ir1CondStmt(
      [[ir1Var("g"), ir1Assign(balance, ir1Binop("add", limit, ir1LitNat(1)))]],
      null,
    );

    const program = buildScalarSsaProgram(stmt);

    assert.deepEqual(
      new Set(program.declaredRules),
      new Set(["Account_balance", "Account_limit"]),
    );
    assert.deepEqual(program.modifiedRules, ["Account_balance"]);
    assert.deepEqual(program.framedRules, ["Account_limit"]);
  });

  it.skip("preserves translateBody parity for scalar sequential write/read fixtures", async () => {
    const output = await emitFixture(
      "functions-mutating-conditional.ts",
      "accumulateIf",
    );

    // PENDING Patch 4: this fixture should still emit the same Pantagruel
    // text after the scalar SSA route becomes active.
    assert.match(
      output,
      /account--balance' a = \(cond g => 10 \+ 5, true => 10\)\./u,
    );
  });

  it("preserves translateBody parity for asymmetric branch write fixtures", async () => {
    const output = await emitFixture(
      "functions-mutating-conditional.ts",
      "asymmetric",
    );

    assert.match(
      output,
      /account--balance' a = \(cond g => 0, true => account--balance a\)\./u,
    );
    assert.match(
      output,
      /account--owner' a = \(cond g => account--owner a, true => new-owner\)\./u,
    );
  });

  it("preserves translateBody parity for chained early-return fixtures", async () => {
    const output = await emitFixture(
      "functions-mutating-early-exit.ts",
      "chainedEarlyReturns",
    );

    assert.match(
      output,
      /account--balance' a = \(cond ~g => \(cond ~h => 1, true => account--balance a\), true => account--balance a\)\./u,
    );
  });

  it("preserves translateBody parity for else-branch early-return fixtures", async () => {
    const output = await emitFixture(
      "functions-mutating-early-exit.ts",
      "elseBranchReturn",
    );

    assert.match(
      output,
      /account--balance' a = \(cond g => v, true => account--balance a\)\./u,
    );
  });

  it("preserves translateBody parity for write-then-early-return fixtures", async () => {
    const output = await emitFixture(
      "functions-mutating-early-exit.ts",
      "writeThenEarlyReturn",
    );

    assert.match(
      output,
      /account--balance' a = \(cond ~g => 10 \+ 5, true => 10\)\./u,
    );
  });
});
