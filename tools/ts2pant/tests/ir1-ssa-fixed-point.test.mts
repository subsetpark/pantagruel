import assert from "node:assert/strict";
import { before, describe, it } from "node:test";

import { emitDocument } from "../src/emit.js";
import type { IR1Expr, IR1Stmt } from "../src/ir1.js";
import {
  lowerFixedPointLoopL1Body,
  recognizeFixedPointLoopShape,
} from "../src/ir1-ssa-fixed-point.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { newSynthCell } from "../src/translate-types.js";

const aVar: IR1Expr = { kind: "var", name: "a" };
const balanceMember: IR1Expr = {
  kind: "member",
  receiver: aVar,
  name: "balance",
};
const targetVar: IR1Expr = { kind: "var", name: "target" };
const stepVar: IR1Expr = { kind: "var", name: "step" };

before(async () => {
  await loadAst();
});

function fixedPointWhile(): Extract<IR1Stmt, { kind: "while" }> {
  return {
    kind: "while",
    cond: { kind: "binop", op: "lt", lhs: balanceMember, rhs: targetVar },
    body: {
      kind: "assign",
      target: balanceMember,
      value: { kind: "binop", op: "add", lhs: balanceMember, rhs: stepVar },
    },
  };
}

describe("ir1-ssa-fixed-point", () => {
  it("recognizes a single-location while-loop fixed-point shape", () => {
    const result = recognizeFixedPointLoopShape(fixedPointWhile());

    assert.ok(!("unsupported" in result));
    assert.equal(result.guardExpr.kind, "binop");
    assert.equal(result.bodyStmt.kind, "assign");
    assert.equal(result.mutatedLocation.kind, "property");
    assert.equal(result.mutatedLocation.ruleName, "balance");
  });

  it("rejects literal-true guards at the recognizer level", () => {
    const result = recognizeFixedPointLoopShape({
      ...fixedPointWhile(),
      cond: { kind: "lit", value: { kind: "bool", value: true } },
    });

    assert.ok("unsupported" in result);
    assert.match(result.unsupported, /literal-true guard/u);
  });

  it("rejects multi-location while bodies", () => {
    const result = recognizeFixedPointLoopShape({
      ...fixedPointWhile(),
      body: {
        kind: "block",
        stmts: [
          {
            kind: "assign",
            target: balanceMember,
            value: { kind: "lit", value: { kind: "nat", value: 1 } },
          },
          {
            kind: "assign",
            target: { kind: "member", receiver: aVar, name: "limit" },
            value: { kind: "lit", value: { kind: "nat", value: 2 } },
          },
        ],
      },
    });

    assert.ok("unsupported" in result);
    assert.match(result.unsupported, /single-location bodies only/u);
    assert.match(result.unsupported, /2 rules/u);
  });

  it("constructs IR1SsaLoopBody with terminationMetric: null", () => {
    const result = lowerFixedPointLoopL1Body(fixedPointWhile());

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.programs.length, 1);
    assert.equal(result.programs[0]!.loopBodies.length, 1);
    assert.equal(result.programs[0]!.loopBodies[0]!.terminationMetric, null);
  });

  it("synthesises a uniquely-named recursive helper rule via NameRegistry", () => {
    const synthCell = newSynthCell();
    const first = lowerFixedPointLoopL1Body(fixedPointWhile(), { synthCell });
    const second = lowerFixedPointLoopL1Body(fixedPointWhile(), { synthCell });
    const firstDecl = first.propositions[0]!;
    const secondDecl = second.propositions[0]!;

    assert.equal(firstDecl.kind, "rule-decl");
    assert.equal(secondDecl.kind, "rule-decl");
    assert.equal(firstDecl.ruleName, "fn--loop");
    assert.equal(secondDecl.ruleName, "fn--loop1");
  });

  it("emits a rule-decl PropResult for the helper and an equation PropResult for the caller", () => {
    const ast = getAst();
    const result = lowerFixedPointLoopL1Body(fixedPointWhile(), {
      locationType: ast.tName("Nat0"),
      invariantTypes: new Map([
        ["step", ast.tName("Nat0")],
        ["target", ast.tName("Nat0")],
      ]),
    });

    assert.deepEqual(
      result.propositions.map((p) => p.kind),
      ["rule-decl", "equation"],
    );
    const ruleDecl = result.propositions[0]!;
    const equation = result.propositions[1]!;
    assert.equal(ruleDecl.kind, "rule-decl");
    assert.equal(equation.kind, "equation");
    assert.deepEqual(
      ruleDecl.params.map((p) => p.name),
      ["s", "step", "target"],
    );

    const source = emitDocument({
      moduleName: "FixedPointTest",
      imports: [],
      declarations: [
        { kind: "domain", name: "Account" },
        {
          kind: "rule",
          name: "balance",
          params: [{ name: "a", type: "Account" }],
          returnType: "Nat0",
        },
        {
          kind: "action",
          label: "Run",
          params: [
            { name: "a", type: "Account" },
            { name: "step", type: "Nat0" },
            { name: "target", type: "Nat0" },
          ],
        },
      ],
      propositions: result.propositions,
      checks: [],
    });
    assert.match(
      source,
      /fn--loop s: Nat0, step: Nat0, target: Nat0 => Nat0\./u,
    );
    assert.match(source, /balance' a = fn--loop \(balance a\) step target\./u);
  });
});
