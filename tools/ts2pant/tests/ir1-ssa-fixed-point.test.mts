// @archlint.module test
// @archlint.domain ts2pant.ir1-ssa-fixed-point

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import * as fc from "fast-check";

import { emitDocument } from "../src/emit.js";
import type { IR1Expr, IR1Stmt } from "../src/ir1.js";
import { lowerL1BodyToSsaProps } from "../src/ir1-lower-body.js";
import {
  lowerFixedPointLoopL1Body,
  recognizeFixedPointLoopShape,
} from "../src/ir1-ssa-fixed-point.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { newSynthCell, RealStrategy } from "../src/translate-types.js";

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
  it("generated fixed-point variants are recognized and lowered", () => {
    fc.assert(
      fc.property(fc.constantFrom("target", "limit"), (rhsName) => {
        const stmt = {
          ...fixedPointWhile(),
          cond: {
            kind: "binop",
            op: "lt",
            lhs: balanceMember,
            rhs: { kind: "var", name: rhsName },
          } satisfies IR1Expr,
        };
        const shape = recognizeFixedPointLoopShape(stmt);
        assert.ok(!("unsupported" in shape));
        const result = lowerFixedPointLoopL1Body(stmt);
        assert.deepEqual(result.diagnostics, []);
        assert.equal(result.programs.length, 1);
      }),
    );
  });

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
    // Header (above `---`): bare rule declaration, no body.
    assert.match(
      source,
      /fn--loop s: Nat0, step: Nat0, target: Nat0 => Nat0\./u,
    );
    // Body (below `---`): equation form with the cond body.
    assert.match(source, /fn--loop s step target = cond/u);
    assert.match(source, /balance' a = fn--loop \(balance a\) step target\./u);
  });

  it("allocates a fresh state binder when s is a loop invariant", () => {
    const ast = getAst();
    const sVar: IR1Expr = { kind: "var", name: "s" };
    const result = lowerFixedPointLoopL1Body({
      ...fixedPointWhile(),
      cond: { kind: "binop", op: "lt", lhs: balanceMember, rhs: sVar },
    });
    const ruleDecl = result.propositions[0]!;

    assert.equal(ruleDecl.kind, "rule-decl");
    assert.deepEqual(
      ruleDecl.params.map((p) => p.name),
      ["s1", "s", "step"],
    );
    assert.match(ast.strExpr(ruleDecl.body), /cond s1 < s =>/u);
  });

  it("inlines let-then-while preludes transitively before collecting invariants", () => {
    const ast = getAst();
    const result = lowerFixedPointLoopL1Body({
      kind: "block",
      stmts: [
        {
          kind: "let",
          name: "limit",
          value: { kind: "binop", op: "add", lhs: targetVar, rhs: stepVar },
        },
        {
          kind: "let",
          name: "cap",
          value: {
            kind: "binop",
            op: "add",
            lhs: { kind: "var", name: "limit" },
            rhs: stepVar,
          },
        },
        {
          ...fixedPointWhile(),
          cond: {
            kind: "binop",
            op: "lt",
            lhs: balanceMember,
            rhs: { kind: "var", name: "cap" },
          },
        },
      ],
    });
    const ruleDecl = result.propositions[0]!;

    assert.equal(ruleDecl.kind, "rule-decl");
    assert.deepEqual(
      ruleDecl.params.map((p) => p.name),
      ["s", "step", "target"],
    );
    assert.doesNotMatch(ast.strExpr(ruleDecl.body), /\b(limit|cap)\b/u);
    assert.match(ast.strExpr(ruleDecl.body), /target \+ step \+ step/u);
  });

  it("does not treat target-shaped member reads under each binders as the loop target", () => {
    const result = lowerFixedPointLoopL1Body({
      ...fixedPointWhile(),
      body: {
        kind: "assign",
        target: balanceMember,
        value: {
          kind: "binop",
          op: "add",
          lhs: balanceMember,
          rhs: {
            kind: "unop",
            op: "card",
            arg: {
              kind: "each",
              binder: "a",
              src: { kind: "var", name: "accounts" },
              guards: [],
              proj: {
                kind: "member",
                receiver: { kind: "var", name: "a" },
                name: "balance",
              },
            },
          },
        },
      },
    });

    assert.match(
      result.diagnostics[0]?.reason ?? "",
      /supports guards and updates over the mutated property only/u,
    );
  });

  it("uses the configured numeric strategy for default helper types", () => {
    const ast = getAst();
    const result = lowerFixedPointLoopL1Body(fixedPointWhile(), {
      strategy: RealStrategy,
    });
    const ruleDecl = result.propositions[0]!;

    assert.equal(ruleDecl.kind, "rule-decl");
    assert.equal(ast.strTypeExpr(ruleDecl.returnType), "Real");
    assert.deepEqual(
      ruleDecl.params.map((p) => ast.strTypeExpr(p.type)),
      ["Real", "Real", "Real"],
    );
  });

  it("infers helper state and invariant types from declarations", () => {
    const ast = getAst();
    const flagMember: IR1Expr = {
      kind: "member",
      receiver: aVar,
      name: "flag",
    };
    const result = lowerFixedPointLoopL1Body(
      {
        kind: "while",
        cond: {
          kind: "binop",
          op: "and",
          lhs: flagMember,
          rhs: { kind: "var", name: "enabled" },
        },
        body: {
          kind: "assign",
          target: flagMember,
          value: { kind: "lit", value: { kind: "bool", value: false } },
        },
      },
      {
        declarations: [
          { kind: "domain", name: "Account" },
          {
            kind: "rule",
            name: "flag",
            params: [{ name: "a", type: "Account" }],
            returnType: "Bool",
          },
          {
            kind: "action",
            label: "Run",
            params: [
              { name: "a", type: "Account" },
              { name: "enabled", type: "Bool" },
            ],
          },
        ],
      },
    );
    const ruleDecl = result.propositions[0]!;

    assert.equal(ruleDecl.kind, "rule-decl");
    assert.equal(ast.strTypeExpr(ruleDecl.returnType), "Bool");
    assert.deepEqual(
      ruleDecl.params.map((p) => ast.strTypeExpr(p.type)),
      ["Bool", "Bool"],
    );
  });

  it("threads declaration-derived types through the body-level adapter", () => {
    const ast = getAst();
    const flagMember: IR1Expr = {
      kind: "member",
      receiver: aVar,
      name: "flag",
    };
    const result = lowerL1BodyToSsaProps(
      {
        kind: "while",
        cond: flagMember,
        body: {
          kind: "assign",
          target: flagMember,
          value: { kind: "lit", value: { kind: "bool", value: false } },
        },
      },
      [
        { kind: "domain", name: "Account" },
        {
          kind: "rule",
          name: "flag",
          params: [{ name: "a", type: "Account" }],
          returnType: "Bool",
        },
        {
          kind: "action",
          label: "Run",
          params: [{ name: "a", type: "Account" }],
        },
      ],
      { applyConst: (e) => e },
    );
    const ruleDecl = result.propositions[0]!;

    assert.equal(ruleDecl.kind, "rule-decl");
    assert.equal(ast.strTypeExpr(ruleDecl.returnType), "Bool");
    assert.deepEqual(
      ruleDecl.params.map((p) => ast.strTypeExpr(p.type)),
      ["Bool"],
    );
  });

  describe("handle-list consumption", () => {
    it("break-handle merge produces post-loop cond per location", () => {
      const ast = getAst();
      const result = lowerFixedPointLoopL1Body({
        ...fixedPointWhile(),
        body: {
          kind: "block",
          stmts: [
            fixedPointWhile().body,
            {
              kind: "cond-stmt",
              arms: [
                [
                  {
                    kind: "binop",
                    op: "ge",
                    lhs: balanceMember,
                    rhs: targetVar,
                  },
                  { kind: "break" },
                ],
              ],
              otherwise: null,
            },
          ],
        },
      });

      assert.deepEqual(result.diagnostics, []);
      assert.equal(result.programs[0]!.loopBodies[0]!.breakHandles.length, 1);
      const ruleDecl = result.propositions[0]!;
      assert.equal(ruleDecl.kind, "rule-decl");
      assert.match(ast.strExpr(ruleDecl.body), /cond .* >= target =>/u);
    });

    it("continue-handle threads into header phi loop-back input", () => {
      const ast = getAst();
      const result = lowerFixedPointLoopL1Body({
        ...fixedPointWhile(),
        body: {
          kind: "block",
          stmts: [
            {
              kind: "cond-stmt",
              arms: [
                [
                  {
                    kind: "binop",
                    op: "eq",
                    lhs: stepVar,
                    rhs: { kind: "lit", value: { kind: "nat", value: 0 } },
                  },
                  { kind: "continue" },
                ],
              ],
              otherwise: null,
            },
            fixedPointWhile().body,
          ],
        },
      });

      assert.deepEqual(result.diagnostics, []);
      assert.equal(
        result.programs[0]!.loopBodies[0]!.continueHandles.length,
        1,
      );
      const ruleDecl = result.propositions[0]!;
      assert.equal(ruleDecl.kind, "rule-decl");
      assert.match(ast.strExpr(ruleDecl.body), /cond step = 0 => s/u);
    });

    it("return-handle produces function-level return-value cond", () => {
      const ast = getAst();
      const result = lowerFixedPointLoopL1Body(
        {
          ...fixedPointWhile(),
          body: {
            kind: "block",
            stmts: [
              fixedPointWhile().body,
              {
                kind: "cond-stmt",
                arms: [
                  [
                    {
                      kind: "binop",
                      op: "ge",
                      lhs: balanceMember,
                      rhs: targetVar,
                    },
                    { kind: "return", expr: balanceMember },
                  ],
                ],
                otherwise: null,
              },
            ],
          },
        },
        { returnRuleName: "update" },
      );

      assert.deepEqual(result.diagnostics, []);
      assert.equal(result.programs[0]!.loopBodies[0]!.returnHandles.length, 1);
      assert.equal(result.returnValue?.ruleName, "update");
      assert.match(
        ast.strExpr(result.returnValue!.expression),
        /cond balance a >= target => balance a/u,
      );
    });

    it("throw-handle conjoins precondition with recursive-rule guard", () => {
      const ast = getAst();
      const result = lowerFixedPointLoopL1Body({
        ...fixedPointWhile(),
        body: {
          kind: "block",
          stmts: [
            {
              kind: "cond-stmt",
              arms: [
                [
                  {
                    kind: "binop",
                    op: "eq",
                    lhs: stepVar,
                    rhs: { kind: "lit", value: { kind: "nat", value: 0 } },
                  },
                  {
                    kind: "throw",
                    expr: {
                      kind: "lit",
                      value: { kind: "string", value: "bad" },
                    },
                  },
                ],
              ],
              otherwise: null,
            },
            fixedPointWhile().body,
          ],
        },
      });

      assert.deepEqual(result.diagnostics, []);
      assert.equal(result.programs[0]!.loopBodies[0]!.throwHandles.length, 1);
      const ruleDecl = result.propositions[0]!;
      assert.equal(ruleDecl.kind, "rule-decl");
      assert.match(ast.strExpr(ruleDecl.body), /~\(step = 0\)/u);
    });
  });

  describe("literal-true rejection", () => {
    it("narrows to while(true) with no reachable break/return", () => {
      const rejected = recognizeFixedPointLoopShape({
        ...fixedPointWhile(),
        cond: { kind: "lit", value: { kind: "bool", value: true } },
      });
      assert.ok("unsupported" in rejected);

      const accepted = recognizeFixedPointLoopShape({
        ...fixedPointWhile(),
        cond: { kind: "lit", value: { kind: "bool", value: true } },
        body: {
          kind: "block",
          stmts: [fixedPointWhile().body, { kind: "break" }],
        },
      });
      assert.ok(!("unsupported" in accepted));
    });
  });
});
