import assert from "node:assert/strict";
import { before, describe, it } from "node:test";

import {
  ir1Assign,
  ir1Block,
  ir1LitNat,
  ir1MapDelete,
  ir1MapSet,
  ir1Member,
  ir1SetAddOrDelete,
  ir1SetClear,
  ir1Var,
} from "../src/ir1.js";
import { lowerCollectionSsaToResult } from "../src/ir1-ssa-collections.js";
import {
  appendFramesForUnmodifiedRules,
  ir1SsaBodyLowerSuccess,
} from "../src/ir1-ssa-lower.js";
import { lowerScalarSsaToProps } from "../src/ir1-ssa-scalars.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import type { PropResult } from "../src/types.js";

type UnsupportedDiagnostic = Extract<PropResult, { kind: "unsupported" }>;

interface PendingUnifiedLowerResult {
  propositions: PropResult[];
  modifiedRules: string[];
  framedRules: string[];
  diagnostics: UnsupportedDiagnostic[];
}

function pendingUnifiedLowerResult(input: {
  declaredRules?: readonly string[];
  propositions?: readonly PropResult[];
  modifiedRules?: readonly string[];
  diagnostics?: readonly UnsupportedDiagnostic[];
}): PendingUnifiedLowerResult {
  const diagnostics = [...(input.diagnostics ?? [])];
  const modifiedRules = [...new Set(input.modifiedRules ?? [])];
  const declaredRules = [...new Set(input.declaredRules ?? [])];
  const modifiedRuleSet = new Set(modifiedRules);
  const framedRules =
    diagnostics.length > 0
      ? []
      : declaredRules.filter((rule) => !modifiedRuleSet.has(rule));
  return {
    propositions: [...(input.propositions ?? [])],
    modifiedRules,
    framedRules,
    diagnostics,
  };
}

function findEquation(
  propositions: readonly PropResult[],
  prefix: string,
): Extract<PropResult, { kind: "equation" }> | undefined {
  const ast = getAst();
  return propositions.find(
    (p): p is Extract<PropResult, { kind: "equation" }> =>
      p.kind === "equation" && ast.strExpr(p.lhs).startsWith(prefix),
  );
}

before(async () => {
  await loadAst();
});

describe("ir1-ssa-propresult-lowering", () => {
  it.skip("scalar properties lower through one result", () => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const scalar = lowerScalarSsaToProps(
      ir1Block([
        ir1Assign(balance, ir1LitNat(1)),
        ir1Assign(balance, ir1LitNat(3)),
      ]),
      { declaredRules: ["Account_balance", "Account_limit"] },
    );
    const result = pendingUnifiedLowerResult({
      declaredRules: scalar.program.declaredRules,
      propositions: scalar.propositions,
      modifiedRules: scalar.modifiedRules,
      diagnostics: scalar.diagnostics,
    });
    const ast = getAst();
    const equation = findEquation(result.propositions, "Account_balance'");

    assert.deepEqual(result.diagnostics, []);
    assert.deepEqual(result.modifiedRules, ["Account_balance"]);
    assert.deepEqual(result.framedRules, ["Account_limit"]);
    assert.equal(scalar.finalProperties.length, 1);
    assert.ok(equation, "expected the unified result to carry the final write");
    if (equation !== undefined) {
      assert.equal(ast.strExpr(equation.lhs), "Account_balance' a");
      assert.equal(ast.strExpr(equation.rhs), "3");
    }
  });

  it.skip(
    "collections lower value and membership props through one result",
    () => {
      const stmt = ir1Block([
        ir1MapSet(
          "Cache_value",
          "Cache_hasKey",
          "Owner",
          "Key",
          ir1Var("cache"),
          ir1Var("key"),
          ir1LitNat(7),
        ),
        ir1MapDelete(
          "Cache_value",
          "Cache_hasKey",
          "Owner",
          "Key",
          ir1Var("cache"),
          ir1Var("otherKey"),
        ),
        ir1SetAddOrDelete(
          "add",
          "Owner_tags",
          "Owner",
          "Tag",
          ir1Var("owner"),
          ir1Var("x"),
        ),
        ir1SetAddOrDelete(
          "delete",
          "Owner_tags",
          "Owner",
          "Tag",
          ir1Var("owner"),
          ir1Var("x"),
        ),
        ir1SetClear("Owner_tags", "Owner", "Tag", ir1Var("owner")),
        ir1SetAddOrDelete(
          "add",
          "Owner_tags",
          "Owner",
          "Tag",
          ir1Var("owner"),
          ir1Var("y"),
        ),
      ]);

      const lowered = lowerCollectionSsaToResult(stmt, {
        declaredRules: ["Owner_name"],
      });
      const result = pendingUnifiedLowerResult({
        declaredRules: lowered.program.declaredRules,
        propositions: lowered.propositions,
        modifiedRules: lowered.modifiedRules,
        diagnostics: lowered.diagnostics,
      });
      const ast = getAst();
      const valueEq = findEquation(result.propositions, "Cache_value'");
      const membershipEq = findEquation(result.propositions, "Cache_hasKey'");
      const setAssertion = result.propositions.find(
        (p): p is Extract<PropResult, { kind: "assertion" }> =>
          p.kind === "assertion",
      );

      assert.deepEqual(result.diagnostics, []);
      assert.deepEqual(
        new Set(result.modifiedRules),
        new Set(["Cache_value", "Cache_hasKey", "Owner_tags"]),
      );
      assert.deepEqual(result.framedRules, ["Owner_name"]);
      assert.ok(valueEq, "expected a final Map value proposition");
      assert.ok(membershipEq, "expected a final Map membership proposition");
      assert.ok(setAssertion, "expected a final Set membership proposition");
      if (valueEq !== undefined) {
        assert.match(ast.strExpr(valueEq.rhs), /Cache_value.*->\s*7/u);
      }
      if (membershipEq !== undefined) {
        assert.match(ast.strExpr(membershipEq.rhs), /Cache_hasKey.*->\s*true/u);
        assert.match(ast.strExpr(membershipEq.rhs), /Cache_hasKey.*->\s*false/u);
      }
      if (setAssertion !== undefined) {
        assert.equal(
          ast.strExpr(setAssertion.body),
          "y1 in Owner_tags' owner <-> (cond y1 = y => true, true => false)",
        );
      }
    },
  );

  it("frames derive from result modified rules", () => {
    const result = appendFramesForUnmodifiedRules(
      ir1SsaBodyLowerSuccess({
        modifiedRules: ["Account_balance", "Account_balance"],
      }),
      [
        {
          kind: "rule",
          name: "Account_balance",
          params: [{ name: "account", type: "Account" }],
          returnType: "Nat",
        },
        {
          kind: "rule",
          name: "Account_limit",
          params: [{ name: "account", type: "Account" }],
          returnType: "Nat",
        },
        {
          kind: "rule",
          name: "Account_limit",
          params: [{ name: "account", type: "Account" }],
          returnType: "Nat",
        },
      ],
    );
    const ast = getAst();
    const framedRules = result.propositions
      .filter((p) => p.kind === "equation")
      .map((p) => ast.strExpr(p.lhs).replace(/'.*/u, ""));

    assert.deepEqual(result.modifiedRules, ["Account_balance"]);
    assert.deepEqual(framedRules, ["Account_limit"]);
    assert.equal(
      framedRules.filter((rule) => rule === "Account_limit").length,
      1,
    );
  });

  it("unsupported diagnostics suppress frame emission", () => {
    const unsupported = lowerCollectionSsaToResult(
      ir1Assign(ir1Var("x"), ir1LitNat(1)),
      { declaredRules: ["Account_balance", "Account_limit"] },
    );
    const result = appendFramesForUnmodifiedRules(
      {
        programs: [unsupported.program],
        finalProperties: unsupported.finalProperties,
        propositions: unsupported.propositions,
        modifiedRules: unsupported.modifiedRules,
        diagnostics: unsupported.diagnostics,
      },
      [
        {
          kind: "rule",
          name: "Account_balance",
          params: [{ name: "account", type: "Account" }],
          returnType: "Nat",
        },
        {
          kind: "rule",
          name: "Account_limit",
          params: [{ name: "account", type: "Account" }],
          returnType: "Nat",
        },
      ],
    );
    const pendingEquivalent = pendingUnifiedLowerResult({
      declaredRules: unsupported.program.declaredRules,
      propositions: unsupported.propositions,
      modifiedRules: unsupported.modifiedRules,
      diagnostics: unsupported.diagnostics,
    });

    assert.equal(result.diagnostics.length, 1);
    assert.deepEqual(result.modifiedRules, []);
    assert.deepEqual(result.propositions, []);
    assert.deepEqual(pendingEquivalent.framedRules, []);
    assert.equal(
      result.diagnostics[0]?.reason,
      "collection SSA assignment target must be a property member",
    );
  });
});
