import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import { emitDocument } from "../src/emit.js";
import { createSourceFile } from "../src/extract.js";
import {
  ir1Assign,
  ir1Binop,
  ir1Foreach,
  ir1LitBool,
  ir1Member,
  ir1Var,
} from "../src/ir1.js";
import { lowerL1BodyToSsaProps } from "../src/ir1-lower-body.js";
import { buildLoopSsaProgram } from "../src/ir1-ssa-loops.js";
import { loadAst } from "../src/pant-wasm.js";
import { buildDocumentFromSourceFile } from "./helpers.mts";

before(async () => {
  await loadAst();
});

function extractEquationRhs(output: string): string {
  const equationLine = output
    .trim()
    .split("\n")
    .findLast((line) => line.includes(" = "));
  assert.ok(equationLine, "expected an emitted equation line");
  return equationLine.slice(equationLine.indexOf(" = ") + 3);
}

describe("ir1-ssa-loops", () => {
  // PENDING Patch 2: introduce the dedicated loop-summary SSA helper and
  // summary/version bookkeeping for supported loop-like constructs.
  // PENDING Patch 4: route foreach Shape B lowering through general-loop SSA.

  it("reports unsupported loop summary inputs without summaries", () => {
    const result = buildLoopSsaProgram(
      {
        kind: "unsupported",
        reason: "general loops are not supported by loop-summary SSA",
      },
      { declaredRules: ["Account_total"] },
    );

    assert.equal(result.program.loopSummaries.length, 0);
    assert.deepEqual(result.program.modifiedRules, []);
    assert.deepEqual(result.program.framedRules, ["Account_total"]);
    assert.deepEqual(result.diagnostics, [
      {
        kind: "unsupported",
        reason: "general loops are not supported by loop-summary SSA",
      },
    ]);
  });

  it("preserves unsupported diagnostics for out-of-scope loops", () => {
    const result = buildLoopSsaProgram([
      {
        kind: "unsupported",
        reason: "general loops are not supported by loop-summary SSA",
      },
      {
        kind: "unsupported",
        reason:
          "nested proposition-emitting loop body is not supported — the inner proposition would escape the outer iterator scope",
      },
      {
        kind: "unsupported",
        reason: "Map mutation inside foreach body is out of scope",
      },
    ]);

    assert.equal(result.program.loopSummaries.length, 0);
    assert.deepEqual(result.program.modifiedRules, []);
    assert.deepEqual(
      result.diagnostics.map((d) => d.reason),
      [
        "general loops are not supported by loop-summary SSA",
        "nested proposition-emitting loop body is not supported — the inner proposition would escape the outer iterator scope",
        "Map mutation inside foreach body is out of scope",
      ],
    );
  });

  it("preserves production parity for mu-search fixtures", async () => {
    const filePath = resolve(
      import.meta.dirname,
      "fixtures/constructs/expressions-while-mu-search.ts",
    );
    const sourceFile = createSourceFile(filePath);
    const funcs = [
      "firstUnusedSuffix",
      "unbracedWhileBody",
      "compoundIncrementStep",
      "explicitIncrementStep",
      "explicitIncrementStepFlipped",
    ];

    const outputs = await Promise.all(
      funcs.map(async (funcName) => {
        const doc = await buildDocumentFromSourceFile(sourceFile, funcName);
        return emitDocument(doc);
      }),
    );

    const rhsOutputs = outputs.map(extractEquationRhs);
    for (const rhs of rhsOutputs) {
      assert.match(rhs, /min over each j\d*: Int/u);
    }
    for (const rhs of rhsOutputs.slice(1)) {
      assert.equal(rhs, rhsOutputs[0]);
    }
  });

  it("preserves production parity for foreach fixtures", async () => {
    const filePath = resolve(
      import.meta.dirname,
      "fixtures/constructs/functions-mutating-loop.ts",
    );
    const sourceFile = createSourceFile(filePath);
    const funcs = ["forEachActivate", "forEachSum", "mixedUpdates"];

    const outputs = await Promise.all(
      funcs.map(async (funcName) => {
        const doc = await buildDocumentFromSourceFile(sourceFile, funcName);
        return emitDocument(doc);
      }),
    );

    for (const output of outputs) {
      assert.doesNotMatch(output, /UNSUPPORTED/u);
    }
    assert.match(
      outputs[0]!,
      /all ([\w$]+) in users \| user--active' \1 = true/u,
    );
    assert.match(
      outputs[1]!,
      /account--total' a = account--total a \+ \(\+ over each ([\w$]+) in items \| item--value \1\)/u,
    );
    assert.match(
      outputs[2]!,
      /all ([\w$]+) in items \| item--tagged' \1 = true/u,
    );
    assert.match(
      outputs[2]!,
      /account--total' a = account--total a \+ \(\+ over each ([\w$]+) in items \| item--value \1\)/u,
    );
  });

  it(
    "foreach Shape A call site emits IR1SsaProgram with populated loopHeaderJoins and loopBodies",
    () => {
      const result = lowerL1BodyToSsaProps(
        ir1Foreach(
          "item0",
          ir1Var("items"),
          ir1Assign(
            ir1Member(ir1Var("item0"), "Item_seen"),
            ir1LitBool(true),
          ),
        ),
        [
          {
            kind: "rule",
            name: "Item_seen",
            params: [{ name: "item0", type: "Item" }],
            returnType: "Bool",
          },
        ],
        { applyConst: (expr) => expr },
      );

      assert.deepEqual(result.diagnostics, []);
      assert.equal(result.programs.length, 1);
      const [program] = result.programs;
      assert.ok(program, "expected a Shape A SSA program");
      assert.equal(program.loopSummaries.length, 0);
      assert.ok(program.loopHeaderJoins.length > 0);
      assert.ok(program.loopBodies.length > 0);
      assert.deepEqual(program.modifiedRules, ["Item_seen"]);
      assert.equal(
        program.loopBodies[0]?.terminationMetric?.kind,
        "ssa-iterating-source-metric",
      );
    },
  );

  it(
    "foreach Shape B call site emits IR1SsaProgram with populated loopHeaderJoins and loopBodies",
    () => {
      const account = ir1Var("account");
      const item = ir1Var("item");
      const result = lowerL1BodyToSsaProps(
        ir1Foreach("item", ir1Var("items"), null, [
          {
            target: account,
            prop: "Account_total",
            combiner: "add",
            outerOp: "add",
            rhs: ir1Member(item, "Item_value"),
            guard: null,
          },
        ]),
        [],
        { applyConst: (expr) => expr },
      );

      assert.deepEqual(result.diagnostics, []);
      assert.equal(result.programs.length, 1);
      const program = result.programs[0]!;
      assert.equal(program.loopSummaries.length, 0);
      assert.equal(program.loopHeaderJoins.length, 1);
      assert.equal(program.loopBodies.length, 1);
      const body = program.loopBodies[0]!;
      const header = body.headerJoins[0]!;
      const write = body.writes[0]!;
      assert.equal(header.loopBackVersion, write.version);
      assert.equal(body.terminationMetric?.kind, "ssa-iterating-source-metric");
      assert.equal(write.value.kind, "property");
      if (write.value.kind === "property") {
        assert.deepEqual(
          write.value.value,
          ir1Binop(
            "add",
            ir1Member(account, "Account_total"),
            ir1Member(item, "Item_value"),
          ),
        );
      }
    },
  );

  it("μ-search summary symbols are deleted from ir1-ssa-loops.ts", () => {
    const source = readFileSync(
      resolve(import.meta.dirname, "../src/ir1-ssa-loops.ts"),
      "utf8",
    );

    assert.doesNotMatch(source, /lowerMuSearchSummary/u);
    assert.doesNotMatch(source, /MuSearchSummaryInput/u);
  });
});
