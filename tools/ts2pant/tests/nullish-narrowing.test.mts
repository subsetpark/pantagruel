import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import ts from "typescript";
import {
  createAssumptionEnv,
  enterFrame,
  nonNullFactInScope,
  pushFact,
  type Fact,
} from "../src/assumption-env.js";
import { renderNullishObligation } from "../src/definedness-obligation.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  negateFact,
  recognizeNullishNarrowing,
} from "../src/narrowing-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  buildDocument,
  emitAndCheck,
  getPantBin,
  PROJECT_ROOT,
} from "./helpers.mts";
import { runCheck } from "../src/emit.js";

const FIXTURES = resolve(import.meta.dirname, "fixtures/constructs");

function solverAvailable(): boolean {
  try {
    execFileSync("z3", ["-version"], { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
}

before(async () => {
  await loadAst();
});

function parseExpr(source: string): {
  expr: ts.Expression;
  checker: ts.TypeChecker;
} {
  const sf = createSourceFileFromSource(`
    declare const x: number | null | undefined;
    declare const y: number | null | undefined;
    const _ = (${source});
  `);
  const checker = getChecker(sf);
  const stmt = sf.compilerNode.statements.at(-1);
  if (!stmt || !ts.isVariableStatement(stmt)) {
    throw new Error("test helper: expected trailing variable statement");
  }
  const init = stmt.declarationList.declarations[0]?.initializer;
  if (!init) {
    throw new Error("test helper: expected initializer");
  }
  let expr: ts.Expression = init;
  while (ts.isParenthesizedExpression(expr)) {
    expr = expr.expression;
  }
  return { expr, checker };
}

function recognize(source: string): Fact | null {
  const { expr, checker } = parseExpr(source);
  return recognizeNullishNarrowing(expr, checker);
}

describe("nullish narrowing helpers", () => {
  it("recognizeNullishNarrowing maps nullish shapes to a non-null fact", () => {
    for (const source of [
      "x != null",
      "x !== null",
      "x !== undefined",
      "typeof x !== 'undefined'",
      "x !== null && x !== undefined",
    ]) {
      assert.deepEqual(recognize(source), {
        kind: "non-null",
        receiver: "x",
        negated: false,
      });
    }
  });

  it("recognizeNullishNarrowing maps null branches to a negated non-null fact", () => {
    for (const source of [
      "x == null",
      "x === null",
      "x === undefined",
      "typeof x === 'undefined'",
      "x === null || x === undefined",
    ]) {
      assert.deepEqual(recognize(source), {
        kind: "non-null",
        receiver: "x",
        negated: true,
      });
    }
  });

  it("negateFact flips non-null and is-null", () => {
    const fact: Fact = {
      kind: "non-null",
      receiver: "x",
      negated: false,
    };

    assert.deepEqual(negateFact(fact), {
      kind: "non-null",
      receiver: "x",
      negated: true,
    });
    assert.deepEqual(negateFact(negateFact(fact)), fact);
  });

  it("nonNullFactInScope finds matching non-null facts", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, { kind: "non-null", receiver: "x", negated: false });
    pushFact(env, { kind: "non-null", receiver: "y", negated: false });
    pushFact(env, { kind: "non-null", receiver: "x", negated: true });

    assert.deepEqual(nonNullFactInScope(env, "x"), [
      { receiver: "x", negated: false },
      { receiver: "x", negated: true },
    ]);
  });

  it("renderNullishObligation renders a discharged cardinality goal", () => {
    const ast = getAst();
    const obligation = renderNullishObligation({
      receiver: ast.var("x"),
      inScope: [{ receiver: "x", negated: false }],
    });

    assert.equal(obligation.text, "true");
  });

  it(
    "x !== null guard discharges optional read (@pant entails)",
    { skip: !solverAvailable() ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocument(
        resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
        "strictNullRead",
      );
      const output = await emitAndCheck(doc);
      assert.match(
        output,
        /strict-null-read x d = \(cond ~\(#x = 0\) => x 1, true => d\)\./u,
      );
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      assert.equal(result.passed, true, result.output);
      assert.ok(
        result.checks.some((c) => c.message.startsWith("OK: Entailed:")),
        result.output,
      );
    },
  );

  it(
    "optional read in the null branch is not entailed (negative control)",
    { skip: !solverAvailable() ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocument(
        resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
        "nullBranchControl",
      );
      const output = await emitAndCheck(doc);
      assert.match(
        output,
        /null-branch-control x d = \(cond #x = 0 => x, true => d\)\./u,
      );
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      assert.ok(
        result.checks.some(
          (c) =>
            c.message.startsWith("FAIL: Not entailed:") ||
            c.message.startsWith("UNKNOWN: Entailed:"),
        ) || /UNKNOWN: Entailed:|FAIL: Not entailed:/u.test(result.output),
        result.output,
      );
    },
  );
});
