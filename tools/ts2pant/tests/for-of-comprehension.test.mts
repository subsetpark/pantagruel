// @archlint.module test
// @archlint.domain ts2pant.for-of-comprehension

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { createL1AssumptionEnv } from "../src/ir1-build.js";
import { formatIR1Expr } from "../src/ir1-printer.js";
import { makeUniqueSupply } from "../src/supply.js";
import { recognizeForOfPush } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";
import {
  buildDocument,
  emitAndCheck,
  runCheck,
  solverAvailable,
} from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/constructs/expressions-for-of-comprehension.ts",
);

describe("for-of comprehension recognizer", () => {
  it("matches map/filter build-list and rejects out-of-scope shapes", () => {
    const positiveCases: Array<{
      name: string;
      body: string;
      expected: {
        binder: string;
        src: string;
        proj: string;
        guards: string[];
        accName: string;
      };
    }> = [
      {
        name: "direct push",
        body: "for (const user of users) out.push(user.label);",
        expected: {
          binder: "user",
          src: "users",
          proj: "user--label user",
          guards: [],
          accName: "out",
        },
      },
      {
        name: "block push",
        body: "{ for (const user of users) { out.push(user.label); } }",
        expected: {
          binder: "user",
          src: "users",
          proj: "user--label user",
          guards: [],
          accName: "out",
        },
      },
      {
        name: "if push",
        body: "for (const user of users) { if (user.active) { out.push(user.label); } }",
        expected: {
          binder: "user",
          src: "users",
          proj: "user--label user",
          guards: ["user--active user"],
          accName: "out",
        },
      },
      {
        name: "if continue then push",
        body: "for (const user of users) { if (!user.active) continue; out.push(user.label); }",
        expected: {
          binder: "user",
          src: "users",
          proj: "user--label user",
          guards: ["user--active user"],
          accName: "out",
        },
      },
      {
        name: "Set source",
        body: "for (const label of labelSet) out.push(label);",
        expected: {
          binder: "label",
          src: "labelSet",
          proj: "label",
          guards: [],
          accName: "out",
        },
      },
    ];

    for (const testCase of positiveCases) {
      const { stmt, ctx } = setup(testCase.body);
      const result = recognizeForOfPush(stmt, new Set(["out"]), ctx);
      assertRecognized(testCase.name, result, testCase.expected);
    }

    const rejectionCases = [
      {
        name: "non-candidate accumulator",
        body: "for (const user of users) other.push(user.label);",
      },
      {
        name: "multiple statements",
        body: "for (const user of users) { out.push(user.label); out.push(user.label); }",
      },
      {
        name: "nested loop",
        body: "for (const user of users) { for (const label of labels) out.push(label); }",
      },
      {
        name: "break",
        body: "for (const user of users) { if (!user.active) break; out.push(user.label); }",
      },
      {
        name: "early return",
        body: "for (const user of users) { if (!user.active) return []; out.push(user.label); }",
      },
      {
        name: "scalar fold",
        body: "for (const user of users) total += user.label.length;",
      },
      {
        name: "Set.add",
        body: "for (const user of users) out.add(user.label);",
      },
      {
        name: "destructuring binder",
        body: "for (const [key, value] of entries) out.push(value);",
      },
      {
        name: "impure projection",
        body: "for (const user of users) out.push(tick(user.label));",
      },
      {
        name: "impure guard",
        body: "for (const user of users) { if (tick(user.active)) out.push(user.label); }",
      },
      {
        name: "generic iterable",
        body: "for (const user of genericUsers) out.push(user.label);",
      },
    ];

    for (const testCase of rejectionCases) {
      const { stmt, ctx } = setup(testCase.body);
      const result = recognizeForOfPush(stmt, new Set(["out"]), ctx);
      assert.equal(result, null, testCase.name);
    }
  });
});

function sourceFor(body: string): string {
  return `
    interface User {
      readonly label: string;
      readonly active: boolean;
    }
    declare function tick<T>(value: T): T;
    function f(
      users: readonly User[],
      labels: readonly string[],
      labelSet: ReadonlySet<string>,
      entries: ReadonlyMap<string, string>,
      genericUsers: Iterable<any>,
    ): string[] {
      const out: string[] = [];
      let total = 0;
      ${body}
      return out;
    }
  `;
}

function setup(body: string) {
  const sourceFile = createSourceFileFromSource(sourceFor(body));
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (fn === undefined) {
    throw new Error("expected function declaration");
  }
  const paramNames = new Map<string, string>();
  for (const param of fn.parameters) {
    if (ts.isIdentifier(param.name)) {
      paramNames.set(param.name.text, param.name.text);
    }
  }
  let stmt: ts.ForOfStatement | undefined;
  function visit(node: ts.Node): void {
    if (stmt === undefined && ts.isForOfStatement(node)) {
      stmt = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  visit(sourceFile.compilerNode);
  if (stmt === undefined) {
    throw new Error(`expected for-of statement in test body: ${body}`);
  }
  return {
    stmt,
    ctx: {
      checker,
      strategy: IntStrategy,
      paramNames,
      state: undefined,
      supply: makeUniqueSupply(),
      env: createL1AssumptionEnv(),
    },
  };
}

function assertRecognized(
  label: string,
  result: ReturnType<typeof recognizeForOfPush>,
  expected: {
    binder: string;
    src: string;
    proj: string;
    guards: string[];
    accName: string;
  },
): void {
  assert.notEqual(result, null, label);
  assert.equal(result.binder, expected.binder, label);
  assert.equal(formatIR1Expr(result.src), expected.src, label);
  assert.equal(formatIR1Expr(result.proj), expected.proj, label);
  assert.deepEqual(result.guards.map(formatIR1Expr), expected.guards, label);
  assert.equal(result.accName, expected.accName, label);
}

describe("for-of comprehension integration", () => {
  const hasSolver = solverAvailable();

  it("translates a build-list to an each comprehension", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "mapLabels"),
    );
    assert.match(
      output,
      /map-labels xs = \(each x in xs \| item--label x\)\./u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("filtered build-list emits a guarded each", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "filterIfActive"),
    );
    assert.match(
      output,
      /filter-if-active xs = \(each x in xs, item--active x \| x\)\./u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("build-list projection can reference a preceding const binding", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "mapWithPreludeConst"),
    );
    assert.match(output, /offset\s+=> Int\./u);
    assert.match(output, /offset = 1\./u);
    assert.match(
      output,
      /map-with-prelude-const xs = \(each x in xs \| item--value x \+ offset\)\./u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("@pant on a build-list entails", {
    skip: hasSolver ? false : "z3 unavailable",
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "filterIfActive"),
    );
    const result = runCheck(output);
    assert.match(result.output, /OK: Entailed:/u);
  });

  it("continue-filter build-list emits the same guarded each", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "filterContinueActive"),
    );
    assert.match(
      output,
      /filter-continue-active xs = \(each x in xs, item--active x \| x\)\./u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("Set source build-list emits an each comprehension", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "collectSet"),
    );
    assert.match(output, /collect-set xs = \(each x in xs \| x\)\./u);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("out-of-scope scalar fold still refuses with a precise reason", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "sumLengths"),
    );
    assert.match(
      output,
      /UNSUPPORTED: sum-lengths .* for-of loop is not a recognized build-list comprehension/u,
    );
  });

  it("Map-entry destructuring still refuses with a precise reason", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "mapEntryCopy"),
    );
    assert.match(
      output,
      /UNSUPPORTED: map-entry-copy .* for-of loop is not a recognized build-list comprehension/u,
    );
  });
});
