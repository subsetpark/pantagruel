// @archlint.module test
// @archlint.domain ts2pant.narrowing-integration

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import {
  type AssumptionEnv,
  envDepth,
  type Fact,
  queryFact,
} from "../src/assumption-env.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  buildL1Conditional,
  createL1AssumptionEnv,
  type L1BuildContext,
} from "../src/ir1-build.js";
import { buildL1IfMutation } from "../src/ir1-build-body.js";
import { loadAst } from "../src/pant-wasm.js";
import {
  makeSymbolicState,
  translateBody,
  type UniqueSupply,
} from "../src/translate-body.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function setupFunction(source: string): {
  fn: ts.FunctionDeclaration;
  checker: ts.TypeChecker;
  ctx: L1BuildContext;
  captures: Array<{ location: string; snapshot: AssumptionEnv }>;
} {
  const sourceFile = createSourceFileFromSource(source, "narrowing.ts");
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(
    (stmt): stmt is ts.FunctionDeclaration =>
      ts.isFunctionDeclaration(stmt) && stmt.body !== undefined,
  );
  if (!fn?.body) {
    throw new Error("setup: expected a function body");
  }
  const captures: Array<{ location: string; snapshot: AssumptionEnv }> = [];
  const env = createL1AssumptionEnv();
  const supply: UniqueSupply = { n: 0, synthCell: newSynthCell() };
  return {
    fn,
    checker,
    captures,
    ctx: {
      checker,
      strategy: IntStrategy,
      paramNames: new Map([["s", "s"]]),
      state: undefined,
      supply,
      env,
      recognitionHook: (hookEnv, location) => {
        captures.push({ location, snapshot: cloneEnv(hookEnv) });
      },
    },
  };
}

function cloneEnv(env: AssumptionEnv): AssumptionEnv {
  return { frames: env.frames.map((frame) => new Map(frame)) };
}

function envAt(
  captures: Array<{ location: string; snapshot: AssumptionEnv }>,
  location: string,
): AssumptionEnv {
  const capture = captures.find((c) => c.location === location);
  assert.ok(capture, `missing capture for ${location}`);
  return capture.snapshot;
}

function assertDiscriminant(
  env: AssumptionEnv,
  property: string,
  literal: string,
  negated = false,
): void {
  const expected: Fact = {
    kind: "discriminant",
    receiver: "s",
    property,
    literal,
    negated,
  };
  assert.ok(
    queryFact(env, expected),
    `missing ${negated ? "negated " : ""}${property} === ${literal}`,
  );
}

function assertNonNull(
  env: AssumptionEnv,
  receiver: string,
  negated = false,
): void {
  const expected: Fact = {
    kind: "non-null",
    receiver,
    negated,
  };
  assert.ok(
    queryFact(env, expected),
    `missing ${negated ? "nullish" : "non-null"} fact for ${receiver}`,
  );
}

describe("narrowing integration", () => {
  it("if-then arm sees discriminant fact", () => {
    const { fn, ctx, captures } = setupFunction(`
      interface Shape { kind: "circle" | "square"; }
      function f(s: Shape): number {
        if (s.kind === "circle") return 1;
        else return 2;
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.IfStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assertDiscriminant(envAt(captures, "if.then"), "kind", "circle");
    assert.equal(envDepth(ctx.env), 1);
  });

  it("if-else arm sees negated fact", () => {
    const { fn, ctx, captures } = setupFunction(`
      interface Shape { kind: "circle" | "square"; }
      function f(s: Shape): number {
        if (s.kind === "circle") return 1;
        else return 2;
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.IfStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assertDiscriminant(envAt(captures, "if.else"), "kind", "circle", true);
    assert.equal(envDepth(ctx.env), 1);
  });

  it("if not-equal arm sees negated fact and else sees positive fact", () => {
    const { fn, ctx, captures } = setupFunction(`
      interface Shape { kind: "circle" | "square"; }
      function f(s: Shape): number {
        if (s.kind !== "circle") return 1;
        else return 2;
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.IfStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assertDiscriminant(envAt(captures, "if.then"), "kind", "circle", true);
    assertDiscriminant(envAt(captures, "if.else"), "kind", "circle");
    assert.equal(envDepth(ctx.env), 1);
  });

  it("if non-null arm sees non-null fact and else sees nullish fact", () => {
    const { fn, ctx, captures } = setupFunction(`
      function f(s: number | null): number {
        if (s !== null) return s;
        else return 0;
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.IfStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assertNonNull(envAt(captures, "if.then"), "s");
    assertNonNull(envAt(captures, "if.else"), "s", true);
    assert.equal(envDepth(ctx.env), 1);
  });

  it("switch case and default arms see case facts", () => {
    const { fn, ctx, captures } = setupFunction(`
      interface Shape { kind: "circle" | "square"; }
      function f(s: Shape): number {
        switch (s.kind) {
          case "circle": return 1;
          case "square": return 2;
          default: return 3;
        }
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.SwitchStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assertDiscriminant(envAt(captures, "switch.case"), "kind", "circle");
    const defaults = envAt(captures, "switch.default");
    assertDiscriminant(defaults, "kind", "circle", true);
    assertDiscriminant(defaults, "kind", "square", true);
    assert.equal(envDepth(ctx.env), 1);
  });

  it("early-return fall-through sees negated fact", () => {
    const sourceFile = createSourceFileFromSource(
      `
        interface Shape { kind: "circle" | "square"; }
        function f(s: Shape): number {
          if (s.kind === "circle") return 1;
          return 2;
        }
      `,
      "narrowing-early.ts",
    );
    const env = createL1AssumptionEnv();
    const captures: Array<{ location: string; snapshot: AssumptionEnv }> = [];
    const result = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
      assumptionEnv: env,
      recognitionHook: (hookEnv, location) => {
        captures.push({ location, snapshot: cloneEnv(hookEnv) });
      },
    });
    assert.equal(
      result.some((r) => r.kind === "unsupported"),
      false,
    );
    assertDiscriminant(
      envAt(captures, "early-return.fallthrough"),
      "kind",
      "circle",
      true,
    );
    assert.equal(envDepth(env), 1);
  });

  it("early-return fall-through sees non-null complement", () => {
    const sourceFile = createSourceFileFromSource(
      `
        function f(s: number | null): number {
          if (s == null) return 0;
          return s;
        }
      `,
      "narrowing-early-nullish.ts",
    );
    const env = createL1AssumptionEnv();
    const captures: Array<{ location: string; snapshot: AssumptionEnv }> = [];
    const result = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
      assumptionEnv: env,
      recognitionHook: (hookEnv, location) => {
        captures.push({ location, snapshot: cloneEnv(hookEnv) });
      },
    });
    assert.equal(
      result.some((r) => r.kind === "unsupported"),
      false,
    );
    assertNonNull(envAt(captures, "early-return.fallthrough"), "s");
    assert.equal(envDepth(env), 1);
  });

  it("variant field reads record discharged narrowing", () => {
    const { fn, ctx } = setupFunction(`
      type Shape =
        | { kind: "circle"; r: number; shared: string }
        | { kind: "square"; s: number; shared: string };
      function f(s: Shape): number {
        if (s.kind === "circle") return s.r;
        else return 0;
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.IfStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assert.equal(result.kind, "cond");
    const variantRead = result.arms[0][1];
    assert.equal(variantRead.kind, "member");
    assert.equal(variantRead.name, "shape--r");
    assert.equal(variantRead.narrowingDischarged, true);
  });

  it("variant field discharge normalizes transparent receiver wrappers", () => {
    const { fn, ctx } = setupFunction(`
      type Shape =
        | { kind: "circle"; r: number; shared: string }
        | { kind: "square"; s: number; shared: string };
      function f(s: Shape): number {
        if (s.kind === "circle") return (s as Shape).r;
        else return 0;
      }
    `);
    const result = buildL1Conditional(
      fn.body!.statements[0] as ts.IfStatement,
      ctx,
    );
    assert.equal("unsupported" in result, false);
    assert.equal(result.kind, "cond");
    const variantRead = result.arms[0][1];
    assert.equal(variantRead.kind, "member");
    assert.equal(variantRead.name, "shape--r");
    assert.equal(variantRead.narrowingDischarged, true);
  });

  it("nested mutating if pushes both frames", () => {
    const sourceFile = createSourceFileFromSource(
      `
        interface Shape { kind: "circle" | "square"; color: "red" | "blue"; value: number; }
        function f(s: Shape): void {
          if (s.kind === "circle") {
            if (s.color === "red") {
              s.value = 1;
            }
          }
        }
      `,
      "narrowing-mutating.ts",
    );
    const checker = getChecker(sourceFile);
    const fn = sourceFile.compilerNode.statements.find(
      (stmt): stmt is ts.FunctionDeclaration =>
        ts.isFunctionDeclaration(stmt) && stmt.body !== undefined,
    );
    assert.ok(fn?.body);
    const env = createL1AssumptionEnv();
    const captures: Array<{ location: string; snapshot: AssumptionEnv }> = [];
    const result = buildL1IfMutation(fn.body.statements[0] as ts.IfStatement, {
      checker,
      strategy: IntStrategy,
      paramNames: new Map([["s", "s"]]),
      state: makeSymbolicState(),
      supply: { n: 0, synthCell: newSynthCell() },
      env,
      recognitionHook: (hookEnv, location) => {
        captures.push({ location, snapshot: cloneEnv(hookEnv) });
      },
    });
    assert.equal("unsupported" in result, false);
    const inner = captures.filter((c) => c.location === "if.then")[1];
    assert.ok(inner);
    assert.equal(envDepth(inner.snapshot), 3);
    assertDiscriminant(inner.snapshot, "kind", "circle");
    assertDiscriminant(inner.snapshot, "color", "red");
    assert.equal(envDepth(env), 1);
  });

  it("mutating if not-equal arm sees negated fact and else sees positive fact", () => {
    const sourceFile = createSourceFileFromSource(
      `
        interface Shape { kind: "circle" | "square"; value: number; }
        function f(s: Shape): void {
          if (s.kind !== "circle") {
            s.value = 1;
          } else {
            s.value = 2;
          }
        }
      `,
      "narrowing-mutating-not-equal.ts",
    );
    const checker = getChecker(sourceFile);
    const fn = sourceFile.compilerNode.statements.find(
      (stmt): stmt is ts.FunctionDeclaration =>
        ts.isFunctionDeclaration(stmt) && stmt.body !== undefined,
    );
    assert.ok(fn?.body);
    const env = createL1AssumptionEnv();
    const captures: Array<{ location: string; snapshot: AssumptionEnv }> = [];
    const result = buildL1IfMutation(fn.body.statements[0] as ts.IfStatement, {
      checker,
      strategy: IntStrategy,
      paramNames: new Map([["s", "s"]]),
      state: makeSymbolicState(),
      supply: { n: 0, synthCell: newSynthCell() },
      env,
      recognitionHook: (hookEnv, location) => {
        captures.push({ location, snapshot: cloneEnv(hookEnv) });
      },
    });
    assert.equal("unsupported" in result, false);
    assertDiscriminant(envAt(captures, "if.then"), "kind", "circle", true);
    assertDiscriminant(envAt(captures, "if.else"), "kind", "circle");
    assert.equal(envDepth(env), 1);
  });
});
