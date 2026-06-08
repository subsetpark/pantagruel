// @archlint.module test
// @archlint.domain ts2pant.purity

import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import assert from "node:assert/strict";
import * as fc from "fast-check";
import ts from "typescript";
import {
  createSourceFile,
  createSourceFileFromSource,
  getChecker,
} from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  expressionHasSideEffects,
  isEffectFree,
  isKnownPureCall,
  isPureUserCall,
  isPureUserFunction,
  isStaticallyBoolTyped,
  resolveEffectLibraryExport,
} from "../src/purity.js";
import { translateSignature } from "../src/translate-signature.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

const generatedPurityCases = [
  "x > 0",
  "x >= 0 && x < 10",
  "Math.abs(x) >= 0",
  "Math.max(x, 0) >= x",
  "x === x",
  "x !== 1",
  "helper(x)",
  "x > 0 ? helper(x) : x === 0",
  "String(label).trim().length >= 0",
].map((exprText) => {
  const sf = createSourceFileFromSource(`
      function helper(x: number): boolean { return x > 0; }
      function f(x: number, label: string): boolean { return ${exprText}; }
    `);
  const checker = getChecker(sf);
  const expr = findReturnExpression(sf.compilerNode);
  assert.ok(expr);
  const helper = findFunctionDeclaration(sf.compilerNode, "helper");
  return { checker, expr, exprText, helper };
});

const generatedPureCallCase = (() => {
  const sf = createSourceFileFromSource(`
    function helper(x: number): boolean { return x > 0; }
    function g(x: number): boolean { return helper(x); }
  `);
  return {
    call: findCallInNamedFunction(sf.compilerNode, "g"),
    checker: getChecker(sf),
  };
})();

it("generated pure expressions preserve purity classifications", () => {
  fc.assert(
    fc.property(fc.constantFrom(...generatedPurityCases), (testCase) => {
      const { checker, expr, helper } = testCase;
      assert.equal(isEffectFree(expr, checker), true);
      assert.equal(expressionHasSideEffects(expr, checker), false);
      assert.equal(isStaticallyBoolTyped(expr, checker), true);

      assert.equal(isPureUserFunction(helper, checker), true);
      assert.equal(
        isPureUserCall(
          generatedPureCallCase.call,
          generatedPureCallCase.checker,
        ),
        true,
      );
      assert.equal(
        resolveEffectLibraryExport(
          generatedPureCallCase.call.expression,
          generatedPureCallCase.checker,
        ),
        null,
      );
    }),
    { numRuns: generatedPurityCases.length },
  );
});

/**
 * Find the first CallExpression in a source file's function body.
 * Searches depth-first through all function declarations.
 */
function findCallExpression(
  sourceFile: ts.SourceFile,
): ts.CallExpression | undefined {
  let result: ts.CallExpression | undefined;
  function visit(node: ts.Node) {
    if (result) return;
    if (ts.isCallExpression(node)) {
      result = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sourceFile, visit);
  return result;
}

/**
 * Helper: create a source with a function containing the expression,
 * extract the call expression and checker, and return isKnownPureCall result.
 */
function checkPurity(source: string): boolean {
  const sf = createSourceFileFromSource(source);
  const checker = getChecker(sf);
  const callExpr = findCallExpression(sf.compilerNode);
  if (!callExpr) {
    throw new Error("No CallExpression found in source");
  }
  return isKnownPureCall(callExpr, checker);
}

function findReturnExpression(
  sourceFile: ts.SourceFile,
): ts.Expression | undefined {
  let result: ts.Expression | undefined;
  function visit(node: ts.Node) {
    if (result) return;
    if (ts.isReturnStatement(node) && node.expression) {
      result = node.expression;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sourceFile, visit);
  return result;
}

function findFunctionDeclaration(
  sourceFile: ts.SourceFile,
  name: string,
): ts.FunctionDeclaration {
  let result: ts.FunctionDeclaration | undefined;
  function visit(node: ts.Node) {
    if (result) return;
    if (ts.isFunctionDeclaration(node) && node.name?.text === name) {
      result = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sourceFile, visit);
  if (!result) {
    throw new Error(`No function declaration found for ${name}`);
  }
  return result;
}

function findCallInNamedFunction(
  sourceFile: ts.SourceFile,
  name: string,
): ts.CallExpression {
  const fn = findFunctionDeclaration(sourceFile, name);
  let result: ts.CallExpression | undefined;
  function visit(node: ts.Node) {
    if (result) return;
    if (ts.isCallExpression(node)) {
      result = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  if (fn.body) {
    ts.forEachChild(fn.body, visit);
  }
  if (!result) {
    throw new Error(`No CallExpression found in function ${name}`);
  }
  return result;
}

function checkEffectFree(source: string): boolean {
  const sf = createSourceFileFromSource(source);
  const checker = getChecker(sf);
  const expr = findReturnExpression(sf.compilerNode);
  if (!expr) {
    throw new Error("No return expression found in source");
  }
  return isEffectFree(expr, checker);
}

describe("isKnownPureCall", () => {
  // --- Tier 1a: Pure namespaces ---

  it("should return true for Math.max", () => {
    assert.equal(
      checkPurity(`
        function f(a: number, b: number) { return Math.max(a, b); }
      `),
      true,
    );
  });

  it("should return true for Math.abs", () => {
    assert.equal(
      checkPurity(`
        function f(x: number) { return Math.abs(x); }
      `),
      true,
    );
  });

  it("should return false for Math.random", () => {
    assert.equal(
      checkPurity(`
        function f() { return Math.random(); }
      `),
      false,
    );
  });

  it("should return true for nested pure calls", () => {
    assert.equal(
      checkPurity(`
        function f(x: number, y: number) { return Math.max(Math.abs(x), y); }
      `),
      true,
    );
  });

  // --- Tier 1a: String methods ---

  it("should return true for String.prototype.indexOf", () => {
    assert.equal(
      checkPurity(`
        function f(s: string) { return s.indexOf("x"); }
      `),
      true,
    );
  });

  // --- Tier 1a: Array methods (no callback) ---

  it("should return true for Array.prototype.includes", () => {
    assert.equal(
      checkPurity(`
        function f(arr: number[]) { return arr.includes(1); }
      `),
      true,
    );
  });

  // --- Tier 1a: Higher-order array methods with arrow callbacks ---

  it("should return true for array.filter with pure callback", () => {
    assert.equal(
      checkPurity(`
        function f(arr: number[]) { return arr.filter(x => x > 0); }
      `),
      true,
    );
  });

  it("should return false for array.filter with impure callback", () => {
    assert.equal(
      checkPurity(`
        declare function sideEffect(x: number): boolean;
        function f(arr: number[]) { return arr.filter(x => sideEffect(x)); }
      `),
      false,
    );
  });

  it("should return true for array.map with pure arrow callback", () => {
    assert.equal(
      checkPurity(`
        function f(arr: number[]) { return arr.map(x => x * 2); }
      `),
      true,
    );
  });

  it("should return false for array.map with impure arrow callback", () => {
    assert.equal(
      checkPurity(`
        declare function transform(x: number): number;
        function f(arr: number[]) { return arr.map(x => transform(x)); }
      `),
      false,
    );
  });

  it("should return true for array.filter with expression-bodied arrow", () => {
    assert.equal(
      checkPurity(`
        function f(arr: string[]) { return arr.filter(s => s.length > 0); }
      `),
      true,
    );
  });

  it("should return false for array.filter with block-bodied arrow containing side effect", () => {
    assert.equal(
      checkPurity(`
        declare function log(x: number): void;
        function f(arr: number[]) {
          return arr.filter(x => { log(x); return x > 0; });
        }
      `),
      false,
    );
  });

  // --- Tier 1b: Effect-TS ---
  //
  // Two detection paths:
  // (1) Symbol resolution: when `effect` package is installed, the callee is
  //     traced to its declaration file via getAliasedSymbol. If it originates
  //     from node_modules/effect/, it's classified by the library's purity
  //     guarantee (all non-runner/non-allocator exports are pure).
  // (2) Bare-name fallback: for test environments without `effect` installed,
  //     pipe/flow/identity are matched by identifier name + argument purity.
  //
  // Tests below use `declare function` which exercises the fallback path.
  // The symbol resolution path is exercised in integration tests with the
  // real `effect` package.

  it("should return false for user-defined Effect-returning function", () => {
    // A user function returning Effect is NOT guaranteed pure — its body may
    // have side effects. Only Effect library exports have the purity guarantee.
    // Symbol resolution correctly distinguishes: user file != effect package.
    assert.equal(
      checkPurity(`
        interface Effect<A, E, R> {
          readonly EffectTypeId: unique symbol;
          readonly _A: A;
          readonly _E: E;
          readonly _R: R;
        }
        declare function succeed<A>(value: A): Effect<A, never, never>;
        function f(x: number) { return succeed(x); }
      `),
      false,
    );
  });

  it("should return false for Effect.runSync", () => {
    // Runners are in the EFFECT_IMPURE_EXPORTS set — impure regardless of
    // whether detected via symbol resolution or name matching.
    assert.equal(
      checkPurity(`
        interface Effect<A, E, R> {
          readonly EffectTypeId: unique symbol;
          readonly _A: A;
          readonly _E: E;
          readonly _R: R;
        }
        declare const Effect: {
          runSync<A>(effect: Effect<A, never, never>): A;
        };
        declare const eff: Effect<number, never, never>;
        function f() { return Effect.runSync(eff); }
      `),
      false,
    );
  });

  it("should return false for Effect.runPromise", () => {
    assert.equal(
      checkPurity(`
        declare const Effect: {
          runPromise<A>(effect: any): Promise<A>;
        };
        function f(eff: any) { return Effect.runPromise(eff); }
      `),
      false,
    );
  });

  it("should return true for pipe from effect/Function (fallback path)", () => {
    // bare-name fallback: pipe matched by identifier + args checked for purity
    assert.equal(
      checkPurity(`
        declare function pipe<A, B>(a: A, f: (a: A) => B): B;
        function f(x: number) { return pipe(x, n => n + 1); }
      `),
      true,
    );
  });

  it("should return false for pipe with impure argument", () => {
    assert.equal(
      checkPurity(`
        declare function pipe<A, B>(a: A, f: (a: A) => B): B;
        declare function sideEffect(): number;
        function f() { return pipe(sideEffect(), n => n + 1); }
      `),
      false,
    );
  });

  it("should return false for identity with impure argument", () => {
    assert.equal(
      checkPurity(`
        declare function identity<A>(a: A): A;
        declare function sideEffect(): number;
        function f() { return identity(sideEffect()); }
      `),
      false,
    );
  });

  it("should return false for makeSemaphore (mutable allocator)", () => {
    assert.equal(
      checkPurity(`
        declare const Effect: {
          makeSemaphore(permits: number): any;
        };
        function f() { return Effect.makeSemaphore(1); }
      `),
      false,
    );
  });

  // --- Tier 1c: Conservative default ---

  it("should return false for unknown function", () => {
    assert.equal(
      checkPurity(`
        declare function unknownFn(x: number): number;
        function f(x: number) { return unknownFn(x); }
      `),
      false,
    );
  });
});

describe("isEffectFree", () => {
  it("treats known-pure builtin calls as effect-free", () => {
    assert.equal(
      checkEffectFree(`
        function f(a: number, b: number) { return Math.max(a, b); }
      `),
      true,
    );
  });

  it("treats user calls as effectful", () => {
    assert.equal(
      checkEffectFree(`
        declare function userFn(x: number): number;
        function f(x: number) { return userFn(x); }
      `),
      false,
    );
  });

  it("treats mutation as effectful", () => {
    assert.equal(
      checkEffectFree(`
        function f(x: number) { return ++x; }
      `),
      false,
    );
  });

  it("treats await as effectful", () => {
    assert.equal(
      checkEffectFree(`
        async function f(p: Promise<number>) { return await p; }
      `),
      false,
    );
  });

  it("treats new expressions as effectful", () => {
    assert.equal(
      checkEffectFree(`
        class Box {}
        function f() { return new Box(); }
      `),
      false,
    );
  });

  it("guard extraction over a builtin-call condition uses the checker-aware oracle", () => {
    const sf = createSourceFileFromSource(`
        export function deposit(amount: number, limit: number): number {
          if (!(Math.max(amount, limit) > 0)) {
            throw new Error("amount");
          }
          return amount;
        }
      `);

    const synthCell = newSynthCell();
    const result = translateSignature(sf, "deposit", IntStrategy, synthCell);

    assert.ok(result.declaration.kind === "rule");
    assert.ok(result.declaration.guard);
    assert.ok(synthCell.imports.has("JS_MATH"));
    assert.equal(
      getAst().strExpr(result.declaration.guard),
      "JS_MATH::max-of amount limit > 0",
    );
  });
});

describe("isPureUserFunction", () => {
  it("classifies a pure helper and its direct call as pure", () => {
    const sf = createSourceFileFromSource(`
      function isPositive(n: number): boolean {
        const limit = Math.max(0, n);
        return limit > 0;
      }
      function f(n: number): boolean {
        return isPositive(n);
      }
    `);
    const checker = getChecker(sf);

    assert.equal(
      isPureUserFunction(
        findFunctionDeclaration(sf.compilerNode, "isPositive"),
        checker,
      ),
      true,
    );
    assert.equal(
      isPureUserCall(findCallInNamedFunction(sf.compilerNode, "f"), checker),
      true,
    );
  });

  it("classifies mutating, throwing, and async helpers as effectful", () => {
    const sf = createSourceFileFromSource(`
      let counter = 0;
      function mutates(n: number): number {
        counter = n;
        return n;
      }
      function throws(n: number): number {
        if (n < 0) throw new Error("bad");
        return n;
      }
      async function awaits(p: Promise<number>): Promise<number> {
        return await p;
      }
    `);
    const checker = getChecker(sf);

    for (const name of ["mutates", "throws", "awaits"]) {
      assert.equal(
        isPureUserFunction(
          findFunctionDeclaration(sf.compilerNode, name),
          checker,
        ),
        false,
      );
    }
  });

  it("bails to effectful on recursive helper calls", () => {
    const sf = createSourceFileFromSource(`
      function isEven(n: number): boolean {
        if (n === 0) return true;
        return isOdd(n - 1);
      }
      function isOdd(n: number): boolean {
        if (n === 0) return false;
        return isEven(n - 1);
      }
      function f(n: number): boolean {
        return isEven(n);
      }
    `);
    const checker = getChecker(sf);

    assert.equal(
      isPureUserFunction(
        findFunctionDeclaration(sf.compilerNode, "isEven"),
        checker,
      ),
      false,
    );
    assert.equal(
      isPureUserCall(findCallInNamedFunction(sf.compilerNode, "f"), checker),
      false,
    );
  });

  it("bails to effectful for node_modules call targets", () => {
    const sf = createSourceFileFromSource(
      `
        function helper(n: number): boolean {
          return n > 0;
        }
        function f(n: number): boolean {
          return helper(n);
        }
      `,
      "/project/node_modules/pkg/index.ts",
    );
    const checker = getChecker(sf);

    assert.equal(
      isPureUserCall(findCallInNamedFunction(sf.compilerNode, "f"), checker),
      false,
    );
  });
});

// ---------------------------------------------------------------------------
// Effect-TS symbol resolution tests (Tier 1b with real `effect` package)
//
// These tests use a real fixture file that imports from `effect`, so the
// TypeChecker can resolve callee symbols back to node_modules/effect/.
// This exercises the resolveEffectLibraryExport path (not the bare-name
// fallback used in the declare-function tests above).
// ---------------------------------------------------------------------------

/**
 * Find the first CallExpression inside a named exported function.
 */
function findCallInFunction(
  sourceFile: ts.SourceFile,
  funcName: string,
): ts.CallExpression | undefined {
  let result: ts.CallExpression | undefined;
  function visit(node: ts.Node) {
    if (result) return;
    if (
      ts.isFunctionDeclaration(node) &&
      node.name?.text === funcName &&
      node.body
    ) {
      // Find first call in this function's body
      function findCall(n: ts.Node) {
        if (result) return;
        if (ts.isCallExpression(n)) {
          result = n;
          return;
        }
        ts.forEachChild(n, findCall);
      }
      ts.forEachChild(node.body, findCall);
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sourceFile, visit);
  return result;
}

describe("isKnownPureCall (Effect-TS symbol resolution)", () => {
  const fixturePath = resolve(
    import.meta.dirname,
    "fixtures/effect-ts-purity.ts",
  );
  const sourceFile = createSourceFile(fixturePath);
  const checker = getChecker(sourceFile);

  function checkFixturePurity(funcName: string): boolean {
    const callExpr = findCallInFunction(sourceFile.compilerNode, funcName);
    if (!callExpr) {
      throw new Error(`No CallExpression found in function ${funcName}`);
    }
    return isKnownPureCall(callExpr, checker);
  }

  // --- Pure constructors (symbol resolves to effect package) ---

  it("Effect.succeed is pure (library constructor)", () => {
    assert.equal(checkFixturePurity("effectSucceed"), true);
  });

  it("Effect.map is pure (library combinator)", () => {
    assert.equal(checkFixturePurity("effectMap"), true);
  });

  it("Effect.flatMap is pure (library combinator)", () => {
    assert.equal(checkFixturePurity("effectFlatMap"), true);
  });

  it("pipe from effect is pure (library combinator)", () => {
    assert.equal(checkFixturePurity("effectPipe"), true);
  });

  it("Effect.sync is pure (library constructor)", () => {
    assert.equal(checkFixturePurity("effectSync"), true);
  });

  it("Effect.fail is pure (library constructor)", () => {
    assert.equal(checkFixturePurity("effectFail"), true);
  });

  // --- Impure runners ---

  it("Effect.runSync is impure (runner)", () => {
    assert.equal(checkFixturePurity("effectRunSync"), false);
  });

  it("Effect.runPromise is impure (runner)", () => {
    assert.equal(checkFixturePurity("effectRunPromise"), false);
  });

  // --- User function returning Effect ---

  it("pure user function returning Effect is pure by body analysis", () => {
    assert.equal(checkFixturePurity("userEffectReturning"), true);
  });
});
