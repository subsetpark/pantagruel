/**
 * Unit tests for the state-aware Map/Set read forms (`map-read`,
 * `set-read`) that the L1 build pass emits inside mutating bodies
 * when `ctx.state !== undefined`. The forms exist so the body lower
 * path can dispatch to `readMapThroughWrites` /
 * `readSetThroughWrites`, observing prior staged writes from the
 * same path. Pure-path callers (no state) keep emitting the bare
 * `App` / `Binop(in, ...)` shapes via the legacy fast-path. See
 * issue #168.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  type L1BuildContext,
  tryBuildL1PureSubExpression,
} from "../src/ir1-build.js";
import type { IR1Expr } from "../src/ir1.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  makeSymbolicState,
  translateBody,
  type UniqueSupply,
} from "../src/translate-body.js";
import { translateSignature } from "../src/translate-signature.js";
import {
  cellRegisterName,
  IntStrategy,
  newSynthCell,
  toPantTermName,
} from "../src/translate-types.js";
import type { PropResult } from "../src/types.js";

before(async () => {
  await loadAst();
});

interface ExprSetup {
  expr: ts.Expression;
  ctx: L1BuildContext;
}

/** Parse a single `return EXPR;` body and hand back the expression + a build ctx. */
function setupReturn(source: string, withState: boolean): ExprSetup {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (!fn || !fn.body) {
    throw new Error("setup: expected a function declaration with a body");
  }
  const synthCell = newSynthCell();
  const paramNames = new Map<string, string>();
  for (const p of fn.parameters) {
    if (ts.isIdentifier(p.name)) {
      const pantName = cellRegisterName(synthCell, toPantTermName(p.name.text));
      paramNames.set(p.name.text, pantName);
    }
  }
  const supply: UniqueSupply = { n: 0, synthCell };
  const ctx: L1BuildContext = {
    checker,
    strategy: IntStrategy,
    paramNames,
    state: withState ? makeSymbolicState() : undefined,
    supply,
  };
  const stmt = fn.body.statements[0];
  if (!stmt || !ts.isReturnStatement(stmt) || !stmt.expression) {
    throw new Error("setup: expected a return statement with an expression");
  }
  return { expr: stmt.expression, ctx };
}

function expectIR(r: IR1Expr | { unsupported: string } | null): IR1Expr {
  if (r === null) {
    throw new Error("expected an IR1Expr, got null");
  }
  if ("unsupported" in r) {
    throw new Error(`expected an IR1Expr, got unsupported: ${r.unsupported}`);
  }
  return r;
}

describe("ir1-build state-aware Map/Set reads", () => {
  describe("Set.has", () => {
    it("Stage A receiver builds set-read when ctx.state is defined", () => {
      const { expr, ctx } = setupReturn(
        `interface Tagged { readonly tags: Set<string>; }
         function f(c: Tagged, x: string): boolean {
           return c.tags.has(x);
         }`,
        true,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(
        built.kind,
        "set-read",
        `expected set-read, got ${built.kind}`,
      );
      if (built.kind !== "set-read") return;
      assert.equal(built.ruleName, "tagged--tags");
      assert.equal(built.ownerType, "Tagged");
      assert.equal(built.elemType, "String");
    });

    it("Stage A receiver builds bare Binop(in, ...) when ctx.state is undefined", () => {
      const { expr, ctx } = setupReturn(
        `interface Tagged { readonly tags: Set<string>; }
         function f(c: Tagged, x: string): boolean {
           return c.tags.has(x);
         }`,
        false,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(
        built.kind,
        "binop",
        `expected binop (bare path), got ${built.kind}`,
      );
      if (built.kind !== "binop") return;
      assert.equal(built.op, "in");
    });

    it("parameter-level Set receiver keeps the bare Binop(in, ...) form", () => {
      // No staged Set writes can exist on a parameter receiver
      // (parameter-level Set mutation is rejected at translate time),
      // so the bare form is sound and the build pass keeps it for
      // byte-equivalent output with the pure path.
      const { expr, ctx } = setupReturn(
        `function f(s: Set<string>, x: string): boolean {
           return s.has(x);
         }`,
        true,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(built.kind, "binop");
    });
  });

  describe("Map.has / Map.get", () => {
    it("Stage A `.has` builds map-read with op=has when ctx.state is defined", () => {
      const { expr, ctx } = setupReturn(
        `interface Cache { readonly entries: Map<string, number>; }
         function f(c: Cache, k: string): boolean {
           return c.entries.has(k);
         }`,
        true,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(built.kind, "map-read");
      if (built.kind !== "map-read") return;
      assert.equal(built.op, "has");
      assert.equal(built.ruleName, "cache--entries");
      assert.equal(built.keyPredName, "cache--entries-key");
      assert.equal(built.ownerType, "Cache");
      assert.equal(built.keyType, "String");
    });

    it("Stage A `.get` builds map-read with op=get when ctx.state is defined", () => {
      const { expr, ctx } = setupReturn(
        `interface Cache { readonly entries: Map<string, number>; }
         function f(c: Cache, k: string): number {
           return c.entries.get(k)!;
         }`,
        true,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(built.kind, "map-read");
      if (built.kind !== "map-read") return;
      assert.equal(built.op, "get");
    });

    it("Stage B `.get` builds map-read against the synthesized rule", () => {
      const { expr, ctx } = setupReturn(
        `function f(m: Map<string, number>, k: string): number {
           return m.get(k)!;
         }`,
        true,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(built.kind, "map-read");
      if (built.kind !== "map-read") return;
      assert.equal(built.op, "get");
      // Synth names: `domain` is CamelCase (`StringToIntMap`); `rule`
      // is the kebab-cased term form (`string-to-int-map`); `keyPred`
      // appends `-key`.
      assert.equal(built.ruleName, "string-to-int-map");
      assert.equal(built.keyPredName, "string-to-int-map-key");
      assert.equal(built.ownerType, "StringToIntMap");
    });

    it("Stage A Map receiver keeps bare App when ctx.state is undefined", () => {
      const { expr, ctx } = setupReturn(
        `interface Cache { readonly entries: Map<string, number>; }
         function f(c: Cache, k: string): boolean {
           return c.entries.has(k);
         }`,
        false,
      );
      const built = expectIR(tryBuildL1PureSubExpression(expr, ctx));
      assert.equal(built.kind, "app", `expected bare app, got ${built.kind}`);
    });
  });
});

/**
 * Translate `functionName` from `source` with synth + paramNameMap
 * threaded through signature translation — needed for Map/Set bodies
 * (which allocate domains and binders against the document-wide
 * `NameRegistry`) and for emit-time binder allocation.
 */
function translateBodyWithSynth(source: string, functionName: string): PropResult[] {
  const sf = createSourceFileFromSource(source);
  const synthCell = newSynthCell();
  const { paramNameMap } = translateSignature(
    sf,
    functionName,
    IntStrategy,
    synthCell,
  );
  return translateBody({
    sourceFile: sf,
    functionName,
    strategy: IntStrategy,
    synthCell,
    paramNameMap,
  });
}

describe("end-to-end: branched Map/Set read observes prior staged write", () => {
  it("Stage A Set: branched .add then branched .has observes the staged add", () => {
    const props = translateBodyWithSynth(
      `interface Tagged { tags: Set<string>; flag: boolean; }
       export function f(c: Tagged, x: string, gate: boolean): void {
         if (gate) {
           c.tags.add(x);
           if (c.tags.has(x)) {
             c.flag = true;
           }
         }
       }`,
      "f",
    );
    const unsupported = props.filter((p) => p.kind === "unsupported");
    assert.equal(
      unsupported.length,
      0,
      `unexpected unsupported props: ${JSON.stringify(unsupported)}`,
    );
    // Find the flag' equation. Its rhs (a cond merging the gate with
    // the identity fall-through) must reference `true` — the inner
    // guard `c.tags.has(x)` reduced to `true` because the staged `.add`
    // was visible. With the pre-fix bare read, the rhs would have
    // instead carried `cond x in (tagged--tags c) => true, ...`.
    const ast = getAst();
    const flagEq = props.find((p) => {
      if (p.kind !== "equation") return false;
      const lhsStr = ast.strExpr(
        (p as { lhs: import("../src/pant-ast.js").OpaqueExpr }).lhs,
      );
      return /flag'\s*c/u.test(lhsStr);
    });
    assert.ok(flagEq, "expected a `flag' c` equation");
    if (flagEq && flagEq.kind === "equation") {
      const rhsStr = ast.strExpr(
        (flagEq as { rhs: import("../src/pant-ast.js").OpaqueExpr }).rhs,
      );
      // The inner guard's `c.tags.has(x)` lowered through
      // `readSetThroughWrites` resolves to a cond whose first arm is
      // the equality `x = x => true` produced by the prior staged
      // `.add(x)`. With the pre-fix bare read, the guard would have
      // been the bare pre-state membership `x in tagged--tags c`
      // rather than the staged equality arm.
      assert.match(
        rhsStr,
        /cond\s+x\s*=\s*x\s*=>\s*true/u,
        `flag' rhs should include the staged Set-read equality arm: ${rhsStr}`,
      );
      assert.doesNotMatch(
        rhsStr,
        /cond\s+x\s+in\s+tagged--tags\s+c\s*=>\s*true/u,
        `flag' rhs should not be guarded by bare pre-state membership: ${rhsStr}`,
      );
    }
  });

  it("Stage B Map: branched .set followed by branched .has + .get observes the prior write", () => {
    const props = translateBodyWithSynth(
      `export function f(m: Map<string, number>, k: string, v: number, gate: boolean): void {
         if (gate) {
           m.set(k, v);
           if (m.has(k)) {
             m.set(k, m.get(k)! + 1);
           }
         }
       }`,
      "f",
    );
    const unsupported = props.filter((p) => p.kind === "unsupported");
    assert.equal(
      unsupported.length,
      0,
      `unexpected unsupported props: ${JSON.stringify(unsupported)}`,
    );
    const equations = props.filter((p) => p.kind === "equation");
    assert.ok(
      equations.length > 0,
      "expected emitted equations for the branched body",
    );
    // The merged value-rule equation must reference `v` — the inner
    // `m.get(k)!` saw the staged `.set(k, v)` through
    // `readMapThroughWrites` and inlined `v` rather than reading the
    // pre-state rule.
    const ast = getAst();
    const valueEq = props.find((p) => {
      if (p.kind !== "equation") return false;
      const lhsStr = ast.strExpr(
        (p as { lhs: import("../src/pant-ast.js").OpaqueExpr }).lhs,
      );
      // The Stage B value rule's primed form. Match either kebab or
      // CamelCase to avoid coupling to current name format.
      return /string-to-int-map'/u.test(lhsStr);
    });
    assert.ok(
      valueEq,
      "expected a primed Stage B Map value-rule equation",
    );
    if (valueEq && valueEq.kind === "equation") {
      const rhsStr = ast.strExpr(
        (valueEq as { rhs: import("../src/pant-ast.js").OpaqueExpr }).rhs,
      );
      // The inner `m.get(k)!` lowered through `readMapThroughWrites`
      // sees the prior staged `.set(k, v)`, so it reads through the
      // override `string-to-int-map[(m, k) |-> v] m k + 1` rather
      // than the bare pre-state rule `string-to-int-map m k + 1`.
      assert.match(
        rhsStr,
        /string-to-int-map\[\(m,\s*k\)\s*\|->\s*v\]\s*m\s*k\s*\+\s*1/u,
        `Map value-rule rhs should read through the staged override: ${rhsStr}`,
      );
      assert.doesNotMatch(
        rhsStr,
        /string-to-int-map\s+m\s+k\s*\+\s*1/u,
        `Map value-rule rhs should not use bare pre-state \`m.get(k)\`: ${rhsStr}`,
      );
    }
  });
});
