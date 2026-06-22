// @archlint.module test
// @archlint.domain ts2pant.ir1

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import * as fc from "fast-check";

import {
  ir1Block,
  ir1CombTyped,
  ir1Exists,
  ir1Forall,
  ir1LitBool,
  ir1LitNat,
  ir1OpaqueOriginId,
  ir1Return,
  ir1SsaBreakHandle,
  ir1SsaCloseLoopHeader,
  ir1SsaContinueHandle,
  ir1SsaInitialVersion,
  ir1SsaJoin,
  ir1SsaLoopBody,
  ir1SsaOpenLoopHeader,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRead,
  ir1SsaReturnHandle,
  ir1SsaReturnValueLocation,
  ir1SsaRuleOfLocation,
  ir1SsaThrowHandle,
  ir1SsaWrite,
  ir1SsaExprEquals,
  ir1Var,
} from "../src/ir1.js";

describe("ir1", () => {
  describe("new L1 binder forms", () => {
    it("ir1CombTyped constructs comb-typed shape", () => {
      assert.deepEqual(
        ir1CombTyped("min", "j", "Int", [ir1Var("g")], ir1Var("j")),
        {
          kind: "comb-typed",
          combiner: "min",
          binder: "j",
          binderType: "Int",
          guards: [ir1Var("g")],
          proj: ir1Var("j"),
        },
      );
    });

    it("ir1Forall constructs forall shape", () => {
      assert.deepEqual(ir1Forall("x", "Nat0", ir1Var("body"), ir1Var("g")), {
        kind: "forall",
        binder: "x",
        binderType: "Nat0",
        guard: ir1Var("g"),
        body: ir1Var("body"),
      });
    });

    it("ir1Exists constructs exists shape", () => {
      assert.deepEqual(ir1Exists("x", "Nat0", ir1Var("body")), {
        kind: "exists",
        binder: "x",
        binderType: "Nat0",
        body: ir1Var("body"),
      });
    });

    it("ir1SsaExprEquals compares comb-typed fields", () => {
      const base = ir1CombTyped("min", "j", "Int", [ir1Var("g")], ir1Var("j"));
      assert.equal(ir1SsaExprEquals(base, base), true);
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1CombTyped("max", "j", "Int", [ir1Var("g")], ir1Var("j")),
        ),
        false,
      );
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1CombTyped("min", "j", "Real", [ir1Var("g")], ir1Var("j")),
        ),
        false,
      );
    });

    it("ir1SsaExprEquals compares forall fields", () => {
      const base = ir1Forall("x", "Int", ir1Var("body"), ir1Var("guard"));
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1Forall("x", "Int", ir1Var("body"), ir1Var("guard")),
        ),
        true,
      );
      assert.equal(ir1SsaExprEquals(base, ir1Forall("x", "Int", ir1Var("body"))), false);
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1Forall("x", "Nat0", ir1Var("body"), ir1Var("guard")),
        ),
        false,
      );
    });

    it("ir1SsaExprEquals compares exists fields", () => {
      const base = ir1Exists("x", "Int", ir1Var("body"), ir1LitBool(true));
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1Exists("x", "Int", ir1Var("body"), ir1LitBool(true)),
        ),
        true,
      );
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1Exists("y", "Int", ir1Var("body"), ir1LitBool(true)),
        ),
        false,
      );
      assert.equal(
        ir1SsaExprEquals(
          base,
          ir1Exists("x", "Int", ir1Var("other"), ir1LitBool(true)),
        ),
        false,
      );
    });
  });

  describe("new early-exit handles", () => {
    it("ir1SsaReturnHandle constructs return-handle shape", () => {
      const location = ir1SsaPropertyLocation("value", ir1Var("obj"), "value");
      const version = ir1SsaWrite(
        location,
        ir1SsaPropertyValue(ir1LitNat(1)),
      ).version;
      const handle = ir1SsaReturnHandle(location, version);

      assert.equal(handle.kind, "ssa-return-handle");
      assert.equal(handle.location, location);
      assert.equal(handle.version, version);
      assert.throws(
        () => ir1SsaReturnHandle(ir1SsaReturnValueLocation("f"), version),
        /location mismatch/u,
      );
    });

    it("ir1SsaThrowHandle constructs throw-handle shape with guard", () => {
      const location = ir1SsaPropertyLocation("value", ir1Var("obj"), "value");
      const version = ir1SsaInitialVersion(location);
      const guard = ir1Var("shouldThrow");
      const handle = ir1SsaThrowHandle(location, version, guard);

      assert.equal(handle.kind, "ssa-throw-handle");
      assert.equal(handle.location, location);
      assert.equal(handle.version, version);
      assert.equal(handle.guard, guard);
      assert.throws(
        () =>
          ir1SsaThrowHandle(ir1SsaReturnValueLocation("f"), version, guard),
        /location mismatch/u,
      );
    });

    it("ir1SsaReturnValueLocation constructs return-value location kind", () => {
      assert.deepEqual(ir1SsaReturnValueLocation("compute"), {
        kind: "return-value",
        ruleName: "compute",
      });
    });
  });

  describe("IR1SsaLoopBody field defaults", () => {
    it("returnHandles defaults to empty", () => {
      assert.deepEqual(ir1SsaLoopBody({}).returnHandles, []);
    });

    it("throwHandles defaults to empty", () => {
      assert.deepEqual(ir1SsaLoopBody({}).throwHandles, []);
    });
  });

  describe("properties", () => {
    it("generated SSA handles preserve location/version invariants", () => {
      fc.assert(
        fc.property(
          fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/),
          fc.integer({ min: 0, max: 1000 }),
          (name, line) => {
            const location = ir1SsaPropertyLocation("value", ir1Var(name), "value");
            const initial = ir1SsaInitialVersion(location);
            const write = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(line)));
            const read = ir1SsaRead(location, initial);
            const join = ir1SsaJoin(location, initial, write.version);
            const header = ir1SsaOpenLoopHeader(location, initial);

            ir1SsaCloseLoopHeader(header, write.version);

            assert.equal(ir1OpaqueOriginId({ file: name, line }), `${name}:${line}`);
            assert.equal(
              ir1OpaqueOriginId({ file: name, line, column: line + 1 }),
              `${name}:${line}:${line + 1}`,
            );
            assert.equal(ir1SsaRuleOfLocation(location), "value");
            assert.equal(read.version, initial);
            assert.equal(header.closed, true);
            assert.equal(ir1SsaBreakHandle(location, write.version).version, write.version);
            assert.equal(
              ir1SsaContinueHandle(location, write.version).version,
              write.version,
            );
            assert.equal(ir1SsaReturnHandle(location, write.version).version, write.version);
            assert.equal(
              ir1SsaThrowHandle(location, write.version, ir1Var(name)).guard.kind,
              "var",
            );
            assert.equal(join.kind, "ssa-join");
            assert.equal(ir1SsaLoopBody({ writes: [write] }).writes[0], write);
          },
        ),
      );
    });

    it("generated binder and block constructors preserve supplied parts", () => {
      fc.assert(
        fc.property(
          fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/),
          fc.constantFrom("Nat0", "Int", "Real"),
          (name, typeName) => {
            const body = ir1Var(name);
            const block = ir1Block([ir1Return(body), ir1Return(null)]);

            assert.equal(ir1Forall(name, typeName, body).binder, name);
            assert.equal(ir1Exists(name, typeName, body).binderType, typeName);
            assert.equal(ir1SsaExprEquals(body, ir1Var(name)), true);
            assert.equal(block.kind, "block");
          },
        ),
      );
    });
  });
});
