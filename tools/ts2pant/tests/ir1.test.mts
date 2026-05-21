import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  ir1CombTyped,
  ir1Exists,
  ir1Forall,
  ir1LitBool,
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
    it.skip("ir1SsaReturnHandle constructs return-handle shape", () => {
      // PENDING Patch 3.
    });

    it.skip("ir1SsaThrowHandle constructs throw-handle shape with guard", () => {
      // PENDING Patch 3.
    });

    it.skip("ir1SsaReturnValueLocation constructs return-value location kind", () => {
      // PENDING Patch 3.
    });
  });

  describe("IR1SsaLoopBody field defaults", () => {
    it.skip("returnHandles defaults to empty", () => {
      // PENDING Patch 3.
    });

    it.skip("throwHandles defaults to empty", () => {
      // PENDING Patch 3.
    });
  });
});
