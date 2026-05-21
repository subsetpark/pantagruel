import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  ir1LitNat,
  ir1SsaLoopBody,
  ir1SsaTerminationMetric,
  ir1Var,
} from "../src/ir1.js";

const pendingLoopBodySetup = ir1SsaLoopBody({
  terminationMetric: ir1SsaTerminationMetric(ir1Var("remaining"), ir1LitNat(0)),
});
void pendingLoopBodySetup;

describe("ir1-ssa-counter-loop", () => {
  it.skip("builds a loop-header join per mutated location", () => {
    // PENDING Patch 4: lowerCounterLoopL1Body opens one ir1SsaOpenLoopHeader per mutated location and back-patches each with the body's terminal write version.
    assert.fail("PENDING Patch 4: loop-header joins per mutated location");
  });

  it.skip("attaches a termination metric to the loop body", () => {
    // PENDING Patch 4: IR1SsaLoopBody.terminationMetric is non-null and references the counter and bound expressions.
    assert.fail("PENDING Patch 4: termination metric");
  });

  it.skip(
    "emits an over-each equation for an accumulator-fold body",
    () => {
      // PENDING Patch 4: propositions contain exactly one equation-kind PropResult per mutated location, with the counter as the over-each binder and the bound as a guard.
      assert.fail("PENDING Patch 4: accumulator-fold over-each equation");
    },
  );

  it.skip(
    "emits a cond equation for a simple counter-only assign body",
    () => {
      // PENDING Patch 4: propositions contain a cond BOUND > 0 => F(BOUND - 1), true => prior equation rather than an over-each.
      assert.fail("PENDING Patch 4: simple-assign cond equation");
    },
  );

  it.skip("rejects non-literal init", () => {
    // PENDING Patch 4: canonical counter loops require a literal-zero let binding in the for initializer.
    assert.fail("PENDING Patch 4: reject non-literal init");
  });

  it.skip("rejects non-canonical step", () => {
    // PENDING Patch 4: canonical counter loops require counter++ / ++counter / counter += 1 / counter = counter + 1.
    assert.fail("PENDING Patch 4: reject non-canonical step");
  });

  it.skip("rejects body with accumulator self-recurrence", () => {
    // PENDING Patch 4: bodies that read their own loop-header join version are rejected for L4 fixed-point handling.
    assert.fail("PENDING Patch 4: reject accumulator self-recurrence");
  });

  it.skip("rejects body with effectful step expression", () => {
    // PENDING Patch 4: counter-loop recognition rejects steps whose expression has non-counter side effects.
    assert.fail("PENDING Patch 4: reject effectful step expression");
  });
});
