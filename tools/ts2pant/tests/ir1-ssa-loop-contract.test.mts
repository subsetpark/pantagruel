import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  ir1LitNat,
  ir1SsaInitialVersion,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaWrite,
  ir1Var,
} from "../src/ir1.js";

describe("ir1-ssa-loop-contract", () => {
  it.skip("opens a loop-header join with a placeholder loop-back input", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const preheaderVersion = ir1SsaInitialVersion(location);

    assert.equal(preheaderVersion.location, location);
    // PENDING Patch 2: ir1SsaOpenLoopHeader returns an open header
    // (closed=false, loopBackVersion=null) with a fresh joinVersion whose
    // origin is loop-header.
  });

  it.skip(
    "closes a loop-header join via back-patch and rejects double-close",
    () => {
      const location = ir1SsaPropertyLocation(
        "Account_balance",
        ir1Var("account"),
        "balance",
      );
      const preheaderVersion = ir1SsaInitialVersion(location);
      const loopBackVersion = ir1SsaWrite(
        location,
        ir1SsaPropertyValue(ir1LitNat(1)),
      ).version;

      assert.equal(preheaderVersion.location, location);
      assert.equal(loopBackVersion.location, location);
      // PENDING Patch 2: ir1SsaCloseLoopHeader back-patches the single
      // loop-back input, marks the same header object closed, and rejects
      // double-close attempts.
    },
  );

  it.skip(
    "rejects loop-back versions whose location does not match the header",
    () => {
      const headerLocation = ir1SsaPropertyLocation(
        "Account_balance",
        ir1Var("account"),
        "balance",
      );
      const otherLocation = ir1SsaPropertyLocation(
        "Account_limit",
        ir1Var("account"),
        "limit",
      );
      const mismatchedVersion = ir1SsaInitialVersion(otherLocation);

      assert.notEqual(mismatchedVersion.location, headerLocation);
      // PENDING Patch 2: ir1SsaCloseLoopHeader rejects loop-back versions
      // whose version.location is not compatible with the header location.
    },
  );

  it.skip("rejects a loop body containing an open header join", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const write = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(2)));

    assert.equal(write.version.location, location);
    // PENDING Patch 2: ir1SsaLoopBody accepts only closed header joins and
    // rejects any body.headerJoins entry that still has closed=false.
  });

  it.skip("constructs a termination metric with optional lower bound", () => {
    const metricExpr = ir1Var("remaining");
    const lowerBound = ir1LitNat(0);

    assert.equal(metricExpr.kind, "var");
    assert.equal(lowerBound.kind, "lit");
    // PENDING Patch 2: ir1SsaTerminationMetric stores the metric expression
    // and defaults lowerBound to null when callers omit it.
  });

  it.skip(
    "snapshots break and continue handle versions with location compatibility",
    () => {
      const location = ir1SsaPropertyLocation(
        "Account_balance",
        ir1Var("account"),
        "balance",
      );
      const snapshotVersion = ir1SsaWrite(
        location,
        ir1SsaPropertyValue(ir1LitNat(3)),
      ).version;

      assert.equal(snapshotVersion.location, location);
      // PENDING Patch 2: ir1SsaBreakHandle and ir1SsaContinueHandle snapshot
      // existing versions and assert each version is compatible with the
      // handle location.
    },
  );

  it.skip(
    "IR1SsaProgram declares loopHeaderJoins and loopBodies arrays",
    () => {
      const location = ir1SsaPropertyLocation(
        "Account_balance",
        ir1Var("account"),
        "balance",
      );
      const write = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(4)));

      assert.equal(write.location, location);
      // PENDING Patch 2: IR1SsaProgram includes loopHeaderJoins and
      // loopBodies arrays, and existing initializers default them to [].
    },
  );
});
