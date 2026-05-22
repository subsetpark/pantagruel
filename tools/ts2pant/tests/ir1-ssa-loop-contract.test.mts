import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  type IR1SsaBreakHandle,
  type IR1SsaContinueHandle,
  type IR1SsaProgram,
  ir1SsaBreakHandle,
  ir1SsaCloseLoopHeader,
  ir1SsaContinueHandle,
  ir1LitNat,
  ir1SsaInitialVersion,
  ir1SsaLoopBody,
  ir1SsaOpenLoopHeader,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaTerminationMetric,
  ir1SsaWrite,
  ir1Var,
} from "../src/ir1.js";

describe("ir1-ssa-loop-contract", () => {
  it("opens a loop-header join with a placeholder loop-back input", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const preheaderVersion = ir1SsaInitialVersion(location);
    const header = ir1SsaOpenLoopHeader(location, preheaderVersion);

    assert.equal(header.kind, "ssa-loop-header-join");
    assert.equal(header.location, location);
    assert.equal(header.preheaderVersion, preheaderVersion);
    assert.equal(header.loopBackVersion, null);
    assert.equal(header.closed, false);
    assert.equal(header.joinVersion.location, location);
    assert.equal(header.joinVersion.origin, "loop-header");
    assert.notEqual(header.joinVersion, preheaderVersion);

    const otherLocation = ir1SsaPropertyLocation(
      "Account_limit",
      ir1Var("account"),
      "limit",
    );
    assert.throws(
      () => ir1SsaOpenLoopHeader(location, ir1SsaInitialVersion(otherLocation)),
      /location mismatch/u,
    );
  });

  it("closes a loop-header join via back-patch and rejects double-close", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const preheaderVersion = ir1SsaInitialVersion(location);
    const header = ir1SsaOpenLoopHeader(location, preheaderVersion);
    const loopBackVersion = ir1SsaWrite(
      location,
      ir1SsaPropertyValue(ir1LitNat(1)),
    ).version;

    ir1SsaCloseLoopHeader(header, loopBackVersion);

    assert.equal(header.loopBackVersion, loopBackVersion);
    assert.equal(header.closed, true);
    assert.notEqual(header.joinVersion, loopBackVersion);
    assert.throws(
      () => ir1SsaCloseLoopHeader(header, loopBackVersion),
      /loop-header already closed/u,
    );
  });

  it("rejects loop-back versions whose location does not match the header", () => {
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
    const header = ir1SsaOpenLoopHeader(
      headerLocation,
      ir1SsaInitialVersion(headerLocation),
    );
    const mismatchedVersion = ir1SsaInitialVersion(otherLocation);

    assert.notEqual(mismatchedVersion.location, headerLocation);
    assert.throws(
      () => ir1SsaCloseLoopHeader(header, mismatchedVersion),
      /location mismatch/u,
    );
    assert.equal(header.loopBackVersion, null);
    assert.equal(header.closed, false);
  });

  it("rejects a loop body containing an open header join", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const write = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(2)));
    const header = ir1SsaOpenLoopHeader(
      location,
      ir1SsaInitialVersion(location),
    );

    assert.throws(
      () => ir1SsaLoopBody({ headerJoins: [header], writes: [write] }),
      /loop body cannot wrap an open loop-header join/u,
    );

    ir1SsaCloseLoopHeader(header, write.version);
    const body = ir1SsaLoopBody({ headerJoins: [header], writes: [write] });
    assert.equal(body.kind, "ssa-loop-body");
    assert.deepEqual(body.headerJoins, [header]);
    assert.deepEqual(body.writes, [write]);
    assert.deepEqual(body.joins, []);
    assert.deepEqual(body.breakHandles, []);
    assert.deepEqual(body.continueHandles, []);
    assert.equal(body.terminationMetric, null);
  });

  it("constructs a termination metric with optional lower bound", () => {
    const metricExpr = ir1Var("remaining");
    const lowerBound = ir1LitNat(0);
    const bounded = ir1SsaTerminationMetric(metricExpr, lowerBound);
    const defaultBound = ir1SsaTerminationMetric(metricExpr);

    assert.equal(bounded.kind, "ssa-termination-metric");
    assert.equal(bounded.expr, metricExpr);
    assert.equal(bounded.lowerBound, lowerBound);
    assert.equal(defaultBound.expr, metricExpr);
    assert.equal(defaultBound.lowerBound, null);
  });

  it("snapshots break and continue handle versions with location compatibility", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const otherLocation = ir1SsaPropertyLocation(
      "Account_limit",
      ir1Var("account"),
      "limit",
    );
    const snapshotVersion = ir1SsaWrite(
      location,
      ir1SsaPropertyValue(ir1LitNat(3)),
    ).version;
    const breakHandle = ir1SsaBreakHandle(location, snapshotVersion);
    const continueHandle = ir1SsaContinueHandle(location, snapshotVersion);

    assert.equal(breakHandle.kind, "ssa-break-handle");
    assert.equal(breakHandle.location, location);
    assert.equal(breakHandle.version, snapshotVersion);
    assert.equal(continueHandle.kind, "ssa-continue-handle");
    assert.equal(continueHandle.location, location);
    assert.equal(continueHandle.version, snapshotVersion);
    assert.throws(
      () => ir1SsaBreakHandle(otherLocation, snapshotVersion),
      /location mismatch/u,
    );
    assert.throws(
      () => ir1SsaContinueHandle(otherLocation, snapshotVersion),
      /location mismatch/u,
    );

    const mismatchedBreakHandle: IR1SsaBreakHandle = {
      kind: "ssa-break-handle",
      location: otherLocation,
      version: snapshotVersion,
    };
    const mismatchedContinueHandle: IR1SsaContinueHandle = {
      kind: "ssa-continue-handle",
      location: otherLocation,
      version: snapshotVersion,
    };
    assert.throws(
      () => ir1SsaLoopBody({ breakHandles: [mismatchedBreakHandle] }),
      /location mismatch/u,
    );
    assert.throws(
      () => ir1SsaLoopBody({ continueHandles: [mismatchedContinueHandle] }),
      /location mismatch/u,
    );
  });

  it("IR1SsaProgram declares loopHeaderJoins and loopBodies arrays", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const write = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(4)));
    const header = ir1SsaOpenLoopHeader(
      location,
      ir1SsaInitialVersion(location),
    );
    ir1SsaCloseLoopHeader(header, write.version);
    const body = ir1SsaLoopBody({ headerJoins: [header], writes: [write] });
    const program: IR1SsaProgram = {
      reads: [],
      writes: [write],
      joins: [],
      loopHeaderJoins: [header],
      loopBodies: [body],
      declaredRules: ["Account_balance"],
      modifiedRules: ["Account_balance"],
      framedRules: [],
    };

    assert.equal(program.loopHeaderJoins[0], header);
    assert.equal(program.loopBodies[0], body);
    assert.equal(program.loopBodies[0]?.headerJoins[0]?.closed, true);
  });
});
