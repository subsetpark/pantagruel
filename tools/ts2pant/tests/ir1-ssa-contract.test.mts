import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  type IR1SsaProgram,
  ir1LitBool,
  ir1LitNat,
  ir1SsaInitialVersion,
  ir1SsaJoin,
  ir1SsaLoopSummary,
  ir1SsaMapMembershipLocation,
  ir1SsaMapMembershipValue,
  ir1SsaMapSetValue,
  ir1SsaMapValueLocation,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRuleOfLocation,
  ir1SsaSetClearValue,
  ir1SsaSetMembershipLocation,
  ir1SsaWrite,
  ir1Var,
} from "../src/ir1.js";

describe("ir1-ssa-contract", () => {
  it("allocates opaque versions with location metadata", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );

    const initial = ir1SsaInitialVersion(location);
    const write = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(10)));

    assert.equal(initial.kind, "ssa-version");
    assert.equal(typeof initial.id, "symbol");
    assert.equal(typeof write.version.id, "symbol");
    assert.equal(initial.location, location);
    assert.equal(write.location, location);
    assert.equal(write.version.location, location);
    assert.notEqual(initial.id, write.version.id);
    assert.equal(ir1SsaRuleOfLocation(location), "Account_balance");
  });

  it("simplifies degenerate joins before constructing join nodes", () => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("account"),
      "balance",
    );
    const left = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(1)));
    const right = ir1SsaWrite(location, ir1SsaPropertyValue(ir1LitNat(2)));

    const degenerate = ir1SsaJoin(location, left.version, left.version);
    assert.equal(degenerate, left.version);

    const join = ir1SsaJoin(location, left.version, right.version);
    assert.equal(join.kind, "ssa-join");
    if (join.kind !== "ssa-join") {
      assert.fail("expected a non-degenerate join node");
    }
    assert.equal(join.location, location);
    assert.equal(join.thenVersion, left.version);
    assert.equal(join.elseVersion, right.version);
    assert.equal(join.joinVersion.location, location);
    assert.notEqual(join.joinVersion, left.version);
    assert.notEqual(join.joinVersion, right.version);

    const otherLocation = ir1SsaPropertyLocation(
      "Account_limit",
      ir1Var("account"),
      "limit",
    );
    assert.throws(
      () =>
        ir1SsaJoin(location, left.version, ir1SsaInitialVersion(otherLocation)),
      /location mismatch/u,
    );
  });

  it("models Map state as coordinated value and membership locations", () => {
    const receiver = ir1Var("scores");
    const key = ir1Var("player");
    const valueLocation = ir1SsaMapValueLocation(
      "Score_value",
      "Score_hasKey",
      "Scoreboard",
      "Player",
      receiver,
      key,
    );
    const membershipLocation = ir1SsaMapMembershipLocation(
      "Score_value",
      "Score_hasKey",
      "Scoreboard",
      "Player",
      receiver,
      key,
    );

    const valueWrite = ir1SsaWrite(
      valueLocation,
      ir1SsaMapSetValue(ir1LitNat(7)),
    );
    const membershipWrite = ir1SsaWrite(
      membershipLocation,
      ir1SsaMapMembershipValue("set"),
    );
    const program: IR1SsaProgram = {
      reads: [],
      writes: [valueWrite, membershipWrite],
      joins: [],
      loopSummaries: [],
      declaredRules: ["Score_value", "Score_hasKey"],
      modifiedRules: [
        ir1SsaRuleOfLocation(valueLocation),
        ir1SsaRuleOfLocation(membershipLocation),
      ],
      framedRules: [],
    };

    assert.equal(valueLocation.kind, "map-value");
    assert.equal(membershipLocation.kind, "map-membership");
    assert.equal(valueLocation.keyPredName, membershipLocation.keyPredName);
    assert.equal(valueWrite.version.location, valueLocation);
    assert.equal(membershipWrite.version.location, membershipLocation);
    assert.deepEqual(program.modifiedRules, ["Score_value", "Score_hasKey"]);
    assert.throws(
      () => ir1SsaWrite(valueLocation, ir1SsaMapMembershipValue("delete")),
      /write value\/location kind mismatch: location=map-value, value=map-membership/u,
    );
  });

  it("models Set clear inside membership SSA value", () => {
    const location = ir1SsaSetMembershipLocation(
      "Group_members",
      "Group",
      "User",
      ir1Var("group"),
    );
    const write = ir1SsaWrite(location, ir1SsaSetClearValue());

    assert.equal(location.kind, "set-membership");
    assert.equal(write.location, location);
    assert.equal(write.value.kind, "set-membership");
    assert.equal(write.value.op, "clear");
    assert.equal(write.value.elem, null);
  });

  it("exposes distinct loop-summary versions", () => {
    const shapeALocation = ir1SsaPropertyLocation(
      "Item_seen",
      ir1Var("$item"),
      "seen",
    );
    const shapeBLocation = ir1SsaPropertyLocation(
      "Account_total",
      ir1Var("account"),
      "total",
    );
    const ordinaryWrite = ir1SsaWrite(
      shapeALocation,
      ir1SsaPropertyValue(ir1LitBool(true)),
    );
    const shapeASummary = ir1SsaLoopSummary("foreach-shape-a", shapeALocation);
    const shapeBSummary = ir1SsaLoopSummary("foreach-shape-b", shapeBLocation);

    assert.equal(shapeASummary.summaryVersion.location, shapeALocation);
    assert.equal(shapeBSummary.summaryVersion.location, shapeBLocation);
    assert.equal(shapeASummary.summaryVersion.origin, "loop-summary");
    assert.equal(shapeBSummary.summaryVersion.origin, "loop-summary");
    assert.notEqual(shapeASummary.summaryVersion, ordinaryWrite.version);
    assert.notEqual(shapeASummary.summaryVersion.id, ordinaryWrite.version.id);
  });
});
