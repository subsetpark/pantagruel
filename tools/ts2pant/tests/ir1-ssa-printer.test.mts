import assert from "node:assert/strict";
import { before, describe, it } from "node:test";

import {
  type IR1SsaProgram,
  type IR1SsaVersion,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1LitNat,
  ir1Member,
  ir1SsaInitialVersion,
  ir1SsaMapMembershipLocation,
  ir1SsaMapMembershipValue,
  ir1SsaMapSetValue,
  ir1SsaMapValueLocation,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRead,
  ir1SsaSetMembershipLocation,
  ir1SsaSetMembershipValue,
  ir1SsaWrite,
  ir1Var,
} from "../src/ir1.js";
import { formatIR1SsaProgram } from "../src/ir1-printer.js";
import { buildScalarSsaProgram } from "../src/ir1-ssa-scalars.js";
import { loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

describe("formatIR1SsaProgram", () => {
  it("renders a phi-function from a then/else mutation merge", (t) => {
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("a"),
      "Account_balance",
    );
    const vThen = {
      kind: "ssa-version",
      id: Symbol("v_then"),
      location,
      origin: "write",
    } as IR1SsaVersion;
    const vElse = {
      kind: "ssa-version",
      id: Symbol("v_else"),
      location,
      origin: "initial",
    } as IR1SsaVersion;
    const vJoin = {
      kind: "ssa-version",
      id: Symbol("v_join"),
      location,
      origin: "join",
    } as IR1SsaVersion;
    const program: IR1SsaProgram = {
      reads: [
        {
          kind: "ssa-read",
          location,
          version: vElse,
          dominated: false,
        },
      ],
      writes: [
        {
          kind: "ssa-write",
          location,
          version: vThen,
          value: ir1SsaPropertyValue(ir1LitNat(1)),
        },
      ],
      joins: [
        {
          kind: "ssa-join",
          location,
          thenVersion: vThen,
          elseVersion: vElse,
          joinVersion: vJoin,
        },
      ],
      loopSummaries: [],
      loopHeaderJoins: [],
      loopBodies: [],
      declaredRules: ["Account_balance"],
      modifiedRules: ["Account_balance"],
      framedRules: [],
    };

    assert.equal(program.joins.length, 1);
    t.assert.snapshot(formatIR1SsaProgram(program));
  });

  it("renders a buildScalarSsaProgram output", (t) => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const stmt = ir1Block([
      ir1Assign(balance, ir1LitNat(1)),
      ir1Assign(balance, ir1Binop("add", balance, ir1LitNat(2))),
    ]);
    const program = buildScalarSsaProgram(stmt);

    assert.equal(program.writes.length, 2);
    t.assert.snapshot(formatIR1SsaProgram(program));
  });

  it("renders map-membership and set-membership locations", (t) => {
    const propertyLocation = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("a"),
      "Account_balance",
    );
    const mapValueLocation = ir1SsaMapValueLocation(
      "Cache_value",
      "Cache_hasKey",
      "Owner",
      "Key",
      ir1Var("cache"),
      ir1Var("key"),
    );
    const mapMembershipLocation = ir1SsaMapMembershipLocation(
      "Cache_value",
      "Cache_hasKey",
      "Owner",
      "Key",
      ir1Var("cache"),
      ir1Var("key"),
    );
    const setMembershipLocation = ir1SsaSetMembershipLocation(
      "Owner_tags",
      "Owner",
      "Tag",
      ir1Var("ownerTags"),
    );
    const setInitial = ir1SsaInitialVersion(setMembershipLocation);
    const writes = [
      ir1SsaWrite(propertyLocation, ir1SsaPropertyValue(ir1LitNat(10))),
      ir1SsaWrite(mapValueLocation, ir1SsaMapSetValue(ir1LitNat(7))),
      ir1SsaWrite(mapMembershipLocation, ir1SsaMapMembershipValue("delete")),
      ir1SsaWrite(
        setMembershipLocation,
        ir1SsaSetMembershipValue("add", ir1Var("tag")),
      ),
    ];
    const program: IR1SsaProgram = {
      reads: [ir1SsaRead(setMembershipLocation, setInitial, false)],
      writes,
      joins: [],
      loopSummaries: [],
      loopHeaderJoins: [],
      loopBodies: [],
      declaredRules: [
        "Account_balance",
        "Cache_value",
        "Cache_hasKey",
        "Owner_tags",
      ],
      modifiedRules: [
        "Account_balance",
        "Cache_value",
        "Cache_hasKey",
        "Owner_tags",
      ],
      framedRules: [],
    };

    assert.equal(program.writes.length, 4);
    t.assert.snapshot(formatIR1SsaProgram(program));
  });
});
