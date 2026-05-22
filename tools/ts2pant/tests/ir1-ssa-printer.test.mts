import assert from "node:assert/strict";
import { before, describe, it } from "node:test";

import {
  type IR1SsaProgram,
  type IR1SsaVersion,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1CondStmt,
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

  // Regression: an if/else mutation merge where both arms read and write
  // the same property. The printer used to label the else-branch's RHS
  // read against the then-branch's write version (a flat
  // `currentVersions` snapshot leaked across the branch boundary). The
  // fix walks `program.reads` in lockstep with `program.writes`, so
  // each branch's RHS read resolves to the pre-cond initial. The
  // observable contract: the else-write's RHS labels v1 (the initial),
  // not v2 (the then-write).
  it("renders else-branch RHS reads against the pre-cond initial", (t) => {
    const balance = ir1Member(ir1Var("a"), "Account_balance");
    const stmt = ir1CondStmt(
      [
        [
          ir1Binop("gt", ir1Var("amount"), ir1LitNat(0)),
          ir1Assign(balance, ir1Binop("add", balance, ir1Var("amount"))),
        ],
      ],
      ir1Assign(balance, ir1Binop("sub", balance, ir1LitNat(1))),
    );
    const program = buildScalarSsaProgram(stmt);
    const output = formatIR1SsaProgram(program);
    assert.equal(program.writes.length, 2);
    assert.equal(program.joins.length, 1);
    // Then-write reads the initial.
    assert.match(output, /v2 = \(v1 \+ amount\)\./u);
    // Else-write also reads the initial — v1, not v2.
    assert.match(output, /v3 = \(v1 - 1\)\./u);
    // Phi joins the two candidate post-states.
    assert.match(output, /v4 = phi v2 v3\./u);
    t.assert.snapshot(output);
  });

  // Regression: a cond-stmt that touches a location whose RHS has no
  // read (e.g., `a.status = 1`) — the falsy arm's contribution to the
  // join is the location's initial version, but no `program.reads`
  // entry references it (the body never read `a.status`). The printer
  // used to label the initial at first reference inside the phi line,
  // producing an undeclared `v_N = ... > initial` reference. The fix
  // walks joins + loop-header joins for additional initial sources.
  it("emits initial lines for join-referenced initials without reads", (t) => {
    const status = ir1Member(ir1Var("a"), "Account_status");
    const stmt = ir1CondStmt(
      [[ir1Binop("gt", ir1Var("amount"), ir1LitNat(0)),
        ir1Assign(status, ir1LitNat(1))]],
      null,
    );
    const program = buildScalarSsaProgram(stmt);
    const output = formatIR1SsaProgram(program);
    // The initial must be emitted before it's referenced in the phi.
    const lines = output.split("\n");
    const initialIdx = lines.findIndex((l) =>
      /^v\d+ = Account_status a\.\s+> initial$/u.test(l),
    );
    const phiIdx = lines.findIndex((l) => /^v\d+ = phi /u.test(l));
    assert.notEqual(initialIdx, -1, "initial line was not emitted");
    assert.notEqual(phiIdx, -1, "phi line was not emitted");
    assert.ok(
      initialIdx < phiIdx,
      `initial line (${initialIdx}) must precede phi line (${phiIdx})`,
    );
    // Every v-label referenced on the RHS of any line must be defined
    // on the LHS of a preceding line.
    const defined = new Set<string>();
    for (const line of lines) {
      const lhsMatch = line.match(/^(v\d+) = /u);
      const rhsMatches = line.match(/(?<![A-Za-z_])v\d+/gu) ?? [];
      if (lhsMatch) {
        const lhs = lhsMatch[1]!;
        for (const ref of rhsMatches) {
          if (ref === lhs) continue;
          assert.ok(
            defined.has(ref),
            `${ref} referenced before definition in line: ${line}`,
          );
        }
        defined.add(lhs);
      }
    }
    t.assert.snapshot(output);
  });
});
