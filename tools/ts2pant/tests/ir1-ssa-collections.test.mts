import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  ir1Assign,
  ir1Block,
  ir1CondStmt,
  ir1LitNat,
  ir1MapDelete,
  ir1MapRead,
  ir1MapSet,
  ir1Member,
  ir1SetAddOrDelete,
  ir1SetRead,
  ir1SsaMapMembershipValue,
  ir1SsaMapSetValue,
  ir1SsaSetClearValue,
  ir1Var,
} from "../src/ir1.js";
import {
  buildCollectionSsaProgram,
  isCollectionSsaL1Body,
} from "../src/ir1-ssa-collections.js";

describe("ir1-ssa-collections", () => {
  // PENDING Patch 2: introduce shared collection SSA state for coordinated
  // Map value/membership and Set membership versions.
  // PENDING Patch 3: implement Map SSA writes, staged reads, and lowering.
  // PENDING Patch 4: implement Set SSA writes, staged reads, clear, and lowering.
  // PENDING Patch 5: route non-looping Map/Set bodies through collection SSA.
  // PENDING Patch 6: keep production parity fixtures pinned while the old
  // SymbolicState-only path is narrowed.

  it("records Map.set as coordinated value and membership SSA writes", () => {
    const value = ir1SsaMapSetValue(ir1LitNat(7));
    const membership = ir1SsaMapMembershipValue("set");
    const stmt = ir1MapSet(
      "Cache_value",
      "Cache_hasKey",
      "Owner",
      "Key",
      ir1Var("cache"),
      ir1Var("key"),
      ir1LitNat(7),
    );

    assert.equal(value.kind, "map-value");
    assert.equal(membership.kind, "map-membership");
    assert.equal(isCollectionSsaL1Body(stmt), true);

    const program = buildCollectionSsaProgram(stmt);

    assert.equal(program.writes.length, 2);
    assert.equal(program.reads.length, 0);
    assert.equal(program.joins.length, 0);
    assert.deepEqual(
      new Set(program.modifiedRules),
      new Set(["Cache_value", "Cache_hasKey"]),
    );

    const [valueWrite, membershipWrite] = program.writes;
    assert.equal(valueWrite!.location.kind, "map-value");
    assert.equal(valueWrite!.location.ruleName, "Cache_value");
    assert.equal(valueWrite!.version.location, valueWrite!.location);
    assert.deepEqual(valueWrite!.value, value);
    assert.equal(membershipWrite!.location.kind, "map-membership");
    assert.equal(membershipWrite!.location.keyPredName, "Cache_hasKey");
    assert.equal(membershipWrite!.version.location, membershipWrite!.location);
    assert.deepEqual(membershipWrite!.value, membership);
  });

  it("resolves Map.get and Map.has through staged SSA writes", () => {
    const stmt = ir1Block([
      ir1MapSet(
        "Cache_value",
        "Cache_hasKey",
        "Owner",
        "Key",
        ir1Var("alias"),
        ir1Var("key"),
        ir1LitNat(7),
      ),
      ir1Assign(
        ir1Member(ir1Var("owner"), "Owner_seen"),
        ir1MapRead(
          "get",
          "Cache_value",
          "Cache_hasKey",
          "Owner",
          "Key",
          ir1Var("cache"),
          ir1Var("key"),
        ),
      ),
      ir1Assign(
        ir1Member(ir1Var("owner"), "Owner_has"),
        ir1MapRead(
          "has",
          "Cache_value",
          "Cache_hasKey",
          "Owner",
          "Key",
          ir1Var("cache"),
          ir1Var("key"),
        ),
      ),
    ]);

    const program = buildCollectionSsaProgram(stmt, {
      canonicalize: (expr) =>
        expr.kind === "var" && expr.name === "alias" ? ir1Var("cache") : expr,
    });

    assert.equal(program.writes.length, 2);
    assert.equal(program.reads.length, 2);
    const [valueWrite, membershipWrite] = program.writes;
    const [valueRead, membershipRead] = program.reads;
    assert.equal(valueRead!.location, valueWrite!.location);
    assert.equal(valueRead!.version, valueWrite!.version);
    assert.equal(valueRead!.dominated, true);
    assert.equal(membershipRead!.location, membershipWrite!.location);
    assert.equal(membershipRead!.version, membershipWrite!.version);
    assert.equal(membershipRead!.dominated, true);
  });

  it("joins Map value and membership versions across branches", () => {
    const stmt = ir1CondStmt(
      [
        [
          ir1Var("gate"),
          ir1MapSet(
            "Cache_value",
            "Cache_hasKey",
            "Owner",
            "Key",
            ir1Var("cache"),
            ir1Var("key"),
            ir1LitNat(7),
          ),
        ],
      ],
      ir1MapDelete(
        "Cache_value",
        "Cache_hasKey",
        "Owner",
        "Key",
        ir1Var("cache"),
        ir1Var("key"),
      ),
    );

    const program = buildCollectionSsaProgram(stmt);

    assert.equal(program.writes.length, 3);
    assert.equal(program.joins.length, 2);
    const [valueWrite, membershipSetWrite, membershipDeleteWrite] =
      program.writes;
    const valueJoin = program.joins.find(
      (j) => j.location.kind === "map-value",
    );
    const membershipJoin = program.joins.find(
      (j) => j.location.kind === "map-membership",
    );
    assert.ok(valueJoin);
    assert.equal(valueJoin.location, valueWrite!.location);
    assert.equal(valueJoin.thenVersion, valueWrite!.version);
    assert.equal(valueJoin.elseVersion.origin, "initial");
    assert.equal(valueJoin.joinVersion.location, valueJoin.location);
    assert.ok(membershipJoin);
    assert.equal(membershipJoin.location, membershipSetWrite!.location);
    assert.equal(membershipJoin.thenVersion, membershipSetWrite!.version);
    assert.equal(membershipJoin.elseVersion, membershipDeleteWrite!.version);
    assert.equal(membershipJoin.joinVersion.location, membershipJoin.location);
  });

  it("resolves Set.has through the current membership version", () => {
    const stmt = ir1Block([
      ir1SetAddOrDelete(
        "add",
        "Owner_tags",
        "Owner",
        "Tag",
        ir1Var("owner"),
        ir1Var("tag"),
      ),
      ir1Assign(
        ir1Member(ir1Var("owner"), "Owner_hasTag"),
        ir1SetRead(
          "Owner_tags",
          "Owner",
          "Tag",
          ir1Var("owner"),
          ir1Var("tag"),
        ),
      ),
    ]);

    const program = buildCollectionSsaProgram(stmt);

    assert.equal(program.writes.length, 1);
    assert.equal(program.reads.length, 1);
    assert.equal(program.writes[0]!.location.kind, "set-membership");
    assert.equal(program.reads[0]!.location, program.writes[0]!.location);
    assert.equal(program.reads[0]!.version, program.writes[0]!.version);
    assert.equal(program.reads[0]!.dominated, true);
  });

  it("rejects loop and expression statement routing shapes", () => {
    assert.equal(
      isCollectionSsaL1Body({
        kind: "expr-stmt",
        expr: ir1Var("x"),
      }),
      false,
    );
    assert.equal(
      isCollectionSsaL1Body({
        kind: "while",
        cond: ir1Var("keepGoing"),
        body: ir1MapDelete(
          "Cache_value",
          "Cache_hasKey",
          "Owner",
          "Key",
          ir1Var("cache"),
          ir1Var("key"),
        ),
      }),
      false,
    );
  });

  it.skip("records Set.add delete and clear as membership SSA writes", () => {
    const clear = ir1SsaSetClearValue();

    assert.equal(clear.kind, "set-membership");
    assert.equal(clear.op, "clear");
    // PENDING Patch 4: build collection SSA for `.add`, `.delete`, and
    // `.clear()` and assert all writes target set-membership locations, with
    // clear represented by `ir1SsaSetClearValue()`.
  });

  it.skip("resolves Set.has through staged membership and clear versions", () => {
    // PENDING Patch 4: lower `add/delete/clear -> has` bodies and assert
    // reads observe later-wins membership versions plus clear fallthrough,
    // matching the existing `readSetThroughWrites` semantics.
  });

  it.skip("preserves production parity for Map and Set mutation fixtures", async () => {
    const mapCases = [
      ["expressions-map-mutation.ts", "put"],
      ["expressions-map-mutation.ts", "remove"],
      ["expressions-map-mutation.ts", "setAndCopy"],
      ["expressions-state-aware-reads.ts", "entrySetThenCheck"],
      ["expressions-state-aware-reads.ts", "bumpInBranch"],
    ] as const;

    const setCases = [
      ["expressions-set-mutation-field.ts", "tagAddThenRemove"],
      ["expressions-set-mutation-field.ts", "tagClearAndAdd"],
      ["expressions-state-aware-reads.ts", "tagThenCheck"],
    ] as const;

    void mapCases;
    void setCases;
    // PENDING Patch 5: route the non-looping production path through
    // collection SSA and keep these existing Map/Set mutation and staged-read
    // fixtures byte-equivalent at emit time.
  });
});
