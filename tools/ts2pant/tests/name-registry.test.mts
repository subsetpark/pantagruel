import { describe, it } from "node:test";
import assert from "node:assert/strict";
import {
  emptyNameRegistry,
  registerName,
} from "../src/name-registry.js";

describe("name-registry", () => {
  it("registerName('max') returns 'max1'", () => {
    const r0 = emptyNameRegistry();
    const { name, registry } = registerName(r0, "max");
    assert.equal(name, "max1");
    assert.ok(registry.used.has("max1"));
    assert.ok(!registry.used.has("max"));
  });

  it("registerName('and') returns 'and1'", () => {
    const r0 = emptyNameRegistry();
    const { name } = registerName(r0, "and");
    assert.equal(name, "and1");
  });

  it("registerName('foo') returns 'foo' (no sanitisation when not a keyword)", () => {
    const r0 = emptyNameRegistry();
    const { name, registry } = registerName(r0, "foo");
    assert.equal(name, "foo");
    assert.ok(registry.used.has("foo"));
  });

  it("registerName sanitises keyword + handles collision interleaving", () => {
    let registry = emptyNameRegistry();

    const a = registerName(registry, "max");
    assert.equal(a.name, "max1");
    registry = a.registry;

    const b = registerName(registry, "max");
    assert.equal(b.name, "max2");
    registry = b.registry;

    const c = registerName(registry, "max1");
    assert.equal(c.name, "max11");
    registry = c.registry;

    const d = registerName(registry, "foo");
    assert.equal(d.name, "foo");
  });

  it("sanitises every reserved keyword", () => {
    const reserved = [
      "module",
      "import",
      "where",
      "true",
      "false",
      "and",
      "or",
      "all",
      "some",
      "each",
      "in",
      "subset",
      "context",
      "initially",
      "closure",
      "cond",
      "over",
      "min",
      "max",
      "check",
    ];
    for (const kw of reserved) {
      const { name } = registerName(emptyNameRegistry(), kw);
      assert.equal(name, `${kw}1`, `expected '${kw}' to sanitise to '${kw}1'`);
    }
  });
});
