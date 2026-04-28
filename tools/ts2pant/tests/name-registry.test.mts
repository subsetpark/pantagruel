import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import {
  emptyNameRegistry,
  registerName,
} from "../src/name-registry.js";

/**
 * Read the canonical reserved-keyword list from `lib/lexer.ml` so the test
 * fails if the lexer adds or removes a keyword without a matching update
 * to `PANT_RESERVED_KEYWORDS` in `name-registry.ts`. The lexer dispatches
 * keywords through lines like `| "module" -> Parser.MODULE`. The same
 * shape appears for symbolic operators (`"---"`, `"=>"`, `"::"`, ...) so
 * we filter to alphabetic identifiers — the universe `name-registry`
 * cares about, since symbolic strings can't be registered as TS names.
 */
function lexerReservedKeywords(): string[] {
  const lexerPath = resolve(import.meta.dirname, "../../../lib/lexer.ml");
  const src = readFileSync(lexerPath, "utf8");
  const keywords = Array.from(
    src.matchAll(/\|\s*"([^"]+)"\s*->\s*Parser\.[A-Z_]+/g),
    (m) => m[1],
  ).filter((s) => /^[a-z]+$/.test(s));
  return keywords;
}

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

  it("sanitises every reserved keyword (derived from lib/lexer.ml)", () => {
    const reserved = lexerReservedKeywords();
    assert.ok(
      reserved.length > 0,
      "expected lexer keyword list to be non-empty",
    );
    for (const kw of reserved) {
      const { name } = registerName(emptyNameRegistry(), kw);
      assert.equal(name, `${kw}1`, `expected '${kw}' to sanitise to '${kw}1'`);
    }
  });
});
