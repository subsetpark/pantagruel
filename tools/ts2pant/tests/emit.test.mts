import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { emitDocument } from "../src/emit.js";
import type { PantDocument } from "../src/types.js";

function emptyDoc(overrides: Partial<PantDocument> = {}): PantDocument {
  return {
    moduleName: "M",
    imports: [],
    declarations: [],
    propositions: [],
    checks: [],
    ...overrides,
  };
}

describe("emit", () => {
  it("document with no imports is byte-stable vs. pre-patch snapshot", () => {
    const doc = emptyDoc();
    // Lines pushed: ["module M.", "", "", "---", "", "true.", ""]
    // joined with "\n" — trailing empty entry yields a trailing newline.
    const expected = "module M.\n\n\n---\n\ntrue.\n";
    assert.equal(emitDocument(doc), expected);
  });

  it("document with imports renders `import X.` after module line", () => {
    const doc = emptyDoc({
      imports: [{ name: "JS_MATH" }, { name: "FOO_AMBIENT" }],
    });
    const out = emitDocument(doc);
    const lines = out.split("\n");
    assert.equal(lines[0], "module M.");
    assert.equal(lines[1], "import JS_MATH.");
    assert.equal(lines[2], "import FOO_AMBIENT.");
    assert.equal(lines[3], "");
  });
});
