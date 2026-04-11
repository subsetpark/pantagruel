import { describe, it, expect } from "vitest";
import { createSourceFileFromSource } from "../src/extract.js";
import { IntStrategy } from "../src/translate-types.js";
import { translateBody } from "../src/translate-body.js";

// Tests for internal translateBody API edge cases not coverable via
// exported fixture functions (see tests/fixtures/constructs/ for
// exhaustive construct coverage).

describe("unsupported patterns", () => {
  it("returns empty for function with no body", () => {
    const source = `
      declare function external(x: number): number;
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "external",
      strategy: IntStrategy,
    });

    expect(props).toHaveLength(0);
  });
});
