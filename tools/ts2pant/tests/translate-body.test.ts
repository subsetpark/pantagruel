import { describe, it, expect } from "vitest";
import { createProgramFromSource } from "../src/extract.js";
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
    const fileName = "test.ts";
    const program = createProgramFromSource(source, fileName);
    const props = translateBody({
      program,
      fileName,
      functionName: "external",
      strategy: IntStrategy,
    });

    expect(props).toHaveLength(0);
  });
});
