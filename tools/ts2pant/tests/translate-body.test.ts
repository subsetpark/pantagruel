import { describe, expect, it } from "vitest";
import { createSourceFileFromSource } from "../src/extract.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";

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

  it("returns unsupported for function with multiple non-guard statements", () => {
    const source = `
      function multi(x: number): number {
        const a = x + 1;
        const b = a * 2;
        return b;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "multi",
      strategy: IntStrategy,
    });

    expect(props).toHaveLength(1);
    expect(props[0]?.kind).toBe("unsupported");
  });

  it("returns empty for bare return with no expression", () => {
    const source = `
      function noop(x: number): void {
        return;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "noop",
      strategy: IntStrategy,
    });

    expect(props).toHaveLength(0);
  });

  it("returns unsupported for single non-translatable statement", () => {
    const source = `
      function loop(x: number): number {
        while (x > 0) { x--; }
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "loop",
      strategy: IntStrategy,
    });

    expect(props).toHaveLength(1);
    expect(props[0]?.kind).toBe("unsupported");
  });
});
