import { describe, it, expect } from "vitest";
import { resolve } from "path";
import { createProgram } from "../src/extract.js";
import { IntStrategy } from "../src/translate-types.js";
import { translateBody } from "../src/translate-body.js";

const FIXTURES = resolve(__dirname, "fixtures/constructs");

function translateFixture(fixtureName: string, functionName: string) {
  const fileName = resolve(FIXTURES, fixtureName);
  const program = createProgram(fileName);
  return translateBody({
    program,
    fileName,
    functionName,
    strategy: IntStrategy,
  });
}

describe("expressions-const-bindings", () => {
  const fixture = "expressions-const-bindings.ts";

  it("simpleConst", () => {
    const props = translateFixture(fixture, "simpleConst");
    expect(props).toMatchSnapshot();
  });

  it("chainedConst", () => {
    const props = translateFixture(fixture, "chainedConst");
    expect(props).toMatchSnapshot();
  });

  it("constWithPropAccess", () => {
    const props = translateFixture(fixture, "constWithPropAccess");
    expect(props).toMatchSnapshot();
  });

  it("constInTernary", () => {
    const props = translateFixture(fixture, "constInTernary");
    expect(props).toMatchSnapshot();
  });

  it("effectfulConstRejected", () => {
    const props = translateFixture(fixture, "effectfulConstRejected");
    expect(props).toMatchSnapshot();
  });

  it("letRejected", () => {
    const props = translateFixture(fixture, "letRejected");
    expect(props).toMatchSnapshot();
  });
});

describe("functions-mutating-const", () => {
  const fixture = "functions-mutating-const.ts";

  it("depositWithConst", () => {
    const props = translateFixture(fixture, "depositWithConst");
    expect(props).toMatchSnapshot();
  });

  it("multiConstMutating", () => {
    const props = translateFixture(fixture, "multiConstMutating");
    expect(props).toMatchSnapshot();
  });
});
