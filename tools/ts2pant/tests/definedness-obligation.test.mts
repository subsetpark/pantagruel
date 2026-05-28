import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import {
  createAssumptionEnv,
  discriminantFactsInScope,
  enterFrame,
  pushFact,
  type Fact,
} from "../src/assumption-env.js";
import { renderDefinednessObligation } from "../src/definedness-obligation.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

const circleFact: Fact = {
  kind: "discriminant",
  receiver: "s",
  property: "kind",
  literal: "circle",
};

const squareFact: Fact = {
  kind: "discriminant",
  receiver: "s",
  property: "kind",
  literal: "square",
};

before(async () => {
  await loadAst();
});

describe("definedness", () => {
  it("finds positive in-scope discriminant fact", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    enterFrame(env);
    pushFact(env, circleFact);

    assert.deepEqual(discriminantFactsInScope(env, "s", "kind"), [
      { literal: "circle", negated: false },
    ]);
  });

  it("finds negated in-scope fact and flags it", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, { ...circleFact, literal: "!(circle)" });

    assert.deepEqual(discriminantFactsInScope(env, "s", "kind"), [
      { literal: "circle", negated: true },
    ]);
  });

  it("unknown receiver yields no in-scope facts", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, circleFact);

    assert.deepEqual(discriminantFactsInScope(env, "other", "kind"), []);
  });

  it("renders obligation over the total discriminant rule", () => {
    const ast = getAst();
    const obligation = renderDefinednessObligation({
      receiver: ast.var("s"),
      discRule: "shape--kind",
      requiredLiteral: "circle",
      inScope: [{ literal: "circle", negated: false }],
    });

    assert.equal(
      obligation.text,
      'shape--kind s = "circle" -> shape--kind s = "circle"',
    );
    assert.doesNotMatch(obligation.text, /shape--r/u);
  });

  it("negated fact renders as negation antecedent", () => {
    const ast = getAst();
    const obligation = renderDefinednessObligation({
      receiver: ast.var("s"),
      discRule: "shape--kind",
      requiredLiteral: "square",
      inScope: [{ literal: "circle", negated: true }],
    });

    assert.equal(
      obligation.text,
      '~(shape--kind s = "circle") -> shape--kind s = "square"',
    );
    assert.doesNotMatch(obligation.text, /shape--r/u);
  });

  it("empty in-scope yields bare consequent", () => {
    const ast = getAst();
    const obligation = renderDefinednessObligation({
      receiver: ast.var("s"),
      discRule: "shape--kind",
      requiredLiteral: "circle",
      inScope: [],
    });

    assert.equal(obligation.text, 'shape--kind s = "circle"');
    assert.doesNotMatch(obligation.text, /shape--r/u);
  });

  it("deduplicates positive and negated facts independently", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, circleFact);
    pushFact(env, circleFact);
    enterFrame(env);
    pushFact(env, { ...circleFact, literal: "!(circle)" });
    pushFact(env, squareFact);

    assert.deepEqual(discriminantFactsInScope(env, "s", "kind"), [
      { literal: "circle", negated: false },
      { literal: "circle", negated: true },
      { literal: "square", negated: false },
    ]);
  });
});
