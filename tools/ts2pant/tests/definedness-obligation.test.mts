// @archlint.module test
// @archlint.domain ts2pant.definedness-obligation

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import * as fc from "fast-check";
import {
  createAssumptionEnv,
  discriminantFactsInScope,
  enterFrame,
  pushFact,
  type Fact,
} from "../src/assumption-env.js";
import {
  renderDefinednessObligation,
  renderNullishObligation,
  renderTypePredicateObligation,
} from "../src/definedness-obligation.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

const circleFact: Fact = {
  kind: "discriminant",
  receiver: "s",
  property: "kind",
  literal: "circle",
  negated: false,
};

const squareFact: Fact = {
  kind: "discriminant",
  receiver: "s",
  property: "kind",
  literal: "square",
  negated: false,
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
    pushFact(env, { ...circleFact, negated: true });

    assert.deepEqual(discriminantFactsInScope(env, "s", "kind"), [
      { literal: "circle", negated: true },
    ]);
  });

  it("preserves literals shaped like the old negation sentinel", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, {
      kind: "discriminant",
      receiver: "s",
      property: "kind",
      literal: "!(circle)",
      negated: false,
    });

    assert.deepEqual(discriminantFactsInScope(env, "s", "kind"), [
      { literal: "!(circle)", negated: false },
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
    pushFact(env, { ...circleFact, negated: true });
    pushFact(env, squareFact);

    assert.deepEqual(discriminantFactsInScope(env, "s", "kind"), [
      { literal: "circle", negated: false },
      { literal: "circle", negated: true },
      { literal: "square", negated: false },
    ]);
  });

  it("renders generated discriminant, nullish, and type-predicate obligations", () => {
    fc.assert(
      fc.property(
        fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/),
        fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/),
        fc.boolean(),
        (receiver, literal, negated) => {
          const ast = getAst();
          const definedness = renderDefinednessObligation({
            receiver: ast.var(receiver),
            discRule: "shape--kind",
            requiredLiteral: literal,
            inScope: [{ literal, negated }],
          });
          const nullish = renderNullishObligation({
            receiver: ast.var(receiver),
            inScope: [{ receiver, negated }],
          });
          const predicate = renderTypePredicateObligation({
            inScope: [
              {
                testExpr: ast.var(receiver),
                receiver,
                negated,
                tractable: true,
              },
            ],
          });

          assert.match(definedness.text, /shape--kind/u);
          assert.match(definedness.text, new RegExp(JSON.stringify(literal), "u"));
          assert.match(nullish.text, negated ? /->/u : /true/u);
          assert.match(predicate.text, negated ? /^~/u : /true/u);
        },
      ),
    );
  });
});
