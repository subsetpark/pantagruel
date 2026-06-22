// @archlint.module test
// @archlint.domain ts2pant.assumption-env

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import * as fc from "fast-check";
import {
  createAssumptionEnv,
  discriminantFactsInScope,
  enterFrame,
  envDepth,
  exitFrame,
  type Fact,
  nonNullFactInScope,
  pushFact,
  queryFact,
  typePredicateFactsInScope,
} from "../src/assumption-env.js";
import type { OpaqueExpr } from "../src/pant-ast.js";
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

const predicateFact: Fact = {
  kind: "predicate",
  testExpr: {} as OpaqueExpr,
};

const separatorFact: Fact = {
  kind: "discriminant",
  receiver: "a|b",
  property: "c",
  literal: "d",
  negated: false,
};

const separatorCollisionFact: Fact = {
  kind: "discriminant",
  receiver: "a",
  property: "b|c",
  literal: "d",
  negated: false,
};

before(async () => {
  await loadAst();
});

describe("AssumptionEnv", () => {
  it("push makes fact queryable", () => {
    const env = createAssumptionEnv();
    enterFrame(env);

    pushFact(env, circleFact);

    assert.equal(queryFact(env, circleFact), true);
  });

  it("enter-frame increases depth", () => {
    const env = createAssumptionEnv();

    enterFrame(env);

    assert.equal(envDepth(env), 1);
  });

  it("exit-frame decreases depth", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    enterFrame(env);

    exitFrame(env);

    assert.equal(envDepth(env), 1);
  });

  it("empty env queries false", () => {
    const env = createAssumptionEnv();

    assert.equal(queryFact(env, circleFact), false);
    assert.equal(queryFact(env, predicateFact), false);
  });

  it("query finds facts across frames", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, circleFact);
    enterFrame(env);
    pushFact(env, squareFact);

    assert.equal(queryFact(env, circleFact), true);
    assert.equal(queryFact(env, squareFact), true);
  });

  it("popped-frame facts are not queryable", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, circleFact);
    enterFrame(env);
    pushFact(env, squareFact);

    exitFrame(env);

    assert.equal(queryFact(env, circleFact), true);
    assert.equal(queryFact(env, squareFact), false);
  });

  it("discriminant keys do not collide on separator characters", () => {
    const env = createAssumptionEnv();
    enterFrame(env);

    pushFact(env, separatorFact);

    assert.equal(queryFact(env, separatorFact), true);
    assert.equal(queryFact(env, separatorCollisionFact), false);
  });

  it("exitFrame throws with no frames", () => {
    const env = createAssumptionEnv();

    assert.throws(
      () => exitFrame(env),
      /AssumptionEnv invariant violation: exitFrame with no frame/u,
    );
  });

  it("pushFact throws with no frames", () => {
    const env = createAssumptionEnv();

    assert.throws(
      () => pushFact(env, circleFact),
      /AssumptionEnv invariant violation: pushFact with no frame/u,
    );
  });

  it("generated facts respect frame push/query/pop semantics", () => {
    fc.assert(
      fc.property(
        fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/u),
        fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/u),
        fc.boolean(),
        (receiver, literal, negated) => {
          const env = createAssumptionEnv();
          const ast = getAst();
          const discriminant: Fact = {
            kind: "discriminant",
            receiver,
            property: "kind",
            literal,
            negated,
          };
          const nonNull: Fact = { kind: "non-null", receiver, negated };
          const predicate: Fact = {
            kind: "predicate",
            testExpr: ast.var(receiver),
            typePredicate: { receiver, negated, tractable: true },
          };

          enterFrame(env);
          pushFact(env, discriminant);
          pushFact(env, nonNull);
          pushFact(env, predicate);

          assert.equal(envDepth(env), 1);
          assert.equal(queryFact(env, discriminant), true);
          assert.deepEqual(discriminantFactsInScope(env, receiver, "kind"), [
            { literal, negated },
          ]);
          assert.deepEqual(nonNullFactInScope(env, receiver), [
            { receiver, negated },
          ]);
          assert.equal(typePredicateFactsInScope(env, receiver).length, 1);

          exitFrame(env);

          assert.equal(envDepth(env), 0);
          assert.equal(queryFact(env, discriminant), false);
        },
      ),
    );
  });
});
