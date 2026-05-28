import assert from "node:assert/strict";
import { describe, it } from "node:test";
import type { OpaqueExpr } from "../src/pant-ast.js";
import {
  createAssumptionEnv,
  enterFrame,
  envDepth,
  exitFrame,
  pushFact,
  queryFact,
  type Fact,
} from "../src/assumption-env.js";

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

const predicateFact: Fact = {
  kind: "predicate",
  testExpr: {} as OpaqueExpr,
};

const separatorFact: Fact = {
  kind: "discriminant",
  receiver: "a|b",
  property: "c",
  literal: "d",
};

const separatorCollisionFact: Fact = {
  kind: "discriminant",
  receiver: "a",
  property: "b|c",
  literal: "d",
};

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
});
