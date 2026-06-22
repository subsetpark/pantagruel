// @archlint.module test
// @archlint.domain ts2pant.mono-survey

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";

import { M6_THRESHOLD, surveyMonomorphization } from "../src/mono-survey.js";

const CASES = resolve(import.meta.dirname, "fixtures/mono-survey/cases.ts");

describe("monomorphization survey classifier", () => {
  const report = surveyMonomorphization([CASES]);
  const byName = new Map(report.functions.map((f) => [f.name, f]));

  it("classifies each verdict branch", () => {
    assert.equal(byName.get("allAgree")?.verdict, "all-agree");
    assert.equal(byName.get("bounded")?.verdict, "disagree-but-bounded");
    assert.equal(byName.get("unbounded")?.verdict, "disagree-unbounded");
    assert.equal(byName.get("recursiveFn")?.verdict, "recursive");
    assert.equal(byName.get("uncalled")?.verdict, "no-visible-call-sites");
    assert.equal(byName.get("oversizedFn")?.verdict, "oversized");
  });

  it("records observed argument types per dynamic parameter", () => {
    assert.deepEqual(byName.get("allAgree")?.observedTypesByParam, {
      x: ["string"],
    });
    const bounded = byName.get("bounded")?.observedTypesByParam.x ?? [];
    assert.deepEqual([...bounded].sort(), ["number", "string"]);
  });

  it("flags exported candidates as external-callers-unknown", () => {
    assert.equal(byName.get("allAgree")?.externalCallersUnknown, true);
  });

  it("`driver` is not a candidate (no dynamic parameter)", () => {
    assert.equal(byName.has("driver"), false);
  });

  it("computes the M6 decision metric over the candidate set", () => {
    // 6 candidates; exactly one (allAgree) is all-agree.
    assert.equal(report.candidateFunctions, 6);
    assert.equal(report.byVerdict["all-agree"], 1);
    assert.equal(report.allAgreeFraction, 1 / 6);
    assert.equal(report.allAgreeFraction < M6_THRESHOLD, true);
  });
});
