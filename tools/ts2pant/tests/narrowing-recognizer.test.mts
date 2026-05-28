import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import type { Fact } from "../src/assumption-env.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  recognizeNarrowingFromSwitchCase,
  recognizeNarrowingPredicate,
} from "../src/narrowing-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

function sourceWithReturn(expr: string): {
  expression: ts.Expression;
  checker: ts.TypeChecker;
} {
  const sf = createSourceFileFromSource(`
    interface Shape {
      kind: string | number | boolean;
      foo: string;
      nested: { kind: string };
    }
    declare function someCall(): boolean;
    function f(s: Shape) {
      return ${expr};
    }
  `);
  const checker = getChecker(sf);
  let expression: ts.Expression | undefined;
  function visit(node: ts.Node): void {
    if (expression) {
      return;
    }
    if (ts.isReturnStatement(node) && node.expression) {
      expression = node.expression;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sf.compilerNode, visit);
  if (!expression) {
    throw new Error("No return expression found");
  }
  return { expression, checker };
}

function recognizeExpr(expr: string): Fact | null {
  const { expression, checker } = sourceWithReturn(expr);
  return recognizeNarrowingPredicate(expression, checker);
}

function assertDiscriminant(
  actual: Fact | null,
  expected: { receiver: string; property: string; literal: string },
): void {
  assert.deepEqual(actual, {
    kind: "discriminant",
    ...expected,
  });
}

function switchParts(): {
  switchTest: ts.Expression;
  caseLabel: ts.Expression;
} {
  const sf = createSourceFileFromSource(`
    interface Shape { kind: string; }
    function f(s: Shape) {
      switch (s.kind) {
        case "circle":
          return 1;
        default:
          return 0;
      }
    }
  `);
  let switchStatement: ts.SwitchStatement | undefined;
  function visit(node: ts.Node): void {
    if (switchStatement) {
      return;
    }
    if (ts.isSwitchStatement(node)) {
      switchStatement = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sf.compilerNode, visit);
  const firstClause = switchStatement?.caseBlock.clauses[0];
  if (!switchStatement || !firstClause || !ts.isCaseClause(firstClause)) {
    throw new Error("No switch case found");
  }
  return {
    switchTest: switchStatement.expression,
    caseLabel: firstClause.expression,
  };
}

describe("narrowing-recognizer", () => {
  it(".kind === literal returns DiscriminantFact", () => {
    assertDiscriminant(recognizeExpr('s.kind === "circle"'), {
      receiver: "s",
      property: "kind",
      literal: "circle",
    });
  });

  it("literal === .kind returns same fact", () => {
    assertDiscriminant(recognizeExpr('"circle" === s.kind'), {
      receiver: "s",
      property: "kind",
      literal: "circle",
    });
  });

  it("numeric and boolean literals are recognised", () => {
    assertDiscriminant(recognizeExpr("s.kind === 42"), {
      receiver: "s",
      property: "kind",
      literal: "42",
    });
    assertDiscriminant(recognizeExpr("s.kind === true"), {
      receiver: "s",
      property: "kind",
      literal: "true",
    });
  });

  it("non-discriminant property still recognised structurally", () => {
    assertDiscriminant(recognizeExpr('s.foo === "bar"'), {
      receiver: "s",
      property: "foo",
      literal: "bar",
    });
  });

  it("nested receiver property access is recognised structurally", () => {
    assertDiscriminant(recognizeExpr('s.nested.kind === "circle"'), {
      receiver: "s.nested",
      property: "kind",
      literal: "circle",
    });
  });

  it("strict not-equal is deferred to arm-specific negation", () => {
    assert.equal(recognizeExpr('s.kind !== "circle"'), null);
  });

  it("boolean predicate returns PredicateFact", () => {
    const fact = recognizeExpr("someCall()");

    assert.equal(fact?.kind, "predicate");
    assert.equal(
      fact?.kind === "predicate" ? getAst().strExpr(fact.testExpr) : null,
      "someCall",
    );
  });

  it("non-boolean expression returns null", () => {
    assert.equal(recognizeExpr("5 + 3"), null);
  });

  it("unsupported boolean expression returns null", () => {
    assert.equal(recognizeExpr('s.kind == "circle"'), null);
  });

  it("switch case label produces DiscriminantFact", () => {
    const { switchTest, caseLabel } = switchParts();

    assertDiscriminant(recognizeNarrowingFromSwitchCase(switchTest, caseLabel), {
      receiver: "s",
      property: "kind",
      literal: "circle",
    });
  });
});
