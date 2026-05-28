import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";

export type Fact =
  | {
      kind: "discriminant";
      receiver: string;
      property: string;
      literal: string;
    }
  | { kind: "predicate"; testExpr: OpaqueExpr };

export interface AssumptionEnv {
  frames: Map<string, Fact>[];
}

export interface InScopeDiscriminantFact {
  literal: string;
  negated: boolean;
}

export function createAssumptionEnv(): AssumptionEnv {
  return { frames: [] };
}

export function envDepth(env: AssumptionEnv): number {
  return env.frames.length;
}

export function enterFrame(env: AssumptionEnv): void {
  env.frames.push(new Map());
}

export function exitFrame(env: AssumptionEnv): void {
  if (env.frames.length === 0) {
    throw new Error(
      "AssumptionEnv invariant violation: exitFrame with no frame",
    );
  }
  env.frames.pop();
}

export function pushFact(env: AssumptionEnv, fact: Fact): void {
  const frame = env.frames.at(-1);
  if (!frame) {
    throw new Error(
      "AssumptionEnv invariant violation: pushFact with no frame",
    );
  }
  frame.set(factKey(fact), fact);
}

export function queryFact(env: AssumptionEnv, fact: Fact): boolean {
  if (env.frames.length === 0) {
    return false;
  }
  const key = factKey(fact);
  return env.frames.some((frame) => frame.has(key));
}

export function discriminantFactsInScope(
  env: AssumptionEnv,
  receiver: string,
  property: string,
): InScopeDiscriminantFact[] {
  const out: InScopeDiscriminantFact[] = [];
  const seen = new Set<string>();
  for (const frame of env.frames) {
    for (const fact of frame.values()) {
      if (
        fact.kind !== "discriminant" ||
        fact.receiver !== receiver ||
        fact.property !== property
      ) {
        continue;
      }
      const inScope = decodeDiscriminantLiteral(fact.literal);
      const key = `${inScope.negated ? "!" : "="}:${JSON.stringify(
        inScope.literal,
      )}`;
      if (!seen.has(key)) {
        seen.add(key);
        out.push(inScope);
      }
    }
  }
  return out;
}

function factKey(fact: Fact): string {
  if (fact.kind === "discriminant") {
    return `disc:${JSON.stringify([
      fact.receiver,
      fact.property,
      fact.literal,
    ])}`;
  }
  return `pred:${JSON.stringify(getAst().strExpr(fact.testExpr))}`;
}

function decodeDiscriminantLiteral(literal: string): InScopeDiscriminantFact {
  const negated = /^!\((.*)\)$/u.exec(literal);
  if (negated !== null) {
    return { literal: negated[1]!, negated: true };
  }
  return { literal, negated: false };
}
