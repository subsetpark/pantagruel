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
  frames: Set<string>[];
}

export function createAssumptionEnv(): AssumptionEnv {
  return { frames: [] };
}

export function envDepth(env: AssumptionEnv): number {
  return env.frames.length;
}

export function enterFrame(env: AssumptionEnv): void {
  env.frames.push(new Set());
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
  frame.add(factKey(fact));
}

export function queryFact(env: AssumptionEnv, fact: Fact): boolean {
  if (env.frames.length === 0) {
    return false;
  }
  const key = factKey(fact);
  return env.frames.some((frame) => frame.has(key));
}

function factKey(fact: Fact): string {
  if (fact.kind === "discriminant") {
    return `disc:${fact.receiver}|${fact.property}|${fact.literal}`;
  }
  return `pred:${getAst().strExpr(fact.testExpr)}`;
}
