import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";

export interface TypePredicateFactInfo {
  receiver: string;
  negated: boolean;
  tractable: boolean;
}

export type Fact =
  | {
      kind: "discriminant";
      receiver: string;
      property: string;
      literal: string;
      negated: boolean;
    }
  | { kind: "non-null"; receiver: string; negated: boolean }
  | {
      kind: "predicate";
      testExpr: OpaqueExpr;
      typePredicate?: TypePredicateFactInfo;
    };

export interface AssumptionEnv {
  frames: Map<string, Fact>[];
}

export interface InScopeDiscriminantFact {
  literal: string;
  negated: boolean;
}

export interface InScopeNonNullFact {
  receiver: string;
  negated: boolean;
}

export interface InScopeTypePredicateFact {
  testExpr: OpaqueExpr;
  receiver: string;
  negated: boolean;
  tractable: boolean;
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
      const inScope: InScopeDiscriminantFact = {
        literal: fact.literal,
        negated: fact.negated,
      };
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

export function nonNullFactInScope(
  env: AssumptionEnv,
  receiver: string,
): InScopeNonNullFact[] {
  const out: InScopeNonNullFact[] = [];
  const seen = new Set<string>();
  for (const frame of env.frames) {
    for (const fact of frame.values()) {
      if (fact.kind !== "non-null" || fact.receiver !== receiver) {
        continue;
      }
      const inScope: InScopeNonNullFact = {
        receiver: fact.receiver,
        negated: fact.negated,
      };
      const key = `${inScope.negated ? "!" : "="}:${JSON.stringify(
        inScope.receiver,
      )}`;
      if (!seen.has(key)) {
        seen.add(key);
        out.push(inScope);
      }
    }
  }
  return out;
}

export function typePredicateFactsInScope(
  env: AssumptionEnv,
  receiver: string,
): InScopeTypePredicateFact[] {
  const out: InScopeTypePredicateFact[] = [];
  const seen = new Set<string>();
  for (const frame of env.frames) {
    for (const fact of frame.values()) {
      if (
        fact.kind !== "predicate" ||
        fact.typePredicate === undefined ||
        fact.typePredicate.receiver !== receiver
      ) {
        continue;
      }
      const inScope: InScopeTypePredicateFact = {
        testExpr: fact.testExpr,
        receiver: fact.typePredicate.receiver,
        negated: fact.typePredicate.negated,
        tractable: fact.typePredicate.tractable,
      };
      const key = `${inScope.negated ? "!" : "="}:${inScope.tractable ? "t" : "b"}:${getAst().strExpr(inScope.testExpr)}`;
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
      fact.negated,
    ])}`;
  }
  if (fact.kind === "non-null") {
    return `nonnull:${JSON.stringify([fact.receiver, fact.negated])}`;
  }
  return `pred:${JSON.stringify([
    getAst().strExpr(fact.testExpr),
    fact.typePredicate?.receiver,
    fact.typePredicate?.negated,
    fact.typePredicate?.tractable,
  ])}`;
}
