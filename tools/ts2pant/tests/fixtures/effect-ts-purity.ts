// Fixture: Effect-TS purity detection via callee symbol resolution.
// These functions import from the real `effect` package so the TypeChecker
// can trace callee symbols back to their declaration files.

import { Effect, pipe } from "effect";

// --- Pure Effect-TS constructors (symbol resolves to effect package) ---

export function effectSucceed(x: number) {
  return Effect.succeed(x);
}

export function effectMap(eff: Effect.Effect<number>) {
  return Effect.map(eff, (n) => n + 1);
}

export function effectFlatMap(eff: Effect.Effect<number>) {
  return Effect.flatMap(eff, (n) => Effect.succeed(n + 1));
}

export function effectPipe(eff: Effect.Effect<number>) {
  return pipe(
    eff,
    Effect.map((n) => n + 1),
  );
}

export function effectSync() {
  return Effect.sync(() => 42);
}

export function effectFail(msg: string) {
  return Effect.fail(msg);
}

// --- Impure Effect-TS runners ---

export function effectRunSync(eff: Effect.Effect<number>) {
  return Effect.runSync(eff);
}

export function effectRunPromise(eff: Effect.Effect<number>) {
  return Effect.runPromise(eff);
}

// --- User function returning Effect (NOT from effect package → impure) ---

function myHelper(): Effect.Effect<number> {
  return Effect.succeed(42);
}

export function userEffectReturning() {
  return myHelper();
}
