import { Data, Effect } from "effect";

class NegError extends Data.TaggedError("NegError")<{}> {}
class TooLargeError extends Data.TaggedError("TooLargeError")<{}> {}
class MissingError extends Data.TaggedError("MissingError")<{}> {}
class ImpureError extends Data.TaggedError("ImpureError")<{}> {}

declare function isBlocked(): boolean;

/** @pant all x: Int | singleError x = x */
export function singleError(
  x: number,
): Effect.Effect<number, NegError, never> {
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    if (x < 0) {
      yield* new NegError();
    }
    return x;
  });
}

/** @pant all x: Int | multiError x = x */
export function multiError(
  x: number,
): Effect.Effect<number, NegError | TooLargeError, never> {
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    if (x < 0) {
      yield* new NegError();
    }
    if (x > 100) {
      yield* new TooLargeError();
    }
    return x;
  });
}

export function partialRecovery(
  x: number,
): Effect.Effect<number, NegError | MissingError, never> {
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    if (x < 0) {
      yield* new NegError();
    }
    return x;
  });
}

export function impureCond(
  x: number,
): Effect.Effect<number, ImpureError, never> {
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    if (isBlocked()) {
      yield* new ImpureError();
    }
    return x;
  });
}

export function unmatchedShape(
  x: number,
): Effect.Effect<number, NegError, never> {
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    yield* new NegError();
    return x;
  });
}

/** @pant all x: Int | combinedWithThrow x = x */
export function combinedWithThrow(
  x: number,
): Effect.Effect<number, NegError, never> {
  if (x <= 1000) {
    // existing if/else-throw guard source
  } else {
    throw new Error("too large");
  }
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    if (x < 0) {
      yield* new NegError();
    }
    return x;
  });
}
