// @archlint.module exempt
// @archlint.exempt-reason test-support

import type * as Brand from "effect/Brand";
import * as Effect from "effect/Effect";

type Positive = number & Brand.Brand<"Positive">;
type NonNegative = number & Brand.Brand<"NonNegative">;
type IntBrand = number & Brand.Brand<"Int">;
type NonEmptyStrings = ReadonlyArray<string> & Brand.Brand<"NonEmpty">;
type Custom = number & Brand.Brand<"Custom">;

class TooSmallError {}

/** @pant all x: Int | positiveBranded x = x */
export function positiveBranded(x: Positive): number {
  return x;
}

/** @pant all x: Int | nonNegativeBranded x = x */
export function nonNegativeBranded(x: NonNegative): number {
  return x;
}

export function intBranded(x: IntBrand): number {
  var unsupportedBody = 0;
  void unsupportedBody;
  return x;
}

/** @pant all xs: [String] | nonEmptyBranded xs = xs */
export function nonEmptyBranded(xs: NonEmptyStrings): ReadonlyArray<string> {
  return xs;
}

/** @pant all x: Int | brandAndEffect x = x */
export function brandAndEffect(
  x: Positive,
): Effect.Effect<number, TooSmallError, never> {
  var unsupportedBody = 0;
  void unsupportedBody;
  return Effect.gen(function* () {
    if (x < 10) {
      yield* new TooSmallError();
    }
    return x;
  });
}

export function unrecognizedBrand(x: Custom): number {
  return x;
}

export function unbrandedControl(x: number): number {
  return x;
}
