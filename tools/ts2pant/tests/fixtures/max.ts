/**
 * Return the larger of two numbers.
 * @pant all a: Int, b: Int | larger a b >= a and larger a b >= b
 */
function larger(a: number, b: number): number {
  if (a >= b) {
    return a;
  } else {
    return b;
  }
}
