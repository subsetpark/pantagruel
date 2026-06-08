// @archlint.module exempt
// @archlint.exempt-reason test-support

export function letForEachCapturedTotal(xs: number[]): number {
  let total = 0;
  xs.forEach((v) => {
    total += v;
  });
  return total;
}

export function letMapCapturedCount(xs: number[]): number {
  let count = 0;
  xs.map((v) => {
    if (v > 0) {
      count++;
    }
    return v;
  });
  return count;
}
