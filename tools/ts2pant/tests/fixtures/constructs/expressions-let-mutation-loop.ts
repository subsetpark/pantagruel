// @archlint.module exempt
// @archlint.exempt-reason test-support

export function letForLoopAccumulator(n: number): number {
  let total = 0;
  for (let i = 0; i < n; i++) {
    total += i;
  }
  return total;
}

export function letWhileAccumulator(limit: number): number {
  let total = 0;
  let i = 0;
  while (i < limit) {
    total += i;
    i++;
  }
  return total;
}

export function letWhileCounter(limit: number): number {
  let n = 0;
  while (n < limit) {
    n++;
  }
  return n;
}
