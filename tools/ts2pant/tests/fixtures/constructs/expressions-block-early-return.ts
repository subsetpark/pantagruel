// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
  limit: number;
}

export function blockEarlyReturnSingleBinding(n: number): number {
  if (n < 0) {
    const z = 0 - n;
    return z;
  }
  return n + 1;
}

export function blockEarlyReturnMultipleBindings(n: number): number {
  if (n < 0) {
    const base = 0 - n;
    const doubled = base * 2;
    return doubled;
  }
  return n;
}

export function blockEarlyReturnBindingReferencesParam(
  n: number,
  offset: number,
): number {
  if (n < offset) {
    const z = offset - n;
    return z;
  }
  return n + offset;
}

export function blockEarlyReturnNestedArms(n: number): number {
  if (n < 0) {
    const z = 0 - n;
    return z;
  }
  if (n === 0) {
    const one = n + 1;
    return one;
  }
  return n;
}

export function blockEarlyReturnMutatingSingleBinding(
  a: Account,
  g: boolean,
  amount: number,
): void {
  if (g) {
    const z = a.balance + amount;
    a.balance = z;
  }
}

export function blockEarlyReturnMutatingMultipleBindings(
  a: Account,
  g: boolean,
  amount: number,
): void {
  if (g) {
    const candidate = a.balance + amount;
    const capped = candidate < a.limit ? candidate : a.limit;
    a.balance = capped;
  }
}

declare function compute(n: number): number;

export function blockEarlyReturnEffectfulConstRejected(n: number): number {
  if (n < 0) {
    const z = compute(n);
    return z;
  }
  return n;
}
