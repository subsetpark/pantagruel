interface Account {
  balance: number;
}

export function simpleConst(a: number, b: number): number {
  const x = a + b;
  return x;
}

export function chainedConst(a: number): number {
  const x = a;
  const y = x + 1;
  return y;
}

export function constWithPropAccess(a: Account): number {
  const b = a.balance;
  return b + 1;
}

export function constInTernary(a: number, b: number): number {
  const x = a + b;
  return x > 0 ? x : 0;
}

export function effectfulConstRejected(): number {
  const x = foo();
  return x;
}

declare function foo(): number;

export function letRejected(): number {
  let x = 1;
  return x;
}
