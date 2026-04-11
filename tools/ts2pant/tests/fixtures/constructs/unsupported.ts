// Unsupported patterns: constructs that produce UNSUPPORTED markers

interface Account {
  balance: number;
  owner: string;
}

interface User {
  name: string;
  active: boolean;
  score: number;
}

interface Item {
  value: number;
}

declare function foo(): string[];
declare function bar(): boolean;

/** function call in return → UNSUPPORTED */
export function callInReturn(): number {
  return foo().length;
}

/** unsupported bubbles through negation */
export function bubbleNegation(): boolean {
  return !bar();
}

/** unsupported bubbles through if condition */
export function bubbleCondition(): number {
  if (bar()) {
    return 1;
  } else {
    return 2;
  }
}

/** block-bodied arrow with locals in map → UNSUPPORTED */
export function arrowWithLocals(users: User[]): string[] {
  return users.filter((u) => u.active).map((u) => { const s = u.score; return u.name; });
}

/** destructuring parameter in filter callback → UNSUPPORTED */
export function destructuredParam(users: User[]): string[] {
  return users.filter(({ active }) => active).map((u) => u.name);
}

/** multi-parameter callback in map → UNSUPPORTED */
export function multiParamCallback(items: Item[]): number[] {
  return items.filter((x) => x.value > 0).map((x, i) => x.value + i);
}

/** conditional assignment → UNSUPPORTED */
export function conditionalAssign(a: Account): void {
  if (true) {
    a.balance = 1;
  }
}

/** loop assignment → UNSUPPORTED */
export function loopAssign(a: Account): void {
  for (let i = 0; i < 3; i++) {
    a.balance = a.balance + 1;
  }
}
