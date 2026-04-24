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

/** conditional assignment with impure guard → UNSUPPORTED */
declare function check(): boolean;
export function impureGuardAssign(a: Account): void {
  if (check()) {
    a.balance = 1;
  }
}

/** loop assignment → UNSUPPORTED */
export function loopAssign(a: Account): void {
  for (let i = 0; i < 3; i++) {
    a.balance = a.balance + 1;
  }
}

/** parameter-level Set mutation → UNSUPPORTED (Stage B out of scope) */
export function paramSetAdd(s: Set<string>, x: string): void {
  s.add(x);
}
