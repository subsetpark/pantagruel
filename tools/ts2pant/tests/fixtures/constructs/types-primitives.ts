// Primitive type mappings: number, string, boolean, null, undefined

interface Account {
  balance: number;
}

interface User {
  name: string;
  active: boolean;
}

interface Task {
  assignee: string | null;
}

/** number → Int */
export function getBalance(a: Account): number {
  return a.balance;
}

/** string → String */
export function getName(u: User): string {
  return u.name;
}

/** boolean → Bool */
export function isActive(u: User): boolean {
  return u.active;
}

/** string | null → String + Nothing */
export function getAssignee(t: Task): string | null {
  return t.assignee;
}
