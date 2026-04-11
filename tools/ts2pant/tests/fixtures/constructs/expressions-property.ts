// Property access patterns: simple, nested, in conditions

interface User {
  name: string;
}

interface Account {
  balance: number;
  owner: User;
  active: boolean;
}

/** simple property access → rule application */
export function getOwner(a: Account): User {
  return a.owner;
}

/** nested property access → composed rule application */
export function getOwnerName(a: Account): string {
  return a.owner.name;
}

/** property access in ternary condition */
export function effectiveBalance(a: Account): number {
  return a.active ? a.balance : 0;
}
