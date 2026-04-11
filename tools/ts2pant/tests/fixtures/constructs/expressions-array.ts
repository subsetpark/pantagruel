// Array operations: .length, .includes, .filter/.map chains

interface User {
  name: string;
  active: boolean;
  score: number;
}

/** .length → # */
export function count(items: string[]): number {
  return items.length;
}

/** .includes(x) → x in */
export function contains(items: string[], x: string): boolean {
  return items.includes(x);
}

/** .filter(p).map(f) → each comprehension with predicate */
export function activeNames(users: User[]): string[] {
  return users.filter((u) => u.active).map((u) => u.name);
}

/** .map(f).map(g) → composed comprehension */
export function nameLengths(users: User[]): number[] {
  return users.map((u) => u.name).map((n) => n.length);
}

/** .map(f).filter(g) → comprehension with composed predicate */
export function highScores(users: User[]): number[] {
  return users.map((u) => u.score).filter((s) => s > 0);
}
