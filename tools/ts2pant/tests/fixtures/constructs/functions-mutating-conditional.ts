// Conditional mutations: path merging into cond expressions
// (Dijkstra's guarded commands, 1975). See CLAUDE.md § Guarded Commands.

interface Account {
  balance: number;
  owner: string;
  active: boolean;
}

interface User {
  active: boolean;
}

/** if/else, both branches write same prop */
export function toggleActive(u: User): void {
  if (u.active) {
    u.active = false;
  } else {
    u.active = true;
  }
}

/** if-only; else-branch uses frame identity */
export function applyFee(a: Account, fee: number): void {
  if (fee > 0) {
    a.balance = a.balance - fee;
  }
}

/** bare if writing a single prop */
export function setFlag(a: Account, g: boolean): void {
  if (g) {
    a.active = true;
  }
}

/** chained if / else-if / else → nested cond */
export function threeWay(a: Account, amount: number): void {
  if (amount > 100) {
    a.balance = a.balance - 100;
  } else if (amount > 0) {
    a.balance = a.balance - 50;
  } else {
    a.balance = 0;
  }
}

/** then writes one prop, else writes a different prop */
export function asymmetric(a: Account, g: boolean, newOwner: string): void {
  if (g) {
    a.balance = 0;
  } else {
    a.owner = newOwner;
  }
}

/** rhs reads the prop being written (conditional self-read) */
export function incrementIfPositive(a: Account, n: number): void {
  if (n > 0) {
    a.balance = a.balance + n;
  }
}

/** compound assignment syntax should desugar to read-modify-write */
export function incrementIfPositiveCompound(a: Account, n: number): void {
  if (n > 0) {
    a.balance += n;
  }
}

/** sequential composition: unconditional write followed by conditional overwrite */
export function initializeAndMaybe(a: Account, g: boolean): void {
  a.balance = 0;
  if (g) {
    a.balance = 1;
  }
}

/** conditional branch reads an earlier unconditional write */
export function accumulateIf(a: Account, g: boolean): void {
  a.balance = 10;
  if (g) {
    a.balance = a.balance + 5;
  }
}

/** conditional branch reads an earlier unconditional write via compound syntax */
export function accumulateIfCompound(a: Account, g: boolean): void {
  a.balance = 10;
  if (g) {
    a.balance += 5;
  }
}

/** nested ifs producing nested cond arms */
export function nestedIfs(a: Account, x: boolean, y: boolean): void {
  if (x) {
    if (y) {
      a.balance = 1;
    } else {
      a.balance = 2;
    }
  } else {
    a.balance = 3;
  }
}
