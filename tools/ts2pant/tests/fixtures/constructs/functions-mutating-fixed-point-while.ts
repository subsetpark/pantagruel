// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
  size: number;
  tickCount: number;
}

export function adjustBalanceTo(
  a: Account,
  target: number,
  step: number,
): void {
  while (a.balance < target) {
    a.balance += step;
  }
}

export function fillUpTo(a: Account, cap: number): void {
  while (a.size < cap) {
    a.size++;
  }
}

export function drainTo(a: Account, floor: number): void {
  while (a.balance > floor) {
    a.balance -= 1;
  }
}

export function spin(a: Account): void {
  while (true) {
    a.tickCount++;
  }
}
