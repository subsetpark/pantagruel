// @archlint.module exempt
// @archlint.exempt-reason test-support

interface User {
  active: boolean;
  score: number;
  name: string;
}

interface Pair {
  x: number;
  y: number;
}

declare function compute(n: number): number;

export function rd2NestedEarlyReturnBlock(n: number): number {
  if (n < 0) {
    if (n < -10) {
      return 10;
    }
    return 0 - n;
  }
  return n;
}

export function rd2NestedBlockWithConstPrelude(n: number): number {
  if (n < 0) {
    const magnitude = 0 - n;
    if (magnitude > 10) {
      return magnitude;
    }
    return magnitude + 1;
  }
  return n;
}

export function rd2TerminalIfElseBranchBlocks(
  n: number,
  flag: boolean,
): number {
  if (flag) {
    if (n < 0) {
      return 0 - n;
    }
    return n + 1;
  } else {
    const shifted = n + 2;
    if (shifted > 10) {
      return shifted;
    }
    return 10;
  }
}

export function rd2SwitchNestedBlockClause(x: number): number {
  switch (x) {
    case 0: {
      if (x < 1) {
        return x + 10;
      }
      return x;
    }
    default: {
      const fallback = x + 1;
      if (fallback > 5) {
        return fallback;
      }
      return 5;
    }
  }
}

export function rd2CallbackNestedBlock(users: User[]): number[] {
  return users.map((u) => {
    if (u.active) {
      if (u.score > 0) {
        return u.score;
      }
      return 0;
    }
    return -1;
  });
}

export function rd2RecordReturnConditional(n: number, flag: boolean): Pair {
  if (flag) {
    return { x: n, y: n + 1 };
  }
  return { x: 0, y: 1 };
}

export function rd2LocalAccumulatorSequencing(seed: string): string[] {
  const lines: string[] = [];
  lines.push(seed);
  return lines;
}

export function rd2EffectfulBlockConstRejected(n: number): number {
  if (n < 0) {
    const value = compute(n);
    if (value > 10) {
      return value;
    }
    return 10;
  }
  return n;
}

export function rd2SwitchNonLiteralLabel(x: number, k: number): number {
  switch (x) {
    case k: {
      if (x > 0) {
        return 1;
      }
      return 0;
    }
    default:
      return -1;
  }
}

export function rd2SwitchFallThrough(x: number): number {
  switch (x) {
    case 0: {
      const ignored = x + 1;
      void ignored;
    }
    default:
      return x;
  }
}

export function rd2RecordReturnMismatchedFields(
  n: number,
  flag: boolean,
): { x: number } | { x: number; y: number } {
  if (flag) {
    return { x: n, y: n + 1 };
  }
  return { x: n };
}
