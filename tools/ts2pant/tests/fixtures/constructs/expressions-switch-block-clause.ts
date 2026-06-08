// @archlint.module exempt
// @archlint.exempt-reason test-support

export function switchBlockClauseSingleBinding(x: number): number {
  switch (x) {
    case 0: {
      const z = x + 10;
      return z;
    }
    default: {
      const fallback = x + 1;
      return fallback;
    }
  }
}

export function switchBlockClauseMultipleBindings(x: number): number {
  switch (x) {
    case 0: {
      const base = x + 1;
      const doubled = base * 2;
      return doubled;
    }
    case 1: {
      const shifted = x + 10;
      return shifted;
    }
    default: {
      const fallback = x - 1;
      return fallback;
    }
  }
}

export function switchBlockClauseStringLabel(s: string): number {
  switch (s) {
    case "ready": {
      const code = 1;
      return code;
    }
    default: {
      const code = 0;
      return code;
    }
  }
}

export function switchBlockClauseNonLiteralLabel(x: number, k: number): number {
  switch (x) {
    case k: {
      const matched = 1;
      return matched;
    }
    default: {
      const fallback = 0;
      return fallback;
    }
  }
}

export function switchBlockClauseFallThrough(x: number): number {
  switch (x) {
    case 0: {
      const ignored = x + 1;
      void ignored;
    }
    case 1: {
      const matched = 1;
      return matched;
    }
    default: {
      const fallback = 0;
      return fallback;
    }
  }
}
