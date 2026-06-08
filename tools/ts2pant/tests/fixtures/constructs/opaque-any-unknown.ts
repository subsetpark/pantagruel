// @archlint.module exempt
// @archlint.exempt-reason test-support

// Dynamic TS top types reject through the UNSUPPORTED_UNKNOWN path.

export function anyParam(value: any): number {
  return value;
}

export function anyReturn(value: number): any {
  return value;
}

export function unknownParam(value: unknown): number {
  return 0;
}

export function unknownReturn(value: number): unknown {
  return value;
}

export function anyArrayReturn(value: number): any[] {
  return [value];
}
